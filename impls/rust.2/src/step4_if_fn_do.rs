use std::{borrow::Cow, collections::HashMap, process::exit, rc::Rc};

use anyhow::Context;
use itertools::Itertools;
use marl::types::MalLambda;
use marl::types::NIL;
use rustyline::{error::ReadlineError, DefaultEditor};

use marl::core::core_ns;
use marl::env::Env;
use marl::printer;
use marl::reader;
use marl::types::{MalFunction, MalHashKey, MalMap, MalType};

fn main() -> anyhow::Result<()> {
    let mut line_editor = DefaultEditor::new()?;

    let env = Rc::new(Env::new());
    for (key, val) in core_ns().iter() {
        let key = MalType::Symbol(key.to_string());
        let val = MalType::Function(val.clone());
        set_in_env(&env, &key, &val)?;
    }

    let code = loop {
        let sig = line_editor.readline("user> ");

        match sig {
            Ok(buffer) => {
                let res = mal_rep(&buffer, &env);
                match res {
                    Ok(r) => println!("{r}"),
                    Err(err) => println!("{err}"),
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                println!("Bye Bye!");
                break 0;
            }
            _ => {
                eprintln!("Something went wrong");
                break 1;
            }
        }
    };

    exit(code);
}

fn mal_read(input: &str) -> anyhow::Result<MalType> {
    reader::read_str(input)
}

/// Assert that the key is a symbol, evaluate the form, and return the
/// key identifier after setting the key, val in the env.
fn set_in_env<'a>(
    env: &Rc<Env>,
    key: &'a MalType,
    form_to_be_evaled: &MalType,
) -> anyhow::Result<&'a str> {
    let MalType::Symbol(ident) = key else {
        anyhow::bail!("Expected identifier");
    };

    let val = mal_eval(form_to_be_evaled, env)?.into_owned();

    env.set(ident.clone(), val);
    Ok(ident)
}

fn eval_lambda<'a, 'b>(
    env: &'a Rc<Env>,
    lambda: &'b Box<MalLambda>,
    args: &'b [MalType],
) -> anyhow::Result<Cow<'b, MalType>> {
    if !lambda.is_variadic && args.len() != lambda.bindings.len() {
        anyhow::bail!("Bad argument count");
    } else if lambda.is_variadic && args.len() < lambda.bindings.len() - 2 {
        // bindings.len() - 2 because we have to account for the `&`
        // and we pass empty list if there are no args
        anyhow::bail!("Bad argument count");
    }

    let func_env = Rc::new(Env::with_outer(Rc::clone(&lambda.outer_env)));

    if !lambda.is_variadic {
        for (key, val) in lambda.bindings.iter().zip(args) {
            let MalType::Symbol(ident) = key else {
                anyhow::bail!("Expected identifier");
            };
            let val = mal_eval(val, env)?.into_owned();
            func_env.set(ident.clone(), val);
        }
    } else {
        let mut key_i = 0;
        let mut val_i = 0;
        while let Some(key) = lambda.bindings.get(key_i) {
            let MalType::Symbol(ident) = key else {
                anyhow::bail!("Expected identifier");
            };

            if ident == "&" {
                let key = lambda.bindings.get(key_i + 1).context("Bad arguments")?;
                let MalType::Symbol(ident) = key else {
                    anyhow::bail!("Expected identifier");
                };

                let evaluated_args = args[val_i..]
                    .iter()
                    .map(|form| mal_eval(form, env))
                    .map(|form| form.map(|f| f.into_owned()))
                    .collect::<anyhow::Result<Vec<_>>>()?;

                func_env.set(ident.clone(), MalType::List(evaluated_args));

                break;
            } else {
                let form = args.get(val_i).expect("We checked arg count");
                let val = mal_eval(form, env)?.into_owned();
                func_env.set(ident.clone(), val);
                val_i += 1;
                key_i += 1;
            }
        }
    }

    mal_eval(&lambda.body, &func_env)
}

fn mal_eval<'a>(ast: &'a MalType, env: &Rc<Env>) -> anyhow::Result<Cow<'a, MalType>> {
    match ast {
        MalType::Symbol(s) => env
            .get(s)
            .map(Cow::Owned)
            .ok_or_else(|| anyhow::anyhow!("Symbol {s} not found")),
        MalType::List(types) if types.is_empty() => Ok(Cow::Borrowed(ast)),
        MalType::List(mal_types) => {
            let first = mal_types.first().expect("We matched non-empty list");

            match first {
                MalType::Symbol(s) if s == "do" => {
                    if mal_types.len() == 1 {
                        anyhow::bail!("No arguments for do");
                    }

                    for form in mal_types.iter().skip(1).take(mal_types.len() - 1) {
                        mal_eval(form, env)?;
                    }

                    if let Some(form) = mal_types.last() {
                        mal_eval(form, env)
                    } else {
                        Ok(Cow::Owned(MalType::Nil))
                    }
                }
                MalType::Symbol(s) if s == "fn*" => {
                    if mal_types.len() != 3 {
                        anyhow::bail!("Bad argument count for `fn*`");
                    }

                    let bindings = match mal_types.get(1).expect("We checked length") {
                        MalType::List(b) => b,
                        MalType::Vector(b) => b,
                        _ => anyhow::bail!("Bad type for fn* arguments"),
                    };

                    let is_variadic = bindings
                        .iter()
                        .any(|i| matches!(i, MalType::Symbol(s) if s == "&"));

                    let body = mal_types.get(2).expect("We checked length");

                    let func = MalLambda::new(
                        Rc::clone(env),
                        bindings.clone(),
                        is_variadic,
                        body.clone(),
                    )?;

                    Ok(Cow::Owned(MalType::Function(MalFunction::Lambda(
                        Box::new(func),
                    ))))
                }
                MalType::Symbol(s) if s == "if" => {
                    if mal_types.len() < 3 || mal_types.len() > 4 {
                        anyhow::bail!("Bad argument count for `if`");
                    }

                    let test = mal_types.get(1).unwrap();
                    let evaluated_test = mal_eval(test, env)?;

                    let expr = if evaluated_test.is_true() {
                        mal_types.get(2).unwrap()
                    } else {
                        mal_types.get(3).unwrap_or(&NIL)
                    };

                    mal_eval(expr, env)
                }
                MalType::Symbol(s) if s == "def!" => {
                    if mal_types.len() != 3 {
                        anyhow::bail!("Expected two arguments for `def!`");
                    }

                    let key = mal_types.get(1).expect("We checked length");
                    let form = mal_types.last().expect("We checked length");
                    let ident = set_in_env(env, key, form)?;

                    // NOTE: We could just return the form here as
                    // Cow::Borrowed but this acts as an assert that
                    // we did actually set it without issues.
                    let val = env.get(ident).expect("We just set it in the env");
                    Ok(Cow::Owned(val))
                }
                MalType::Symbol(s) if s == "let*" => {
                    let new_env = Rc::new(Env::with_outer(Rc::clone(env)));

                    if mal_types.len() != 3 {
                        anyhow::bail!("Expected two arguments for `let*`");
                    }

                    let bindings = match mal_types.get(1).expect("We checked length") {
                        MalType::List(forms) | MalType::Vector(forms) => forms,
                        _ => anyhow::bail!("Expected list or vector for bindings"),
                    };

                    if bindings.len() % 2 != 0 {
                        anyhow::bail!("Even number of arguments expected for bindings");
                    }

                    for (key, val) in bindings.iter().tuples() {
                        set_in_env(&new_env, key, val)?;
                    }

                    let form = mal_types.last().expect("We checked length");
                    let res = mal_eval(form, &new_env)?;
                    Ok(Cow::Owned(res.into_owned()))
                }
                MalType::Symbol(_) => {
                    let evaluated_first = mal_eval(first, env)?;

                    match &*evaluated_first {
                        MalType::Function(MalFunction::Native(func)) => {
                            let evaluated_args = mal_types
                                .iter()
                                .skip(1)
                                .map(|form| mal_eval(form, env))
                                .collect::<anyhow::Result<Vec<_>>>()?;

                            func(&evaluated_args).map(Cow::Owned)
                        }
                        MalType::Function(MalFunction::Lambda(func)) => {
                            eval_lambda(env, func, &mal_types[1..])
                                .map(Cow::into_owned)
                                .map(Cow::Owned)
                        }

                        _ => anyhow::bail!("Cannot apply `{evaluated_first}`"),
                    }
                }
                MalType::List(_) => {
                    let evaluated_first = mal_eval(first, env)?;

                    let mut new_forms = vec![evaluated_first.into_owned()];
                    new_forms.extend_from_slice(&mal_types[1..]);

                    let new_list = MalType::List(new_forms);
                    mal_eval(&new_list, env)
                        .map(Cow::into_owned)
                        .map(Cow::Owned)
                }
                MalType::Function(MalFunction::Lambda(func)) => {
                    eval_lambda(env, func, &mal_types[1..])
                        .map(Cow::into_owned)
                        .map(Cow::Owned)
                }
                _ => anyhow::bail!("Cannot apply `{first}`"),
            }
        }
        MalType::Vector(forms) => {
            let evaluated = forms
                .iter()
                .map(|form| mal_eval(form, env).map(Cow::into_owned))
                .collect::<anyhow::Result<Vec<_>>>()?;

            let res = MalType::Vector(evaluated);
            Ok(Cow::Owned(res))
        }
        MalType::Map(MalMap::Unevaluated(forms)) => {
            let forms = forms
                .iter()
                .map(|form| mal_eval(form, env).map(Cow::into_owned))
                .collect::<anyhow::Result<Vec<_>>>()?;

            let mut map = HashMap::with_capacity(forms.len() / 2);

            for (k, v) in forms.into_iter().tuples() {
                let key = MalHashKey::try_from(k)?;
                map.insert(key, v);
            }

            let mal_map = MalMap::new_evaluated(map);
            Ok(Cow::Owned(MalType::Map(mal_map)))
        }
        _ => Ok(Cow::Borrowed(ast)),
    }
}

fn mal_print(input: &MalType) -> String {
    printer::pr_str(input)
}

fn mal_rep(input: &str, env: &Rc<Env>) -> anyhow::Result<String> {
    let form = mal_read(input)?;
    let res = mal_eval(&form, env)?;
    Ok(mal_print(&res))
}
