use std::{borrow::Cow, collections::HashMap, process::exit};

use itertools::Itertools;
use rustyline::{DefaultEditor, error::ReadlineError};

use marl::env::Env;
use marl::printer;
use marl::reader;
use marl::types::{MalHashKey, MalMap, MalType};

fn main() -> anyhow::Result<()> {
    let mut line_editor = DefaultEditor::new()?;

    let mut env = Env::default();

    let code = loop {
        let sig = line_editor.readline("user> ");

        match sig {
            Ok(buffer) => {
                let res = mal_rep(&buffer, &mut env);
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
    env: &mut Env,
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

fn mal_eval<'a, 'e>(ast: &'a MalType, env: &'e mut Env) -> anyhow::Result<Cow<'a, MalType>>
where
    'e: 'a,
{
    match ast {
        MalType::Symbol(s) => env
            .get(s)
            .map(Cow::Borrowed)
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
                MalType::Symbol(s) if s == "if" => {
                    if mal_types.len() != 4 {
                        anyhow::bail!("Bad argument count for `if`");
                    }

                    let test = mal_types.get(1).unwrap();
                    let evaluated_test = mal_eval(test, env)?;

                    let expr = if evaluated_test.is_true() {
                        mal_types.get(2).unwrap()
                    } else {
                        mal_types.get(3).unwrap()
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
                    Ok(Cow::Borrowed(val))
                }
                MalType::Symbol(s) if s == "let*" => {
                    let mut new_env = Env::with_outer(env);

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
                        set_in_env(&mut new_env, key, val)?;
                    }

                    let form = mal_types.last().expect("We checked length");
                    let res = mal_eval(form, &mut new_env)?;
                    Ok(Cow::Owned(res.into_owned()))
                }
                MalType::Symbol(_) => {
                    let evaluated_first = mal_eval(first, env)?;

                    if let MalType::Function(func) = *evaluated_first {
                        // TODO: Surely this can be done better
                        let evaluated_args = mal_types
                            .iter()
                            .skip(1)
                            .map(|form| mal_eval(form, env).map(|v| v.into_owned()).map(Cow::Owned))
                            .collect::<anyhow::Result<Vec<_>>>()?;

                        let res = func(&evaluated_args)?;
                        Ok(Cow::Owned(res))
                    } else {
                        anyhow::bail!("Cannot apply `{evaluated_first}`");
                    }
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

fn mal_rep(input: &str, env: &mut Env) -> anyhow::Result<String> {
    let form = mal_read(input)?;
    let res = mal_eval(&form, env)?;
    Ok(mal_print(&res))
}
