use std::{borrow::Cow, process::exit};

use rustyline::{error::ReadlineError, DefaultEditor};

use marl::env::Env;
use marl::printer;
use marl::reader;
use marl::types::MalType;

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

fn mal_eval<'a, 'e>(ast: &'a MalType, env: &'e mut Env) -> anyhow::Result<Cow<'a, MalType>>
where
    'e: 'a,
{
    match ast {
        MalType::Symbol(s) => env
            .get(s)
            .map(Cow::Borrowed)
            .ok_or_else(|| anyhow::anyhow!("Symbol not found: {s}")),
        MalType::List(types) if types.is_empty() => Ok(Cow::Borrowed(ast)),
        MalType::List(mal_types) => {
            let first = mal_types.first().expect("We matched non-empty list");

            match first {
                MalType::Symbol(s) if s == "def!" => {
                    if mal_types.len() != 3 {
                        anyhow::bail!("Expected two arguments for `def!`");
                    }

                    let MalType::Symbol(ident) = mal_types.get(1).expect("We checked length")
                    else {
                        anyhow::bail!("Expected identifier");
                    };

                    let form = mal_types.last().expect("We checked length");
                    let val = mal_eval(form, env)?.into_owned();

                    env.set(ident.clone(), val);
                    Ok(Cow::Borrowed(
                        env.get(ident).expect("We just set it in the env"),
                    ))
                }
                MalType::Symbol(s) if s == "let*" => {
                    todo!()
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
