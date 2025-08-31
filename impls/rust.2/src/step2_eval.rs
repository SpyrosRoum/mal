use std::{borrow::Cow, collections::HashMap, ops::Deref, process::exit};

use itertools::Itertools;
use marl::types::MalHashKey;
use rustyline::{error::ReadlineError, DefaultEditor};

use marl::printer;
use marl::reader;
use marl::types::{MalFunction, MalMap, MalType};

fn main() -> anyhow::Result<()> {
    let mut line_editor = DefaultEditor::new()?;

    let env = Env::default();

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

fn mal_eval<'a, 'e>(ast: &'a MalType, env: &'e Env) -> anyhow::Result<Cow<'a, MalType>>
where
    'e: 'a,
{
    match ast {
        MalType::Symbol(s) => {
            if let Some(form) = env.get(s) {
                Ok(Cow::Borrowed(form))
            } else {
                anyhow::bail!("Symbol not found: {s}")
            }
        }
        MalType::List(types) if types.is_empty() => Ok(Cow::Borrowed(ast)),
        MalType::List(mal_types) => {
            let evaluated = mal_types
                .iter()
                .map(|form| mal_eval(form, env))
                .collect::<anyhow::Result<Vec<_>>>()?;

            let form = evaluated.first().expect("We matched non-empty list");

            if let MalType::Function(func) = &**form {
                Ok(Cow::Owned(func(&evaluated[1..])?))
            } else {
                anyhow::bail!("Expected function")
            }
        }
        MalType::Vector(forms) => {
            let evaluated = forms
                .iter()
                .map(|form| mal_eval(form, env))
                .map(|form| form.map(Cow::into_owned))
                .collect::<anyhow::Result<Vec<_>>>()?;

            let res = MalType::Vector(evaluated);
            Ok(Cow::Owned(res))
        }
        MalType::Map(MalMap::Unevaluated(forms)) => {
            let forms = forms
                .iter()
                .map(|form| mal_eval(form, env))
                .collect::<anyhow::Result<Vec<_>>>()?;

            let mut map = HashMap::with_capacity(forms.len() / 2);

            for (k, v) in forms.into_iter().tuples() {
                let key = MalHashKey::try_from(k.into_owned())?;
                map.insert(key, v.into_owned());
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

fn mal_rep(input: &str, env: &Env) -> anyhow::Result<String> {
    let form = mal_read(input)?;
    let res = mal_eval(&form, env)?;
    Ok(mal_print(&res))
}

fn mal_add(args: &[Cow<'_, MalType>]) -> anyhow::Result<MalType> {
    if !args.iter().map(Deref::deref).all(MalType::is_num) {
        anyhow::bail!("Expected number for addition");
    }

    let mut acc = 0.;
    for arg in args {
        let MalType::Number(n) = **arg else {
            unreachable!()
        };
        acc += n;
    }

    Ok(MalType::Number(acc))
}

fn mal_sub(args: &[Cow<'_, MalType>]) -> anyhow::Result<MalType> {
    if args.is_empty() {
        anyhow::bail!("Wrong number of args passed to `-`: 0");
    }

    if !args.iter().map(Deref::deref).all(MalType::is_num) {
        anyhow::bail!("Expected number for addition");
    }

    let MalType::Number(ref acc) = **args.first().unwrap() else {
        unreachable!()
    };

    let mut acc = *acc;
    for arg in &args[1..] {
        let MalType::Number(n) = **arg else {
            unreachable!()
        };
        acc -= n;
    }

    Ok(MalType::Number(acc))
}

fn mal_div(args: &[Cow<'_, MalType>]) -> anyhow::Result<MalType> {
    if args.is_empty() {
        anyhow::bail!("Wrong number of args passed to `/`: 0");
    }

    if !args.iter().map(Deref::deref).all(MalType::is_num) {
        anyhow::bail!("Expected number for addition");
    }

    let MalType::Number(ref acc) = **args.first().unwrap() else {
        unreachable!()
    };

    let mut acc = *acc;
    for arg in &args[1..] {
        let MalType::Number(n) = **arg else {
            unreachable!()
        };
        acc /= n;
    }

    Ok(MalType::Number(acc))
}

fn mal_mul(args: &[Cow<'_, MalType>]) -> anyhow::Result<MalType> {
    if !args.iter().map(Deref::deref).all(MalType::is_num) {
        anyhow::bail!("Expected number for addition");
    }

    let mut acc = 1.;
    for arg in args {
        let MalType::Number(n) = **arg else {
            unreachable!()
        };
        acc *= n;
    }

    Ok(MalType::Number(acc))
}

#[derive(Debug)]
struct Env {
    data: HashMap<String, MalType>,
}

impl Default for Env {
    fn default() -> Self {
        let mut data = HashMap::new();

        data.insert("+".to_string(), MalType::Function(mal_add as MalFunction));
        data.insert("*".to_string(), MalType::Function(mal_mul as MalFunction));
        data.insert("-".to_string(), MalType::Function(mal_sub as MalFunction));
        data.insert("/".to_string(), MalType::Function(mal_div as MalFunction));

        Self { data }
    }
}

impl Env {
    fn get(&self, key: &str) -> Option<&MalType> {
        self.data.get(key)
    }
}
