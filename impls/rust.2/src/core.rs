use std::{borrow::Cow, cell::LazyCell, collections::HashMap, ops::Deref};

use crate::{
    printer::pr_str,
    types::{MalFunction, MalMap, MalType},
};

pub const fn core_ns() -> LazyCell<HashMap<&'static str, MalFunction>> {
    LazyCell::new(|| {
        HashMap::from([
            ("+", MalFunction::Native(mal_add)),
            ("-", MalFunction::Native(mal_sub)),
            ("*", MalFunction::Native(mal_mul)),
            ("/", MalFunction::Native(mal_div)),
            ("prn", MalFunction::Native(print)),
            ("list?", MalFunction::Native(is_list)),
            ("list", MalFunction::Native(list)),
            ("empty?", MalFunction::Native(is_empty)),
            ("count", MalFunction::Native(count)),
            ("<", MalFunction::Native(lt)),
            ("<=", MalFunction::Native(le)),
            ("=", MalFunction::Native(eq)),
            (">=", MalFunction::Native(ge)),
            (">", MalFunction::Native(gt)),
        ])
    })
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

fn print(args: &[Cow<'_, MalType>]) -> anyhow::Result<MalType> {
    let mut buffer = Vec::new();

    for arg in args {
        let temp = pr_str(arg);
        buffer.push(temp);
    }

    println!("{}", buffer.join(" "));
    Ok(MalType::Nil)
}

fn is_list(args: &[Cow<'_, MalType>]) -> anyhow::Result<MalType> {
    if args.len() != 1 {
        anyhow::bail!("Bad argumens for list?");
    }

    let is_list = args
        .iter()
        .all(|arg| matches!(arg.deref(), MalType::List(_)));
    Ok(MalType::Bool(is_list))
}

fn list(args: &[Cow<'_, MalType>]) -> anyhow::Result<MalType> {
    let items = args.iter().map(|i| i.deref().to_owned()).collect();
    Ok(MalType::List(items))
}

fn is_empty(args: &[Cow<'_, MalType>]) -> anyhow::Result<MalType> {
    if args.len() != 1 {
        anyhow::bail!("Bad argumens for empty?");
    }

    let arg = args.first().unwrap();
    let empty = match arg.deref() {
        MalType::List(items) => items.is_empty(),
        MalType::Vector(items) => items.is_empty(),
        _ => anyhow::bail!("Expected list or vector"),
    };

    Ok(MalType::Bool(empty))
}

fn count(args: &[Cow<'_, MalType>]) -> anyhow::Result<MalType> {
    if args.len() != 1 {
        anyhow::bail!("Bad argumens for count");
    }

    let arg = args.first().unwrap();
    let count = match arg.deref() {
        MalType::List(items) => items.len(),
        MalType::Vector(items) => items.len(),
        MalType::Nil => 0,
        _ => anyhow::bail!("Expected list or vector"),
    };

    Ok(MalType::Number(count as f64))
}

fn equal_list_like(left: &[MalType], right: &[MalType]) -> bool {
    if left.len() != right.len() {
        return false;
    }

    left.iter().zip(right.iter()).all(|(left, right)| {
        let args = [Cow::Borrowed(left), Cow::Borrowed(right)];
        let MalType::Bool(res) = eq(&args).expect("Cannot fail") else {
            unreachable!()
        };
        res
    })
}

fn eq(args: &[Cow<'_, MalType>]) -> anyhow::Result<MalType> {
    if args.len() != 2 {
        anyhow::bail!("Bad argumens for =");
    }

    let left = args.first().unwrap();
    let right = args.last().unwrap();

    let are_equal = match (left.deref(), right.deref()) {
        (MalType::String(sl), MalType::String(sr)) => sl == sr,
        (MalType::List(forms_left), MalType::List(forms_right)) => {
            equal_list_like(forms_left, forms_right)
        }
        (MalType::Vector(forms_left), MalType::Vector(forms_right)) => {
            equal_list_like(forms_left, forms_right)
        }
        (MalType::Map(map_left), MalType::Map(map_right)) => {
            let MalMap::Evaluated(map_left) = map_left else {
                unreachable!()
            };
            let MalMap::Evaluated(map_right) = map_right else {
                unreachable!()
            };

            if map_left.len() == map_right.len() {
                for (key, left_val) in map_left.iter() {
                    let Some(right_val) = map_right.get(key) else {
                        return Ok(MalType::Bool(false));
                    };

                    let args = [Cow::Borrowed(left_val), Cow::Borrowed(right_val)];
                    let MalType::Bool(eq_vals) = eq(&args).expect("Cannot fail.") else {
                        unreachable!()
                    };

                    if !eq_vals {
                        return Ok(MalType::Bool(false));
                    }
                }

                true
            } else {
                false
            }
        }
        (MalType::Number(nl), MalType::Number(nr)) => nl == nr,
        (MalType::Symbol(sl), MalType::Symbol(sr)) => sl == sr,
        (MalType::Keyword(kl), MalType::Keyword(kr)) => kl == kr,
        (MalType::Bool(bl), MalType::Bool(br)) => bl == br,
        (MalType::Nil, MalType::Nil) => true,
        _ => false,
    };

    Ok(MalType::Bool(are_equal))
}

fn lt(args: &[Cow<'_, MalType>]) -> anyhow::Result<MalType> {
    if args.len() != 2 {
        anyhow::bail!("Bad argumens for <");
    }

    let MalType::Number(left) = args.first().expect("We checked args").deref() else {
        anyhow::bail!("Bad argumens for <");
    };
    let MalType::Number(right) = args.last().expect("We checked args").deref() else {
        anyhow::bail!("Bad argumens for <");
    };

    Ok(MalType::Bool(left < right))
}

fn le(args: &[Cow<'_, MalType>]) -> anyhow::Result<MalType> {
    if args.len() != 2 {
        anyhow::bail!("Bad argumens for <=");
    }

    let MalType::Number(left) = args.first().expect("We checked args").deref() else {
        anyhow::bail!("Bad argumens for <=");
    };
    let MalType::Number(right) = args.last().expect("We checked args").deref() else {
        anyhow::bail!("Bad argumens for <=");
    };

    Ok(MalType::Bool(left <= right))
}

fn ge(args: &[Cow<'_, MalType>]) -> anyhow::Result<MalType> {
    if args.len() != 2 {
        anyhow::bail!("Bad argumens for >=");
    }

    let MalType::Number(left) = args.first().expect("We checked args").deref() else {
        anyhow::bail!("Bad argumens for >=");
    };
    let MalType::Number(right) = args.last().expect("We checked args").deref() else {
        anyhow::bail!("Bad argumens for >=");
    };

    Ok(MalType::Bool(left >= right))
}

fn gt(args: &[Cow<'_, MalType>]) -> anyhow::Result<MalType> {
    if args.len() != 2 {
        anyhow::bail!("Bad argumens for >");
    }

    let MalType::Number(left) = args.first().expect("We checked args").deref() else {
        anyhow::bail!("Bad argumens for >");
    };
    let MalType::Number(right) = args.last().expect("We checked args").deref() else {
        anyhow::bail!("Bad argumens for >");
    };

    Ok(MalType::Bool(left > right))
}
