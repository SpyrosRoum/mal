use std::{borrow::Cow, collections::HashMap, ops::Deref};

use crate::types::{MalFunction, MalType};

#[derive(Debug)]
pub struct Env<'a> {
    data: HashMap<String, MalType>,
    outer: Option<&'a Self>,
}

impl<'a> Default for Env<'a> {
    fn default() -> Self {
        let mut data = HashMap::new();

        data.insert("+".to_string(), MalType::Function(mal_add as MalFunction));
        data.insert("*".to_string(), MalType::Function(mal_mul as MalFunction));
        data.insert("-".to_string(), MalType::Function(mal_sub as MalFunction));
        data.insert("/".to_string(), MalType::Function(mal_div as MalFunction));

        Self { data, outer: None }
    }
}

impl<'a> Env<'a> {
    pub fn with_outer(outer: &'a Self) -> Self {
        Self {
            data: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn set(&mut self, key: String, val: MalType) {
        self.data.insert(key, val);
    }

    pub fn get(&self, key: &str) -> Option<&MalType> {
        self.data
            .get(key)
            .or_else(|| self.outer.and_then(|env| env.get(key)))
    }
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
