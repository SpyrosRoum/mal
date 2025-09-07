use std::{borrow::Cow, cell::RefCell, collections::HashMap, ops::Deref, rc::Rc};

use crate::types::{MalFunction, MalNativeFunction, MalType};

#[derive(Debug)]
pub struct Env {
    data: RefCell<HashMap<String, MalType>>,
    outer: Option<Rc<Self>>,
}

impl Default for Env {
    fn default() -> Self {
        let mut data = HashMap::new();

        data.insert(
            "+".to_string(),
            MalType::Function(MalFunction::Native(mal_add as MalNativeFunction)),
        );
        data.insert(
            "*".to_string(),
            MalType::Function(MalFunction::Native(mal_mul as MalNativeFunction)),
        );
        data.insert(
            "-".to_string(),
            MalType::Function(MalFunction::Native(mal_sub as MalNativeFunction)),
        );
        data.insert(
            "/".to_string(),
            MalType::Function(MalFunction::Native(mal_div as MalNativeFunction)),
        );

        let data = RefCell::new(data);
        Self { data, outer: None }
    }
}

impl Env {
    pub fn with_outer(outer: Rc<Self>) -> Self {
        Self {
            data: RefCell::new(HashMap::new()),
            outer: Some(outer),
        }
    }

    pub fn set(&self, key: String, val: MalType) {
        self.data.borrow_mut().insert(key, val);
    }

    pub fn get(&self, key: &str) -> Option<MalType> {
        self.data
            .borrow()
            .get(key)
            .cloned()
            .or_else(|| self.outer.as_deref().and_then(|env| env.get(key)))
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
