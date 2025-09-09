use std::{borrow::Cow, collections::HashMap, fmt::Display, hash::Hash, rc::Rc};

use itertools::Itertools;

use crate::env::Env;

pub const NIL: MalType = MalType::Nil;

pub type MalNativeFunction = fn(&[Cow<'_, MalType>]) -> anyhow::Result<MalType>;

#[derive(Debug, Clone)]
pub struct MalLambda {
    pub outer_env: Rc<Env>,
    pub bindings: Vec<MalType>,
    pub is_variadic: bool,
    pub body: MalType,
}

impl MalLambda {
    pub fn new(
        outer_env: Rc<Env>,
        bindings: Vec<MalType>,
        is_variadic: bool,
        body: MalType,
    ) -> anyhow::Result<Self> {
        if !bindings.iter().all(MalType::is_symbol) {
            anyhow::bail!("Expected symbol binding")
        }

        Ok(Self {
            outer_env,
            bindings,
            is_variadic,
            body,
        })
    }
}

#[derive(Debug, Clone)]
pub enum MalFunction {
    Native(MalNativeFunction),
    Lambda(Box<MalLambda>),
}

/// Only Strings and Keywords can be hashmap keys.
/// This struct ensures that we will never have another key type and
/// implements Hash and Eq with this in mind.
#[derive(Clone, Debug)]
pub struct MalHashKey(MalType);

impl Display for MalHashKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Hash for MalHashKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match &self.0 {
            MalType::String(_) => std::mem::discriminant(&self.0).hash(state),
            MalType::Keyword(_) => std::mem::discriminant(&self.0).hash(state),
            _ => unreachable!(),
        }
    }
}
impl Eq for MalHashKey {}
impl PartialEq for MalHashKey {
    fn eq(&self, other: &Self) -> bool {
        match (&self.0, &other.0) {
            (MalType::String(sl), MalType::String(sr)) => sl == sr,
            (MalType::Keyword(sl), MalType::Keyword(sr)) => sl == sr,
            _ => unreachable!(),
        }
    }
}

impl TryFrom<MalType> for MalHashKey {
    type Error = anyhow::Error;

    fn try_from(value: MalType) -> Result<Self, Self::Error> {
        match value {
            MalType::String(_) => Ok(Self(value)),
            MalType::Keyword(_) => Ok(Self(value)),
            _ => Err(anyhow::anyhow!("Invalid map key: {value}")),
        }
    }
}

#[derive(Clone, Debug)]
pub enum MalMap {
    Unevaluated(Vec<MalType>),
    Evaluated(HashMap<MalHashKey, MalType>),
}

impl TryFrom<Vec<MalType>> for MalMap {
    type Error = anyhow::Error;

    fn try_from(values: Vec<MalType>) -> Result<Self, Self::Error> {
        if values.len() % 2 != 0 {
            anyhow::bail!("Odd count in map");
        }

        Ok(Self::Unevaluated(values))
    }
}

impl MalMap {
    pub fn new_evaluated(map: HashMap<MalHashKey, MalType>) -> Self {
        Self::Evaluated(map)
    }
}

#[derive(Clone, Debug)]
pub enum MalType {
    String(String),
    List(Vec<MalType>),
    Vector(Vec<MalType>),
    Map(MalMap),
    Number(f64),
    Symbol(String),
    Keyword(String),
    Bool(bool),
    Function(MalFunction),
    Nil,
}

impl MalType {
    pub fn is_num(&self) -> bool {
        matches!(self, MalType::Number(_))
    }

    pub fn is_symbol(&self) -> bool {
        matches!(self, MalType::Symbol(_))
    }

    pub fn is_true(&self) -> bool {
        !matches!(self, MalType::Nil | MalType::Bool(false))
    }
}

impl Display for MalType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MalType::List(mal_types) | MalType::Vector(mal_types) => {
                let (open, close) = if matches!(self, MalType::List(_)) {
                    ("(", ")")
                } else {
                    ("[", "]")
                };

                f.write_str(open)?;
                for form in mal_types.iter().take(mal_types.len() - 1) {
                    write!(f, "{form} ")?;
                }

                // It's possible it's just an empty list, so we check
                if let Some(form) = mal_types.last() {
                    write!(f, "{form}")?;
                }

                f.write_str(close)
            }
            MalType::Map(MalMap::Evaluated(map)) => {
                write!(f, "{{")?;

                for (key, val) in map.iter().take(map.len() - 1) {
                    write!(f, "{key} {val} ")?;
                }

                if let Some((key, val)) = map.iter().last() {
                    write!(f, "{key} {val}")?;
                }

                write!(f, "}}")
            }
            MalType::Map(MalMap::Unevaluated(forms)) => {
                write!(f, "{{")?;

                // TODO: We should probably write directly instead of
                // allocating all these strings in the middle.
                let res = forms
                    .iter()
                    .tuples()
                    .map(|(k, v)| format!("{k} {v}"))
                    .join(" ");
                write!(f, "{res}")?;

                write!(f, "}}")
            }
            MalType::String(s) => write!(f, "\"{s}\""),
            MalType::Number(n) => n.fmt(f),
            MalType::Symbol(s) => s.fmt(f),
            MalType::Keyword(s) => s.fmt(f),
            MalType::Bool(b) => b.fmt(f),
            MalType::Function(_) => write!(f, "#<function>"),
            MalType::Nil => write!(f, "nil"),
        }
    }
}
