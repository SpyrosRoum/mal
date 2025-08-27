use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub enum MalType {
    String(String),
    List(Vec<MalType>),
    Number(f64),
    Symbol(String),
    Bool(bool),
    Nil,
}

impl Display for MalType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MalType::List(mal_types) => {
                f.write_str("(")?;
                for form in mal_types.iter().take(mal_types.len() - 1) {
                    write!(f, "{form} ")?;
                }

                // It's possible it's just an empty list, so we check
                if let Some(form) = mal_types.last() {
                    write!(f, "{}", form)?;
                }

                f.write_str(")")
            }
            MalType::String(s) => write!(f, "\"{s}\""),
            MalType::Number(n) => n.fmt(f),
            MalType::Symbol(s) => s.fmt(f),
            MalType::Bool(b) => b.fmt(f),
            MalType::Nil => write!(f, "nil"),
        }
    }
}
