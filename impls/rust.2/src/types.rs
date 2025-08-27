#[derive(Clone, Debug, PartialEq)]
pub enum MalType {
    List(Vec<MalType>),
    Number(f64),
    Symbol(String),
    Bool(bool),
}
