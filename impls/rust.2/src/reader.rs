use std::iter::Peekable;

use anyhow::bail;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    OpenParen,
    CloseParen,
    Symbol(String),
    Number(f64),
    Bool(bool),
    EoF,
}

fn tokenize(src: &str) -> anyhow::Result<Vec<Token>> {
    let mut tokens = vec![];

    let mut chars = src.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '(' => tokens.push(Token::OpenParen),
            ')' => tokens.push(Token::CloseParen),
            '0'..='9' => {
                let num = tokenize_number(c, &mut chars)?;
                tokens.push(Token::Number(num))
            }
            '-' => match chars.peek() {
                Some(n) if n.is_ascii_digit() => {
                    let num = tokenize_number(c, &mut chars)?;
                    tokens.push(Token::Number(num))
                }
                _ => {
                    let sym = tokenize_symbol(c, &mut chars);
                    tokens.push(Token::Symbol(sym))
                }
            },
            // Comma is whitespace
            ',' => continue,
            c if c.is_whitespace() => continue,
            _ => {
                let sym = tokenize_symbol(c, &mut chars);

                match sym.as_str() {
                    "true" => tokens.push(Token::Bool(true)),
                    "false" => tokens.push(Token::Bool(false)),
                    _ => tokens.push(Token::Symbol(sym)),
                }
            }
        }
    }

    Ok(tokens)
}

fn tokenize_number(
    first: char,
    chars: &mut Peekable<impl Iterator<Item = char>>,
) -> anyhow::Result<f64> {
    let mut num = String::from(first);
    let mut found_period = false;

    while let Some(c) = chars.next_if(|c| c.is_ascii_digit() || *c == '.') {
        if c == '.' && found_period {
            bail!("Invalid number");
        }

        num.push(c);

        if c == '.' {
            found_period = true;
        }
    }

    num.parse::<f64>()
        .map_err(|_| anyhow::anyhow!(format!("Expected number, got: {num}")))
}

fn tokenize_symbol(first: char, chars: &mut Peekable<impl Iterator<Item = char>>) -> String {
    let mut sym = String::from(first);

    while let Some(c) = chars.next_if(|c| {
        c.is_alphanumeric() || ['*', '+', '!', '-', '_', '\'', '?', '<', '>', '='].contains(c)
    }) {
        sym.push(c);
    }

    sym
}

struct Reader {
    tokens: Vec<Token>,
    pos: usize,
}

impl Reader {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn next(&mut self) -> Token {
        if let Some(token) = self.tokens.get(self.pos).cloned() {
            self.pos += 1;
            token
        } else {
            Token::EoF
        }
    }

    pub fn peek(&self) -> Token {
        self.tokens.get(self.pos).cloned().unwrap_or(Token::EoF)
    }
}

pub fn read_str(input: &str) -> anyhow::Result<()> {
    let tokens = tokenize(input)?;

    let mut reader = Reader::new(tokens);
    read_form(&mut reader);

    Ok(())
}

fn read_form(reader: &mut Reader) {
    loop {
        let token = reader.peek();
        if token == Token::EoF {
            break;
        }
    }
}
