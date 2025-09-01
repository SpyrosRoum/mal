use std::iter::Peekable;

use anyhow::bail;

use crate::types::{MalMap, MalType};

const ESCAPE_CHARS: [char; 2] = ['"', '\\'];

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenCurly,
    CloseCurly,
    Keyword(String),
    Symbol(String),
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
    EOF,
}

fn tokenize(src: &str) -> anyhow::Result<Vec<Token>> {
    let mut tokens = vec![];

    let mut chars = src.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '(' => tokens.push(Token::OpenParen),
            ')' => tokens.push(Token::CloseParen),
            '[' => tokens.push(Token::OpenBracket),
            ']' => tokens.push(Token::CloseBracket),
            '{' => tokens.push(Token::OpenCurly),
            '}' => tokens.push(Token::CloseCurly),
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
            // Skip until EoL, skipping comment
            ';' => tokenizer_skip_line(&mut chars),
            c if c.is_whitespace() => continue,
            '"' => {
                let string = tokenize_string(&mut chars)?;
                tokens.push(Token::String(string))
            }
            _ => {
                let sym = tokenize_symbol(c, &mut chars);

                match (c, sym.as_str()) {
                    (':', _) => tokens.push(Token::Keyword(sym)),
                    (_, "true") => tokens.push(Token::Bool(true)),
                    (_, "false") => tokens.push(Token::Bool(false)),
                    (_, "nil") => tokens.push(Token::Nil),
                    _ => tokens.push(Token::Symbol(sym)),
                }
            }
        }
    }

    Ok(tokens)
}

fn tokenizer_skip_line(chars: &mut Peekable<impl Iterator<Item = char>>) {
    for c in chars {
        if c == '\n' {
            return;
        }
    }
}

fn tokenize_string(chars: &mut Peekable<impl Iterator<Item = char>>) -> anyhow::Result<String> {
    let mut buffer = String::new();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.peek() {
                Some(c) if ESCAPE_CHARS.contains(c) => {
                    buffer.push('\\');
                    buffer.push(*c);
                    // Consume the char we were peeking
                    chars.next();
                    continue;
                }
                Some(c) => bail!("Invalid escape character: {c}"),
                None => bail!("Unclosed string literal"),
            }
        }
        if c == '"' {
            return Ok(buffer);
        }

        buffer.push(c);
    }

    Err(anyhow::anyhow!("unbalanced \""))
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
        c.is_alphanumeric() || ['*', '+', '!', '-', '_', '\'', '?', '<', '>', '=', ':'].contains(c)
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
            Token::EOF
        }
    }

    pub fn peek(&self) -> Token {
        self.tokens.get(self.pos).cloned().unwrap_or(Token::EOF)
    }
}

pub fn read_str(input: &str) -> anyhow::Result<MalType> {
    let tokens = tokenize(input)?;

    let mut reader = Reader::new(tokens);
    read_form(&mut reader)
}

fn read_atom(reader: &mut Reader) -> anyhow::Result<MalType> {
    let token = reader.peek();
    let atom = match token {
        Token::Symbol(s) => MalType::Symbol(s),
        Token::Keyword(s) => MalType::Keyword(s),
        Token::Number(n) => MalType::Number(n),
        Token::Bool(b) => MalType::Bool(b),
        Token::String(s) => MalType::String(s),
        Token::Nil => MalType::Nil,
        Token::OpenParen
        | Token::CloseParen
        | Token::EOF
        | Token::OpenBracket
        | Token::CloseBracket
        | Token::OpenCurly
        | Token::CloseCurly => bail!("Expected atom, got {token:?}"),
    };

    // We didn't error so we can consume the token
    reader.next();
    Ok(atom)
}

fn read_list_like(reader: &mut Reader, closing_token: Token) -> anyhow::Result<MalType> {
    let mut items = vec![];

    loop {
        let token = reader.peek();
        match token {
            Token::OpenParen => {
                // Consume open paren
                reader.next();
                let inner = read_list_like(reader, Token::CloseParen)?;
                items.push(inner);
            }
            Token::OpenBracket => {
                // Consume open bracket
                reader.next();
                let inner = read_list_like(reader, Token::CloseBracket)?;
                items.push(inner);
            }
            Token::OpenCurly => {
                reader.next();
                let inner = read_list_like(reader, Token::CloseCurly)?;
                items.push(inner);
            }
            t if t == closing_token => {
                // Consume closing token
                reader.next();

                let form = match closing_token {
                    Token::CloseParen => MalType::List(items),
                    Token::CloseBracket => MalType::Vector(items),
                    Token::CloseCurly => {
                        let map = MalMap::try_from(items)?;
                        MalType::Map(map)
                    }
                    _ => unreachable!(),
                };
                return Ok(form);
            }
            _ => {
                let atom = read_atom(reader)?;
                items.push(atom);
            }
        }
    }
}

fn read_form(reader: &mut Reader) -> anyhow::Result<MalType> {
    let token = reader.peek();

    let form = match token {
        // There are no tokens at all.
        // TODO: In the future a different error can be used, but with
        // anyhow that's not easy and nil is fine for now
        Token::EOF => MalType::Nil,
        Token::OpenParen => {
            reader.next();
            read_list_like(reader, Token::CloseParen)?
        }
        Token::OpenBracket => {
            reader.next();
            read_list_like(reader, Token::CloseBracket)?
        }
        Token::OpenCurly => {
            reader.next();
            read_list_like(reader, Token::CloseCurly)?
        }
        // The read_list should consume all *balanced* close
        // parens, which means we have some extra if we find this
        Token::CloseParen => bail!("Unbalanced closing parenthesis"),
        Token::CloseBracket => bail!("Unbalanced closing bracket"),
        _ => read_atom(reader)?,
    };

    Ok(form)
}
