mod printer;
mod reader;
mod types;

use std::process::exit;

use rustyline::{DefaultEditor, error::ReadlineError};

use crate::types::MalType;

fn main() -> anyhow::Result<()> {
    let mut line_editor = DefaultEditor::new()?;

    let code = loop {
        let sig = line_editor.readline("user> ");

        match sig {
            Ok(buffer) => {
                let res = mal_rep(&buffer)?;
                println!("{res}");
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

fn mal_eval(input: MalType) -> MalType {
    input
}

fn mal_print(input: MalType) -> String {
    printer::pr_str(input)
}

fn mal_rep(input: &str) -> anyhow::Result<String> {
    let res = mal_read(input);
    if let Err(err) = res {
        println!("{err}");
        return Ok(String::new());
    }
    let res = mal_eval(res.unwrap());
    Ok(mal_print(res))
}
