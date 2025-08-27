use std::process::exit;

use rustyline::{error::ReadlineError, DefaultEditor, Result};

fn main() -> Result<()> {
    let mut line_editor = DefaultEditor::new()?;

    let code = loop {
        let sig = line_editor.readline("user> ");

        match sig {
            Ok(buffer) => {
                let res = mal_rep(&buffer);
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

fn mal_read(input: &str) -> &str {
    input
}
fn mal_eval(input: &str) -> &str {
    input
}
fn mal_print(input: &str) -> &str {
    input
}

fn mal_rep(input: &str) -> &str {
    let res = mal_read(input);
    let res = mal_eval(res);
    mal_print(res)
}
