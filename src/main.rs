mod lexer;
mod parser;

use lexer::Lexer;

const TEST: &str = r#"
# test
2 4 + 3 / 2 * print
"#;

fn main() {
    let lexer = Lexer::from_source(TEST);

    let prog = parser::parse(lexer)
        .unwrap();

    println!("{:?}", prog);
}
