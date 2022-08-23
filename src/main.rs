mod lexer;
mod parser;

use lexer::Lexer;

const TEST: &str = r#"
1 2 + 3 * 4 /
"#;

fn main() {
    let lexer = Lexer::from_source(TEST);

    let prog = parser::parse(lexer)
        .unwrap();

    println!("{:?}", prog);
}
