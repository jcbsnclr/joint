mod lexer;
mod parser;
mod compiler;

use lexer::Lexer;

const TEST: &str = r#"
print 123
print 456
print 789
"#;

fn main() {
    let lexer = Lexer::from_source(TEST);

    let prog = parser::parse(lexer)
        .unwrap();

    println!("{}", compiler::compile(prog));
}
