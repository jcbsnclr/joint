use crate::parser::Expression;

pub fn compile(code: Vec<Expression>) -> String {
    let mut outp = String::new();
    
    outp.push_str("#include <stdio.h>\n");
    outp.push_str("int main(void){\n");

    for expr in code {
        match expr {
            Expression::Print(n) => outp.push_str(&format!("\tprintf(\"%d\",{});\n", n))
        }
    }

    outp.push_str("}");

    outp
}