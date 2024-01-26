use grammar_c::{GrammarCParser, DefaultBuilder};
use rustemo::Parser;


#[rustfmt::skip]
mod grammar_c;
#[allow(unused)]
#[rustfmt::skip]
mod grammar_c_actions;


fn main() {
    let forest = GrammarCParser::new().parse(r#"
    int main(void) {
        return 0;
    }
"#).unwrap();

    // Evaluate each tree from the forest
    let results = forest
        .into_iter()
        .map(|tree| {
            let mut builder = DefaultBuilder::new();
            tree.build(&mut builder)
        })
        .collect::<Vec<_>>();
    println!("{:#?}", results);
}
