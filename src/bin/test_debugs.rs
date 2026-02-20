use lxca::ir::{test_files::TEST_FILES, with_context};

fn main() {
    let target = std::env::var("TEST_TARGET").unwrap_or_else(|_| "x86_64-pc-linux-gnu".to_string());
    for &(name, test_fn) in TEST_FILES {
        println!("Test {name}:");

        with_context(|ctx| {
            let file = test_fn(&target, ctx);

            println!("{file}");
        })
    }
}
