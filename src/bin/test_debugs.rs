fn main() {
    lxca::ir::with_context(|ctx| {
        let file = lxca::ir::test_files::return_42("x86_64-pc-linux-gnu", ctx);
        println!("{file:#?}");
    })
}
