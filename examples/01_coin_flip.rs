//////////////////////////////////////////////////////////////
// Simple coin flip example in Leaf
//
// The standard extension of Leaf code files should be `.lf`
// We use `.rs` here for syntax highlighting purposes only.
//////////////////////////////////////////////////////////////

general fn coin_flip() -> bit {
    let q = qalloc();
    H(&q);
    measr(q)
}

general fn main() -> bit {
    coin_flip()
}
