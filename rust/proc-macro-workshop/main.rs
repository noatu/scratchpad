use derive_builder::Builder;
use seq::seq;

#[derive(Builder)]
pub struct BuilderTest {
    mandatory: u8,
    #[builder(each = "collect")]
    collection: Vec<u8>,
    optional: Option<String>,
}

seq!(N in 0..4 {
    enum Enum~N { #( Variant~N~42, )* }
});

fn main() {
    let builder_test = BuilderTest::builder()
        .collect(4)
        .mandatory(42)
        .collect(2)
        .build()
        .unwrap();

    assert_eq!(builder_test.mandatory, 42);
    assert_eq!(builder_test.collection, vec![4, 2]);
    assert_eq!(builder_test.optional, None);

    // hiiiii
    seq!(_ in 0..4 {
        println!(concat!("h", #("i"),*, "i"));
    });
    // hiiiii1
    // ...
    // hiiiii5
    seq!(N in 1..=5 {
        println!(concat!("h", #("i",)* N));
    });
}
