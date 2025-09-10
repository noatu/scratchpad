use derive_builder::Builder;
use seq::seq;
use sorted::sorted;

#[derive(Builder)]
pub struct BuilderTest {
    mandatory: u8,
    #[builder(each = "collect")]
    collection: Vec<u8>,
    optional: Option<String>,
}

seq!(N in 0..4 {
    #[sorted]
    enum Enum { #( Variant~N~42, )* }
});

#[sorted::check]
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

    #[sorted]
    match Enum::Variant042 {
        Enum::Variant042 => (),
        Enum::Variant142 => (),
        Enum::Variant242 => (),
        Enum::Variant342 => (),
    }
}
