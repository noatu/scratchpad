use derive_builder::Builder;

#[derive(Builder)]
pub struct BuilderTest {
    mandatory: u8,
    #[builder(each = "collect")]
    collection: Vec<u8>,
    optional: Option<String>,
}

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
}
