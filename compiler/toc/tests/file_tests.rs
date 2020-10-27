use goldentests::{TestConfig, TestResult};

#[test]
fn run_frontend_tests() -> TestResult<()> {
    let config = TestConfig::new(
        "../../target/debug/toc",
        "../../compiler/toc/tests/frontend",
        "%%% ",
    )?;

    config.run_tests()
}
