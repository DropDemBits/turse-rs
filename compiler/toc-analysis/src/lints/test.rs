//! Tests for all of the lints
// ???: Should this just be a load of file tests, since we never make a structure?

use toc_reporting::MessageBundle;

use crate::{db::HirAnalysis, test_db::TestDb, ty};

macro_rules! assert_lint {
    ($source:expr) => {{
        let source: &str = $source;
        insta::assert_snapshot!(insta::internals::AutoName, do_lint(source), source);
    }};
}

fn do_lint(source: &str) -> String {
    let (db, pkg) = TestDb::from_source(source);
    let res = db.lint_package(pkg);
    res.messages().assert_no_delayed_reports();

    stringify_lint_results(res.messages())
}

fn stringify_lint_results(messages: &MessageBundle) -> String {
    use std::fmt::Write;

    let mut s = String::new();

    // Pretty print the messages
    for err in messages.iter() {
        write!(&mut s, "\n{err}").unwrap();
    }

    s
}

#[test]
fn lint_impl_limits_string_length() {
    // Strings limit is sorta long, so we'll still make the string programmatically
    const SEQ_LEN: usize = ty::MAX_STRING_LEN as usize;
    const FILLER: &str = "✨✨✨✨✨✨✨✨";
    let mut big_charseq = String::with_capacity(SEQ_LEN);

    for _ in 0..(SEQ_LEN / FILLER.len()) {
        // ✨sparkle✨ to death
        big_charseq.push_str(FILLER);
    }

    // Fill the remaining length with a's
    for _ in big_charseq.len()..SEQ_LEN {
        big_charseq.push('a');
    }

    assert_lint!(&format!(r#"const a := "{}""#, big_charseq));
}

#[test]
fn lint_impl_limits_char_length() {
    // Character sequences are very long, so we'll have to make the string programmatically
    const SEQ_LEN: usize = ty::MAX_CHAR_N_LEN as usize;
    const FILLER: &str = "✨✨✨✨✨✨✨✨";
    let mut big_charseq = String::with_capacity(SEQ_LEN);

    for _ in 0..(SEQ_LEN / FILLER.len()) {
        // ✨sparkle✨ to death
        big_charseq.push_str(FILLER);
    }

    // Fill the remaining length with a's
    for _ in big_charseq.len()..SEQ_LEN {
        big_charseq.push('a');
    }

    assert_lint!(&format!(r#"const a := '{}'"#, big_charseq));
}

#[test]
fn lint_impl_limits_int_value_greater_than() {
    assert_lint!("var k := 16#100000000");
}
