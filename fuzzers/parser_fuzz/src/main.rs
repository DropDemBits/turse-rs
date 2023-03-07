// Workaround for the `afl` crate depending on `xdg`, which isn't compilable on platforms other than linux/unix
// Running cargo [build/run] [--release] isn't affected because this is never touched, but it still trips up
// rust-analyzer, resulting in errors outside of this repo
#[cfg(target_os = "linux")]
mod inner {
    use toc_span::FileId;

    pub(crate) fn do_fuzz() {
        let dummy_file = FileId::dummy(1);
        afl::fuzz!(|data: &[u8]| {
            if let Ok(s) = std::str::from_utf8(data) {
                let _ = toc_parser::parse(dummy_file, s);
            }
        });
    }
}

#[cfg(not(target_os = "linux"))]
mod inner {
    pub(crate) fn do_fuzz() {
        panic!("fuzzing is not supported on platforms other than linux");
    }
}

fn main() {
    inner::do_fuzz();
}
