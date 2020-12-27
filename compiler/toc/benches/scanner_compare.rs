//! Comparing scanner implementations
use criterion::{black_box, criterion_group, criterion_main, Criterion};

use toc_frontend::scanner as old_scanner;
use toc_scanner as new_scanner;

macro_rules! make_bench_over {
    ($i:ident => $src_path:literal) => {
        fn $i(c: &mut Criterion) {
            let mut group = c.benchmark_group($src_path);

            let source = include_str!($src_path);
            group.bench_with_input("old scanner", source, |b, src| {
                b.iter(|| {
                    let scanner = old_scanner::Scanner::scan_source(black_box(src));
                    let _e: Vec<toc_core::token::Token> = black_box(scanner.collect());
                })
            });

            group.bench_with_input("new scanner", source, |b, src| {
                b.iter(|| {
                    let scanner = new_scanner::Scanner::new(black_box(src));
                    let _e: Vec<(toc_scanner::TokenKind, &str)> = black_box(scanner.collect());
                })
            });

            group.finish();
        }
    };
}

make_bench_over!(state_t => "source_files/state.t");
make_bench_over!(ui_util_tu => "source_files/ui_util.tu");
make_bench_over!(main_t => "source_files/main.t");
make_bench_over!(match_t => "source_files/match.t");

criterion_group!(benches, state_t, ui_util_tu, main_t, match_t);
criterion_main!(benches);
