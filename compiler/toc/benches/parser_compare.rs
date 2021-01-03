//! Comparing parser implementations
use criterion::{black_box, criterion_group, criterion_main, Criterion};

use toc_frontend::parser as old_parser;
use toc_parser as new_parser;

use toc_frontend::context::{CompileContext, SourceMap};

use std::sync::Arc;

macro_rules! make_bench_over {
    ($i:ident => $src_path:literal) => {
        fn $i(c: &mut Criterion) {
            let mut group = c.benchmark_group($src_path);

            let source = include_str!($src_path);
            group.bench_with_input("old parser", source, |b, src| {
                b.iter(|| {
                    let scanner = toc_frontend::scanner::Scanner::scan_source(black_box(src));

                    let context = CompileContext::new(SourceMap::new());
                    let context = Arc::new(context);

                    let mut parser = old_parser::Parser::new(scanner, true, context);
                    let _r = black_box(parser.parse());
                })
            });

            group.bench_with_input("new parser", source, |b, src| {
                b.iter(|| {
                    let src = black_box(src);
                    let _r = black_box(new_parser::parse(src));
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
