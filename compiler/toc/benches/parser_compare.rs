//! Comparing parser implementations
use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};

use toc_frontend::parser as old_parser;
use toc_parser as new_parser;

use toc_frontend::context::{CompileContext, SourceMap};

use std::sync::Arc;

fn compare_parsers(c: &mut Criterion) {
    let mut group = c.benchmark_group("Parsers");
    let files = &[
        ("ui_util.tu", include_str!("source_files/ui_util.tu")),
        ("state.t", include_str!("source_files/state.t")),
        ("match.t", include_str!("source_files/match.t")),
        ("main.t", include_str!("source_files/main.t")),
    ];

    for (name, source) in files {
        group.bench_with_input(BenchmarkId::new("Old Parser", name), &source, |b, src| {
            b.iter(|| {
                let scanner = toc_frontend::scanner::Scanner::scan_source(black_box(src));

                let context = CompileContext::new(SourceMap::new());
                let context = Arc::new(context);

                let mut parser = old_parser::Parser::new(scanner, true, context);
                let _r = black_box(parser.parse());
            });
        });
        group.bench_with_input(BenchmarkId::new("New Parser", name), &source, |b, src| {
            b.iter(|| {
                let src = black_box(src);
                let _r = black_box(new_parser::parse(src));
            });
        });
    }

    group.finish();
}

criterion_group!(benches, compare_parsers);
criterion_main!(benches);
