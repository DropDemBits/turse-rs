//! Comparing scanner implementations
use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};

use toc_frontend::scanner as old_scanner;
use toc_scanner as new_scanner;

fn compare_scanners(c: &mut Criterion) {
    let mut group = c.benchmark_group("Scanners");
    let files = &[
        ("ui_util.tu", include_str!("source_files/ui_util.tu")),
        ("state.t", include_str!("source_files/state.t")),
        ("match.t", include_str!("source_files/match.t")),
        ("main.t", include_str!("source_files/main.t")),
    ];

    for (name, source) in files {
        group.bench_with_input(BenchmarkId::new("Old Scanner", name), &source, |b, src| {
            b.iter(|| {
                let scanner = old_scanner::Scanner::scan_source(black_box(src));
                let _e: Vec<_> = black_box(scanner.collect());
            });
        });
        group.bench_with_input(BenchmarkId::new("New Scanner", name), &source, |b, src| {
            b.iter(|| {
                let scanner = new_scanner::Scanner::new(black_box(src));
                let _e = black_box(scanner.collect_all());
            });
        });
    }

    group.finish();
}

criterion_group!(benches, compare_scanners);
criterion_main!(benches);
