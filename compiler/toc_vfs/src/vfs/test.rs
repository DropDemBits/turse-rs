use super::*;

#[test]
fn resolve_dedots_expansion() {
    let mut vfs = Vfs::new();

    vfs.set_prefix_expansion(BuiltinPrefix::Oot, "/path/to/oot");
    let help = vfs.intern_path("/path/to/some/help".into());

    let resolve = vfs.resolve_path(None, "%oot/../other/.././oot/../some/help");
    assert_eq!(resolve, PathResolution::Interned(help));
}

#[test]
fn resolve_dedots_relative() {
    let mut vfs = Vfs::new();

    let child = vfs.intern_path("/src/subdir/child".into());
    let main = vfs.intern_path("/src/main.t".into());

    let resolve = vfs.resolve_path(Some(child), "../././././main.t");
    assert_eq!(resolve, PathResolution::Interned(main));
}

// Windows-specific tests
#[cfg(windows)]
mod windows {
    use super::*;

    #[test]
    fn fixup_joined_paths() {
        // With same drive prefix
        assert_eq!(
            join_dedot(r#"C:\wah\bloop"#.into(), Path::new(r#"C:"#)),
            Path::new(r#"C:\wah\bloop"#)
        );
        assert_eq!(
            join_dedot(r#"C:\wah\bloop"#.into(), Path::new(r#"C:."#)),
            Path::new(r#"C:\wah\bloop"#)
        );
        assert_eq!(
            join_dedot(r#"C:\wah\bloop"#.into(), Path::new(r#"C:.."#)),
            Path::new(r#"C:\wah"#)
        );
        assert_eq!(
            join_dedot(r#"C:\wah\bloop"#.into(), Path::new(r#"C:a"#)),
            Path::new(r#"C:\wah\bloop\a"#)
        );

        // With different drive prefix
        assert_eq!(
            join_dedot(r#"D:\wah\bloop"#.into(), Path::new(r#"C:"#)),
            Path::new(r#"C:\wah\bloop"#)
        );
        assert_eq!(
            join_dedot(r#"D:\wah\bloop"#.into(), Path::new(r#"C:."#)),
            Path::new(r#"C:\wah\bloop"#)
        );
        assert_eq!(
            join_dedot(r#"D:\wah\bloop"#.into(), Path::new(r#"C:.."#)),
            Path::new(r#"C:\wah"#)
        );
        assert_eq!(
            join_dedot(r#"D:\wah\bloop"#.into(), Path::new(r#"C:a"#)),
            Path::new(r#"C:\wah\bloop\a"#)
        );

        // With different case drive prefix
        assert_eq!(
            join_dedot(r#"c:\wah\bloop"#.into(), Path::new(r#"C:"#)),
            Path::new(r#"C:\wah\bloop"#)
        );
        assert_eq!(
            join_dedot(r#"c:\wah\bloop"#.into(), Path::new(r#"C:."#)),
            Path::new(r#"C:\wah\bloop"#)
        );
        assert_eq!(
            join_dedot(r#"c:\wah\bloop"#.into(), Path::new(r#"C:.."#)),
            Path::new(r#"C:\wah"#)
        );
        assert_eq!(
            join_dedot(r#"c:\wah\bloop"#.into(), Path::new(r#"C:a"#)),
            Path::new(r#"C:\wah\bloop\a"#)
        );

        // With drive prefix
        assert_eq!(
            join_dedot(r#"\wah\bloop"#.into(), Path::new(r#"C:"#)),
            Path::new(r#"C:\wah\bloop"#)
        );
        assert_eq!(
            join_dedot(r#"\wah\bloop"#.into(), Path::new(r#"C:."#)),
            Path::new(r#"C:\wah\bloop"#)
        );
        assert_eq!(
            join_dedot(r#"\wah\bloop"#.into(), Path::new(r#"C:.."#)),
            Path::new(r#"C:\wah"#)
        );
        assert_eq!(
            join_dedot(r#"\wah\bloop"#.into(), Path::new(r#"c:a"#)),
            Path::new(r#"c:\wah\bloop\a"#)
        );
    }

    #[test]
    fn no_fixup_paths() {
        // Don't fixup non-disk prefix base paths
        assert_eq!(
            join_dedot(r#"\\?\wah\bloop"#.into(), Path::new(r#"C:a"#)),
            Path::new(r#"\\?\wah\bloop\a"#)
        );
        assert_eq!(
            join_dedot(r#"\\?\UNC\remote\wah\bloop"#.into(), Path::new(r#"C:a"#)),
            Path::new(r#"\\?\UNC\remote\wah\bloop\a"#)
        );
        assert_eq!(
            join_dedot(r#"\\?\C:\wah\bloop"#.into(), Path::new(r#"C:a"#)),
            Path::new(r#"\\?\C:\wah\bloop\a"#)
        );
        assert_eq!(
            join_dedot(r#"\\.\wah\bloop"#.into(), Path::new(r#"C:a"#)),
            Path::new(r#"\\.\wah\bloop\a"#)
        );
        assert_eq!(
            join_dedot(r#"\\UNC\remote\wah\bloop"#.into(), Path::new(r#"C:a"#)),
            Path::new(r#"\\UNC\remote\wah\bloop\a"#)
        );
    }
}

#[test]
fn paths_need_joining() {
    assert_eq!(needs_joining(Path::new("../a")), true);
    assert_eq!(needs_joining(Path::new("./a")), true);
    assert_eq!(needs_joining(Path::new("a")), true);
    assert_eq!(needs_joining(Path::new("")), true);

    if cfg!(windows) {
        // Drive relative paths needs joining (and fixups)
        assert_eq!(needs_joining(Path::new(r#"C:..\a"#)), true);
        assert_eq!(needs_joining(Path::new(r#"C:.\a"#)), true);
        assert_eq!(needs_joining(Path::new(r#"C:a"#)), true);
        assert_eq!(needs_joining(Path::new(r#"C:"#)), true);
    }
}

#[test]
fn paths_dont_need_joining() {
    assert_eq!(needs_joining(Path::new("/")), false);

    if cfg!(windows) {
        // Drive absolute paths don't need joining
        assert_eq!(needs_joining(Path::new(r#"C:\"#)), false);

        // All other prefixes don't need joining
        assert_eq!(needs_joining(Path::new(r#"\\?\heyo"#)), false);
        assert_eq!(needs_joining(Path::new(r#"\\?\UNC\remote\place"#)), false);
        assert_eq!(needs_joining(Path::new(r#"\\?\C:\verbatim_disk"#)), false);
        assert_eq!(needs_joining(Path::new(r#"\\.\NUL"#)), false);
        assert_eq!(needs_joining(Path::new(r#"\\UNC\remote\location"#)), false);
    }
}
