# VFS Query Interface

VFS is the system interface between the main compiler and the file system.
Since IO is messy.

## Problems Space

### File Encoding

The rest of the compiler expects that the format of a file source is encoded as a UTF-8 byte string (from using `String`).
However, since the Turing editor was primarily used on Windows, a file source could theoretically be encoded in any of the
file encodings supported by Windows and Turing editor. In practice, "windows-1252", "iso-8859-1", and potentially "utf-16\[le/be\]"
are the only ones we have to be concerned about.

> Aside: The Turing editor was primarily used in English-speaking North America, which "windows-1252" covers all of the
> characters used in that region. There was also French language in the Turing editor, but that is also covered by "windows-1252".
>
> The Turing runtime also supported DOS Code Page 437 as a runtime charset, but this appears to only be for the display of characters
> in the output window.

Additionally, the compiler expects file paths to also be formatted as UTF-8 byte strings since file paths are derived from file sources.
It should not be exposed to operating system specific encodings of paths (i.e. it should not need to store paths as `OsStr` or `OsString`).

At some point, there needs to be a conversion between the compiler's encoding and the file/operating system's encoding.
This should be handled by the OS-facing side of the VFS.

#### Options for conversion

- Only accept UTF-8 formatted files
  - Simplest, yet there exists real-world files not encoded in UTF-8 that we should accept
- Convert files from the original encoding into UTF-8...
  - Always automatically detect encoding (with user override)
  - When specified by the user
    - Choice between a specific encoding and any supported encoding
    - If not specified, would need to include reporting invalid characters (a simple `Range::contains` check) and a potential encoding

#### Inspiration

- [WHATWG Encoding spec]
- [rust-encoding]

[WHATWG Encoding spec]: https://encoding.spec.whatwg.org/
[rust-encoding]: https://crates.io/crates/encoding

### Path specification

While path strings in source files syntactically appear as normal string literals, they have to be handled specially.
On Windows, the default path specifier is `\`, which conflicts with the slash escape sequence start in a string literal.
The original Turing compiler handles this by reading the string as-is as a path. This is mostly an issue of how we extract
the string path and pass it to the VFS, and not specifically the VFS itself.

Percent prefix expansion (e.g. paths starting with "%oot" or "%home") are an issue that the VFS needs to handle, since the
compiler should not need to store what these prefixes expand to.

### Path normalization

`std::fs::canonicalize` currently canonicalizes paths in UNC form on Windows.
While this does transform relative paths into a common representation, some drivers on Windows do not properly handle UNC paths.

Since we're only concerned about transforming relative paths into absolute paths given a parent path, path normalization appears
to be the better option. [`normpath`] appears to be suitable for this purpose.

[`normpath`]: https://crates.io/crates/normpath

### Variations on file sources

While we primarily source files from the operating system / runtime environment file system in user-facing builds, for testing,
we'd like for there to be a way to provide a constructed file system for allowing testing of things such as the `include` macro
and `import` statements.

### File modification and file watching

When serving as a language server, the language client will inform the server of any file changes on watched files. These changes
need to somehow be propagated back up to the VFS query interface so that file source queries can be properly invalidated.

## API Mockup

```text
VFS Queries
|
VfsStorage
- Key path storage (all `#[salsa::input]`)
  - Root compilation file
  - Turing home directory ("%oot", "%help" is derived)
  - User home directory ("%home")
  - Temporary directory root ("%temp")
  - Job directory [user specified] ("%job")
- Path Interning (via `#[salsa::interned]`)
- Path Expansion
|
VFS Backend
- Path normalization
- File loading
```
