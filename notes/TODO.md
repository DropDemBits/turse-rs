# TODO

## Compiler

- [ ] HIR Lowering
  - [ ] Stmt
    - [x] ConstVarDecl
    - [ ] TypeDecl
    - [ ] BindDecl
    - [ ] ProcDecl
    - [ ] FcnDecl
    - [ ] ProcessDecl
    - [ ] ExternalDecl
    - [ ] ForwardDecl
    - [ ] DeferredDecl
    - [ ] BodyDecl
    - [ ] ModuleDecl
    - [ ] ClassDecl
    - [ ] MonitorDecl
    - [x] AssignStmt
    - [ ] OpenStmt
    - [ ] CloseStmt
    - [X] PutStmt
    - [X] GetStmt
    - [ ] ReadStmt
    - [ ] WriteStmt
    - [ ] SeekStmt
    - [ ] TellStmt
    - [ ] ForStmt
    - [ ] LoopStmt
    - [ ] ExitStmt
    - [ ] IfStmt
    - [ ] CaseStmt
    - [x] BlockStmt
    - [ ] InvariantStmt
    - [ ] AssertStmt
    - [ ] CallStmt
    - [ ] ReturnStmt
    - [ ] ResultStmt
    - [ ] NewStmt
    - [ ] FreeStmt
    - [ ] TagStmt
    - [ ] ForkStmt
    - [ ] SignalStmt
    - [ ] PauseStmt
    - [ ] QuitStmt
    - [ ] BreakStmt
    - [ ] CheckednessStmt
    - [ ] PreStmt
    - [ ] InitStmt
    - [ ] PostStmt
    - [ ] HandlerStmt
    - [ ] InheritStmt
    - [ ] ImplementStmt
    - [ ] ImplementByStmt
    - [ ] ImportStmt
    - [ ] ExportStmt
  - [ ] Expr
    - [x] LiteralExpr
    - [ ] ObjClassExpr
    - [ ] InitExpr
    - [ ] NilExpr
    - [ ] SizeOfExpr
    - [x] BinaryExpr
    - [x] UnaryExpr
    - [x] ParenExpr
    - [x] NameExpr
    - [x] SelfExpr
    - [ ] FieldExpr
    - [ ] DerefExpr
    - [ ] CheatExpr
    - [ ] NatCheatExpr
    - [ ] ArrowExpr
    - [ ] IndirectExpr
    - [ ] BitsExpr
    - [ ] CallExpr
  - [ ] Type
    - [x] PrimType
    - [ ] NameType
    - [ ] RangeType
    - [ ] EnumType
    - [ ] ArrayType
    - [ ] SetType
    - [ ] RecordType
    - [ ] UnionType
    - [ ] PointerType
    - [ ] FcnType
    - [ ] ProcType
    - [ ] CollectionType
    - [ ] ConditionType
  - [ ] Preproc
    - [ ] PreprocIf stmt substitution
    - [ ] PreprocExpr evaluation
    - [ ] PreprocInclude insertion
      - [ ] Gather include files
- [ ] Import & dependency resolution
- [ ] Typeck
- [ ] Const Eval (in `toc_analysis`)
  - [ ] Cache evals of `const` vars
  - [ ] Lazy eval of constants
  - [ ] SizeOf structure size computation
- [ ] HIR Codegen
- [ ] FileDb & FileId maps
- [x] Additional `ReportMessage` notes
  - [ ] Integration with `annotate-snippets`

- [ ] MIR Lowering
- [ ] MIR Optimization
- [ ] MIR Codegen

## LSP Client/Server

- [ ] Basic error reporting

## HIR Lowering Steps

### Group 1: Bare Bones

Putting together a simple program to work with for both HIR-based codegen & typeck.

- ConstVarDecl
- AssignStmt
- BlockStmt
- LiteralExpr
- BinaryExpr
- UnaryExpr
- ParenExpr
- NameExpr
- PrimType

### Group 2: Simple I/O

Interfacing visually with the Turing runtime.

- PutStmt
- GetStmt

### Group 3: Control Flow

Interesting codegen results from basic control flow.

- LoopStmt
- IfStmt
- ExitStmt
- CaseStmt
- ForStmt
