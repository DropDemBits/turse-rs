# TODO

## Compiler

- [ ] HIR Lowering
  - [ ] Stmt
    - [x] ConstVarDecl
    - [x] TypeDecl
    - [x] BindDecl
    - [x] ProcDecl
    - [x] FcnDecl
    - [x] ProcessDecl
    - [ ] ExternalDecl
    - [ ] ForwardDecl
    - [ ] DeferredDecl
    - [ ] BodyDecl
    - [x] ModuleDecl
    - [ ] ClassDecl
    - [ ] MonitorDecl
    - [x] AssignStmt
    - [ ] OpenStmt
    - [ ] CloseStmt
    - [x] PutStmt
    - [x] GetStmt
    - [ ] ReadStmt
    - [ ] WriteStmt
    - [ ] SeekStmt
    - [ ] TellStmt
    - [x] ForStmt
    - [x] LoopStmt
    - [x] ExitStmt
    - [x] IfStmt
    - [x] CaseStmt
    - [x] BlockStmt
    - [ ] InvariantStmt
    - [ ] AssertStmt
    - [~] CallStmt
      - [x] On subprograms
      - [x] On arrays
      - [x] On set constructor
      - [ ] On pointer types
      - [ ] On character sequences
    - [x] ReturnStmt
    - [x] ResultStmt
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
    - [x] ExportStmt
  - [ ] Expr
    - [x] LiteralExpr
    - [ ] ObjClassExpr
    - [x] InitExpr
    - [ ] NilExpr
    - [ ] SizeOfExpr
    - [x] BinaryExpr
    - [x] UnaryExpr
    - [x] ParenExpr
    - [x] NameExpr
      - [x] As reference
      - [x] As paramless call
    - [x] SelfExpr
    - [x] FieldExpr
    - [x] DerefExpr
    - [ ] CheatExpr
    - [ ] NatCheatExpr
    - [ ] ArrowExpr
    - [ ] IndirectExpr
    - [ ] BitsExpr
    - [~] CallExpr
      - [x] On subprograms
      - [x] On arrays
      - [x] On set constructor
      - [ ] On pointer types
      - [ ] On character sequences
  - [ ] Type
    - [x] PrimType
    - [x] NameType
    - [x] RangeType
    - [x] EnumType
    - [x] ArrayType
    - [x] SetType
    - [ ] RecordType
    - [ ] UnionType
    - [x] PointerType
    - [x] FcnType
    - [x] ProcType
    - [ ] CollectionType
    - [ ] ConditionType
  - [ ] Preproc
    - [ ] PreprocIf stmt substitution
    - [ ] PreprocExpr evaluation
    - [ ] PreprocInclude insertion
      - [x] Gather include files
- [ ] Import & dependency resolution
- [ ] Typeck
  - [ ] Stmt
    - [x] ConstVar
      - [x] Binding type to def
      - [x] Handling `init()` as the initialzer expression
    - [x] Type
    - [x] Bind
    - [x] Proc
    - [x] Fcn
    - [x] Process
    - [ ] External
    - [ ] Forward
    - [ ] Deferred
    - [ ] Body
    - [x] Module
    - [ ] Class
    - [ ] Monitor
    - [x] Assign
    - [ ] Open
    - [ ] Close
    - [x] Put
    - [x] Get
    - [ ] Read
    - [ ] Write
    - [ ] Seek
    - [ ] Tell
    - [x] For
    - [x] Loop
    - [x] Exit
    - [x] If
    - [x] Case
    - [x] Block
    - [ ] Invariant
    - [ ] Assert
    - [~] Call
      - [x] On subprograms
      - [x] On arrays
      - [x] On set constructor
      - [ ] On pointer types
      - [ ] On character sequences
    - [x] Return
    - [x] Result
    - [ ] New
    - [ ] Free
    - [ ] Tag
    - [ ] Fork
    - [ ] Signal
    - [ ] Pause
    - [ ] Quit
    - [ ] Break
    - [ ] Checkedness
    - [ ] Pre
    - [ ] Init
    - [ ] Post
    - [ ] Handler
  - [ ] Expr
    - [x] Literal
    - [ ] ObjClass
    - [x] Init
    - [ ] Nil
    - [ ] SizeOf
    - [ ] Binary
      - [x] Arithmetic
      - [x] Bitwise
      - [x] Logical
      - [ ] Comparison
        - [x] Numeric
        - [x] Lexicographic (over charseqs)
        - [ ] Class hierarchy
        - [x] Enums
        - [x] Sets (sub/superset tests)
      - [x] String concatenation
      - [x] Set manipulation
    - [x] Unary
    - [x] Name
    - [ ] Self
    - [x] Field
    - [x] Deref
    - [ ] Cheat
    - [ ] NatCheat
    - [ ] Arrow
    - [ ] Indirect
    - [ ] Bits
    - [~] Call
      - [x] On subprograms
      - [x] On arrays
      - [x] On set constructor
      - [ ] On pointer types
      - [ ] On character sequences
  - [ ] Type
    - [x] Prim (SeqLength)
    - [x] Name
      - [x] Just name
      - [x] Through module paths
    - [x] Range
    - [x] Enum
    - [x] Array
    - [x] Set
    - [ ] Record
    - [ ] Union
    - [x] Pointer
    - [x] Fcn
    - [x] Proc
    - [ ] Collection
    - [ ] Condition
    - [x] Opaque
- [ ] Const Eval (in `toc_analysis`)
  - [x] Cache evals of `ConstExpr`s
  - [x] Deferred eval of `const` vars
  - [ ] Evaluate all valid const ops
  - [ ] SizeOf structure size computation
- [~] HIR Codegen
  - [ ] External functions support
- [x] Additional `ReportMessage` notes
  - [x] Integration with `ariadne`
- [ ] Multi-file compilation
  - [x] Select file dependencies
  - [x] Load in the dependent file sources
  - [x] Generate a test fixture VFS from a text string
  - [ ] Include tree stuff
  - [ ] Reasonable limit on files included

### Potential

- [ ] MIR Lowering
- [ ] MIR Optimization
- [ ] MIR Codegen

### Const Operations

#### Arithmetic

- [ ] Add
  - [x] Over numbers
  - [ ] Over sets
  - [x] Over char seqs
- [ ] Sub
  - [x] Over numbers
  - [ ] Over sets
- [ ] Mul
  - [x] Over numbers
  - [ ] Over sets
- [x] Div
- [x] RealDiv
- [x] Mod
- [x] Rem
- [x] Exp
- [x] Identity
- [x] Negate

#### Bitwise & Logic

- [x] And
- [x] Or
- [x] Xor
- [x] Shl
- [x] Shr
- [x] Imply
- [x] Not

#### Comparison

- [ ] Ordering
  - [x] Over numbers
  - [x] Over char seqs
  - [ ] Over sets
- [ ] Equality
  - [x] Over numbers
  - [x] Over booleans
  - [x] Over char seqs
  - [ ] Over sets

#### Type Conversion

- [ ] ord
- [ ] chr
- [ ] intreal
- [ ] natreal
- [ ] ceil
- [ ] round
- [ ] floor

#### Other

- [ ] pred
- [ ] succ
- [ ] lower
- [ ] upper (?)
- [ ] bits
- [ ] abs
- [ ] max
- [ ] min
- [ ] sign
- [ ] Set Constructor
- [ ] (BinaryOp) In

## LSP Client/Server

- [x] Basic error reporting

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
