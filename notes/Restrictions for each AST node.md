# Restrictions for each AST node

We don't care about checking semantic things like type correctness or correct number of parameters, as AST validation
only aims to restrict the parse tree to be in line with the original grammatical restrictions.
Nevertheless, semantic restrictions are still specified so that this serves as a unified reference for all
node restrictions.

## Definitions

- Top-level block: The main block, or any module-kind block (includes class, monitor, and module blocks)

## Nodes

### `Source`

- If the `unit` token is present, the only nodes that can be present are the following:
  - `ModuleDecl`
  - `ClassDecl`
  - `MonitorDecl`
- If the `unit` token is not present, then an ImportStmt is allowed as the first statement

### `Name`

- None

### `NameList`

- None

### `UnqualifiedAttr`

- None

### `PervasiveAttr`

- None

### `RegisterAttr`

- None

### `ConstAttr`

- None

### `VarAttr`

- None

### `CheatAttr`

- None

### `ForwardAttr`

- None

### `OpaqueAttr`

- None

### `PreprocGlob`

- None (statement position is enforced by the Parser)

### `PPInclude`

- None

### `PPIf`

- None

### `PPElseif`

- Rejected as the first preprocessor command in a PreprocGlob

### `PPElse`

- Rejected as the first preprocessor command in a PreprocGlob

### `PPEndIf`

- Rejected as the first preprocessor command in a PreprocGlob

### `PPBinaryExpr`

- None

### `PPUnaryExpr`

- None

### `PPNameExpr`

- None

### `PPParenExpr`

- None

### `PPTokenBody`

- None

### `StmtList`

- None

### `ConstVarDecl`

- (Parser Restriction) Initializer expr is required if type spec is absent
- Rejects `RegisterAttr` in top-level blocks
- Requires `InitExpr` to be the initializer expr if the type is of an unsized array
  (i.e. an array with any range ending with `*`)
- Accepts `InitExpr` if the type is an array, record, or union
- Rejects `InitExpr` for all other type specs (including absent ones)

### `TypeDecl`

- None (restrictions in types declared are done by the types themselves)

### `BindDecl`

- Rejected in top-level blocks

### `ProcDecl`

- Accepted only in top-level blocks
- (Enforced by Parser) The following nodes are allowed as the first statements in the specified order:
  - `ImportStmt`
  - `PreStmt`
  - `InitStmt`
  - `PostStmt`
  - `HandlerStmt`
  All specified statements are not required to be present

### `ProcHeader`

- `DeviceSpec` is rejected if the node is not in a device monitor block
- `DeviceSpec` is rejected if the parent node is not a `ProcDecl`

### `DeviceSpec`

- None (Checked by `ProcHeader`)

### `FcnDecl`

- Accepted only in top-level blocks
- (Enforced by Parser) The following nodes are allowed as the first statements in the specified order:
  - `ImportStmt`
  - `PreStmt`
  - `InitStmt`
  - `PostStmt`
  - `HandlerStmt`
  All specified statements are not required to be present

### `FcnHeader`

- None

### `FcnResult`

- (Semantic Restriction) optional `Name` must only be used in the `PostStmt`

### `ProcessDecl`

- Accepted only in top-level blocks, excluding classes
- (Enforced by Parser) The following nodes are allowed as the first statements in the specified order:
  - `ImportStmt`
  - `PreStmt`
  - `InitStmt`
  - `PostStmt`
  - `HandlerStmt`
  All specified statements are not required to be present

### `ExternalDecl`

- `AddressSpec` must be a string literal (int exprs aren't accepted in this position yet)

### `ExternalVar`

- Always rejected

### `ForwardDecl`

- (Note) Name node in SubprogHeader is required
- (Semantic Restriction) Must have a corresponding `BodyDecl` within the import boundary

### `DeferredDecl`

- Only allowed in the top-level of a module-kind block

### `BodyDecl`

- Accepted only in top-level blocks
- (Enforced by Parser) The following nodes are allowed as the first statements in the specified order:
  - `PreStmt`
  - `InitStmt`
  - `PostStmt`
  - `HandlerStmt`
  All specified statements are not required to be present
- (Semantic Restriction) Only one `BodyDecl` must be present for each `ForwardDecl` or `DeferredDecl`

### `PlainHeader`

- None

### `ModuleDecl`

- Accepted only in top-level blocks
- The identifier in `EndGroup` must match the `ModuleDecl`'s identifier
- (Enforced by Parser) The following nodes are allowed as the first statements in the specified order:
  - `ImplementStmt`
  - `ImplementByStmt`
  - `ImportStmt`
  - `ExportStmt`
  - `PreStmt`
  All specified statements are not required to be present
- (Enforced by Parser) The following nodes are allowed as the final statement:
  - `PostStmt`
  All specified statements are not required to be present

### `ClassDecl`

- Accepted only in top-level blocks that are not monitors or classes
- The identifier in `EndGroup` must match the `ClassDecl`'s identifier
- Always reject `DeviceSpec` node, regardless of if the `monitor` token is present
- (Enforced by Parser) The following nodes are allowed as the first statements in the specified order:
  - `InheritStmt`
  - `ImplementStmt`
  - `ImplementByStmt`
  - `ImportStmt`
  - `ExportStmt`
  - `PreStmt`
  All specified statements are not required to be present
- (Enforced by Parser) The following nodes are allowed as the final statement:
  - `PostStmt`
  All specified statements are not required to be present

### `MonitorDecl`

- Accepted only in top-level blocks that are not monitors
- The identifier in `EndGroup` must match the `ClassDecl`'s identifier
- (Enforced by Parser) The following nodes are allowed as the first statements in the specified order:
  - `InheritStmt`
  - `ImplementStmt`
  - `ImplementByStmt`
  - `ImportStmt`
  - `ExportStmt`
  - `PreStmt`
  All specified statements are not required to be present
- (Enforced by Parser) The following nodes are allowed as the final statement:
  - `PostStmt`
  All specified statements are not required to be present

### `AssignStmt`

- (Semantic Restriction) Lhs must be a `Reference`

### `AsnOp`

- None

### `OpenStmt`

- None

### `OldOpen`

- None

### `OpenPath`

- (Type restriction) Must be a string-class type

### `OpenMode`

- None? (should open mode be checked here?)

### `NewOpen`

- `file_ref` must be a `Reference`
- Must not have conflicting `IoCap`s present
  - Conflict list:
    - `get` conflicts with `read`
    - `put` conflicts with `write`

### `IoCap`

- None

### `CloseStmt`

- None

### `OldClose`

- `file_ref` must be a `Reference`

### `NewClose`

- `file_ref` must be a `Reference`

### `PutStmt`

- None

### `StreamNum`

- (Type restriction) Must be an int-type expr

### `PutItem`

- None

### `PutOpt`

- (Type restriction) Must be an int-type expr
- (???) Opt must make sense for the type passed in

### `GetStmt`

- None

### `GetItem`

- None

### `GetWidth`

- (Type restriction) Must be an int-type expr

### `ReadStmt`

- None (handled by `BinaryIO`)

### `WriteStmt`

- None (handled by `BinaryIO`)

### `BinaryIO`

- `status` must be a `Reference`
- (Type Restriction) `status` must be a reference to an int-class variable

### `BinaryItem`

- None

### `RequestSize`

- Unknown type restrictions

### `ActualSize`

- expr must be a `Reference`

### `SeekStmt`

- `file_ref` must be a `Reference`
- (Type restriction) `seek_to` must be an int-class expression

### `TellStmt`

- `file_ref` must be a `Reference`
- `tell_to` must be a `Reference`

### `ForStmt`

- `ForBounds` must be a whole range if `'decreasing'` is present

### `ForBounds`

- (Type Restriction) `from` and `to` must be of compatible types
- (Type Restriction) `from` and `to` must be an indexable type

### `StepBy`

- (Type Restriction) `change_by` must be of a compatible type to `ForBounds`'s exprs

### `LoopStmt`

- None

### `ExitStmt`

- (Type Restriction) `condition` must be a boolean expr

### `IfStmt`

- None

### `IfBody`

- (Type Restriction) `condition` must be a boolean expr

### `CaseStmt`

- (Type Restriction) expr must be an indexable type
- At least one `CaseArm` must be present
- If there is only one `CaseArm`, the `CaseArm`'s `select` must be present

### `CaseArm`

- None

### `BlockStmt`

- (Type Restriction) `select` exprs must be indexable types
- (Type Restriction) `select` exprs must be compatable with the parent `CaseStmt`'s expr

### `InvariantStmt`

- (Type Restriction) `condition` must be a boolean expr
- Only allowed in `ForStmt`, `LoopStmt`, `ModuleDecl`, `MonitorDecl`, or `ClassDecl`

### `AssertStmt`

- (Type Restriction) `condition` must be a boolean expr

### `CallStmt`

- expr must be a `Reference`

### `ReturnStmt`

- Rejected in `FcnDecl`
- (Semantic Restriction) Rejected in function-kind blocks
  > `BodyDecl` has an unknown kind, so checking is copied over to semantic passes
- Rejected in the top level of module-kind blocks

### `ResultStmt`

- Allowed in `FcnDecl` or function-kind blocks
- (Semantic Restriction) Allowed in function-kind blocks
  > `BodyDecl` has an unknown kind, so checking is copied over to semantic passes
- Rejected in all other places not specified
- (Type Restriction) expr must be a compatible type with the `FcnDecl`'s return value

### `NewStmt`

- If only 1 expr is present:
  - expr must be a `Reference`

> ??? (Type Restriction)Deal with collection & class specializations, as well as
> flexible array expansions

### `FreeStmt`

- If only 1 expr is present:
  - expr must be a `Reference`
- If only 2 exprs are present:
  - specialize_to must be a `Reference`
  - expr must be a `Reference`
- Any excess number of exprs are rejected

### `TagStmt`

- First expr must be a `Reference`
- (Type Restriction) First expr must refer to a union variable
- (Type Restriction) Second expr must be compatible with the union variable's tag type

### `ForkStmt`

- expr must be a `Reference`
- (Type Restriction) expr must refer to a `Process` type
- (Type Restriction) `ParamList` must be compatible with the `Process`'s `ParamSpec`

### `ForkStatus`

- expr must be a `Reference`
- (Type Restriction) expr must refer to a boolean variable

### `StackSize`

- (Type Restriction) expr must be an integer expr

### `ProcessDesc`

- expr must be a `Reference`
- (Type Restriction) expr must refer to an addressint variable

### `SignalStmt`

- expr must be a `Reference`
- (Type Restriction) expr must refer to a `Condition` type

### `WaitStmt`

- First expr must be a `Reference`
- (Type Restriction) First expr must refer to a `Condition` type
- (Type Restriction) `wait_arg` must be an integer expr
- (Type Restriction) `wait_arg` must be present only if reference is to a priority condition,
  or a timeout condition

### `PauseStmt`

- (Type Restriction) expr must be an integer expr

### `QuitStmt`

- (Type Restriction) `quit_code` must be an integer expr

### `QuitCause`

- None

### `BreakStmt`

- None

### `CheckednessStmt`

- None

### `PreStmt`

- Rejected everywhere, unless otherwise specified
- (Type Restriction) `condition` must be a boolean expr

### `InitStmt`

- Rejected everywhere, unless otherwise specified

### `InitVar`

- None

### `PostStmt`

- Rejected everywhere, unless otherwise specified
- (Type Restriction) `condition` must be a boolean expr

### `HandlerStmt`

- Rejected everywhere, unless otherwise specified

### `InheritStmt`

- Rejected everywhere, unless otherwise specified

### `ImplementStmt`

- Rejected everywhere, unless otherwise specified

### `ImplementByStmt`

- Rejected everywhere, unless otherwise specified

### `ImportStmt`

- Rejected everywhere, unless otherwise specified

### `ImportList`

- None

### `ImportItem`

- `ForwardAttr` is only allowed if `ImportList` is part of a `ForwardDecl`

### `ExportStmt`

- Rejected everywhere, unless otherwise specified
- If an `AllItem` is present, all other items must be rejected

### `ExportItem`

- None

### `ExternalItem`

- `'string_literal'` or `'in'` are only allowed if the `ExternalItem`:
  - is part of the main `ImportStmt`, or
  - is part of the top level decl's `ImportStmt`

### `LiteralExpr`

- None (lowering indicates that the thing should be valid)

### `ObjClassExpr`

- expr must be a `Reference`
- (Type Restriction) expr must refer to a `Class` type

### `InitExpr`

- Rejected everywhere else (unless otherwise specified)

### `NilExpr`

- expr must be a `Reference`
- (Type Restriction) expr must refer to a `Collection` or `Class` type

### `SizeOfExpr`

- `size_for` must be either an `Expr` or `PrimType`

### `BinaryExpr`

- (Type Restriction) `lhs` & `rhs` must be of compatibles types with the given operator

### `UnaryExpr`

- (Type Restriction) `rhs` must be a compatible types with the given operator

### `ParenExpr`

- None

### `NameExpr`

- expr must be a `Reference`

### `SelfExpr`

- Only allowed in class blocks, but not outside a subprogram decl

### `FieldExpr`

- expr must be a `Reference`
- (Type Restriction) `Name` must be a field present in the reference type

### `DerefExpr`

- expr must be a `Reference`
- (Type Restriction) expr must be a `Pointer` type

### `CheatExpr`

- (Semantic Restriction) If `SizeSpec` is present, `CheatExpr` must not be treated as a `Reference`
- (Semantic Restriction) If expr is not a `Reference`, `CheatExpr` must not be treated as a
  `Reference`

### `SizeSpec`

- (Semantic Restriction) expr must be constant evaluable
- (Type Restriction) expr must be an integer type
- (Type Restriction) expr evaluation must be compatible with the `CheatExpr`'s type
  - For int-class and nat-class: 1, 2, 4 are allowed values
  - For real-class: 4, 8 are allowed values

### `NatCheatExpr`

- (Semantic Restriction) If expr is not a `Reference`, `CheatExpr` must not be treated as a
  `Reference`

### `ArrowExpr`

- expr must be a `Reference`
- (Type Restriction) `Name` must be a field present in the reference type

### `IndirectExpr`

- `indirect_ty` must be a `PrimType` or a `NameType`
- (Type Restriction) expr must be an addressint expr

### `BitsExpr`

- (Semantic Restriction) If expr is not a `Reference`, `CheatExpr` must not be treated as a
  `Reference`

### `CallExpr`

- expr must be a `Reference`
- (Type Restriction) `ParamList` must be compatible with the callable type's `ParamSpec`

### `ParamList`

- None

### `Param`

- None

### `AllItem`

- (Type Restriction) Only allowed if the callable type is of a `Set` type

### `RangeItem`

- (Type Restriction) Only allowed if the callable type is of a string-class type

### `RelativeBound`

- (Type Restriction) Only allowed if the callable type is of a string-class type

### `PrimType`

- None

### `SizedCharType`

- None

### `SizedStringType`

- None

### `SeqLength`

- `'*'` is allowed if part of a ConstVarParam

### `NameType`

- None

### `RangeType`

- None

### `RangeSpec`

- `end` is only allowed to be `'*'` if `RangeSpec` is part of an array in
  a var decl, or a `ParamDecl`

### `EnumType`

- Only allowed in `TypeDecl`

### `ArrayType`

- Only allowed in a var decl if:
  - `'flexible'` is present
  - Any range in `ranges` has a `'*'` bound

### `RangeList`

- (Type Restriction) ranges must be indexable types
- (Semantic Restriction) ranges must be constant evaluable if not in a var decl

### `SetType`

- Only allowed in `TypeDecl`

### `RecordType`

- Must have at least one `RecordField` present

### `RecordField`

- None

### `UnionType`

- Must have at least one `UnionVariant` present
- If only one `UnionVariant` is present, `UnionVariant`'s `selector` is required to be present
- (Type Restriction) `range_ty` must be an indexable type

### `UnionVariant`

- None

### `PointerType`

- None

### `FcnType`

- `ParamSpec` must always be present, even if empty

### `ProcType`

- None

### `CollectionType`

- Only allowed in a var decl

### `ConditionType`

- Only allowed in a var decl inside of a monitor-kind block

### `ConditionKind`

- None

### `Checkedness`

- None

### `ConstVarParam`

- None
