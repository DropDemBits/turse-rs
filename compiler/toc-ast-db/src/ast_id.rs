//! Based on <https://github.com/rust-lang/rust-analyzer/blob/5b852da4c3852d030ebca67746980bc0288a85ec/crates/span/src/ast_id.rs>
use std::{any::type_name, marker::PhantomData};

use la_arena::Arena;
use rustc_hash::{FxBuildHasher, FxHashMap};
use toc_syntax::{
    AstPtr, SyntaxKind, SyntaxNode, SyntaxNodePtr,
    ast::{self, AstNode},
};

mod hash_impls;

pub const ROOT_ERASED_AST_ID: ErasedAstId =
    ErasedAstId(pack_erased_id(0, 0, AstIdKind::Root as u32));

/// Any node that always has a stable id.
pub trait AstIdNode: ast::AstNode<Language = toc_syntax::Lang> {}

impl AstIdNode for ast::Source {}

/// Any node that sometimes has a stable id.
pub trait MaybeAstIdNode: ast::AstNode<Language = toc_syntax::Lang> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u32)]
#[allow(unused)]
enum AstIdKind {
    Type,
    Record,
    Union,
    Set,
    Enum,
    EnumVariant,
    UnionVariant,
    /// Refers to an individual record field name.
    RecordField,
    /// Refers to an individual import item.
    ImportItem,
    /// Refers to an individual export item.
    ExportItem,
    /// Normal procedure, function, or process.
    /// Differences are encoded in the hash.
    FunctionLike,
    ExternalFunctionLike,
    ExternalVar,
    /// Encodes function vs procedure vs etc. in the hash
    ForwardFunctionLike,
    DeferredFunctionLike,
    BodyFunctionLike,
    // Module-like items
    /// Module and monitor modules.
    ModuleDecl,
    /// Classes and monitor classes.
    ClassDecl,
    // These are only items at module scope
    /// Refers to an individual [`ast::ConstVarDeclName`].
    ConstVar,
    BindItem,
    // Associated with some [`ast::StmtList`].
    StmtList,
    // TODO: include "preprocessor" macros
    // Root node of a file (unit, package root)
    Root,
}

/// Erased stable reference to an AST node.
///
/// Stored as a compreseed index of:
/// | 27..=31 | 16..=26  | 0..=15 |
/// |---------|----------|--------|
/// | Kind    | Instance | Hash   |
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::Update)]
#[repr(transparent)]
pub struct ErasedAstId(u32);

impl ErasedAstId {
    #[inline]
    fn hash_value(self) -> u16 {
        self.0 as u16
    }

    #[inline]
    fn instance(self) -> u32 {
        (self.0 << KIND_BITS) >> (KIND_BITS + HASH_BITS)
    }

    #[inline]
    fn kind(self) -> u32 {
        self.0 >> (HASH_BITS + INSTANCE_BITS)
    }

    fn ast_id_for(
        node: &SyntaxNode,
        id_gen: &mut AstIdGen,
        parent: Option<ErasedAstId>,
    ) -> Option<ErasedAstId> {
        hash_impls::named_ast_id(node, id_gen)
            .or_else(|| hash_impls::assoc_ast_id(node, id_gen, parent))
            .or_else(|| hash_impls::maybe_assoc_ast_id(node, id_gen, parent))
    }

    /// Whether a given node might allocate a stable id
    fn might_alloc(node: &SyntaxNode) -> bool {
        hash_impls::might_alloc_named(node)
            || hash_impls::might_alloc_assoc(node)
            || ast::ConstVarDeclName::can_cast(node.kind())
            || ast::BindItem::can_cast(node.kind())
    }

    /// Whether a maybe allocated node will or won't be allocated
    fn maybe_might_alloc(node: &SyntaxNode, has_items: HasItems) -> bool {
        if ast::ConstVarDeclName::can_cast(node.kind()) || ast::BindItem::can_cast(node.kind()) {
            // Bind & ConstVar items must be at the top level of a module like to be allocated.
            return has_items == HasItems::ModuleLike;
        } else {
            // Another type of item.
            return true;
        }
    }

    #[inline]
    pub const fn to_raw(self) -> u32 {
        self.0
    }

    #[inline]
    pub const fn from_raw(raw: u32) -> Self {
        Self(raw)
    }
}

impl std::fmt::Debug for ErasedAstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind = self.kind() as u32;
        macro_rules! kind {
            ($($kind:ident),* $(,)?) => {

                if false {
                    // Ensure we're handling all of the variants.
                    const _: () = {
                        match AstIdKind::Root {
                            $(AstIdKind::$kind => (),)*
                        }
                    };

                    unreachable!()
                }
                $(else if kind == AstIdKind::$kind as u32 {
                    stringify!($kind)
                })*
                else {
                    "Unknown"
                }
            };
        }
        let kind = kind!(
            Type,
            Record,
            Union,
            Set,
            Enum,
            EnumVariant,
            UnionVariant,
            RecordField,
            ImportItem,
            ExportItem,
            FunctionLike,
            ExternalFunctionLike,
            ExternalVar,
            ForwardFunctionLike,
            DeferredFunctionLike,
            BodyFunctionLike,
            ModuleDecl,
            ClassDecl,
            ConstVar,
            BindItem,
            StmtList,
            Root,
        );

        if f.alternate() {
            write!(f, "{kind}[{:04X}, {}]", self.hash_value(), self.instance())
        } else {
            f.debug_struct("ErasedAstId")
                .field("kind", &format_args!("{kind}"))
                .field("hash", &format_args!("{:04X}", self.hash_value()))
                .field("instance", &self.instance())
                .finish()
        }
    }
}

const HASH_BITS: u32 = 16;
/// Disambiguates between stable ids that have the same kind & hash.
const INSTANCE_BITS: u32 = 11;
/// High bit is reserved for distinguishing between items scopes & expression scopes. ?
const KIND_BITS: u32 = 5;

const fn pack_erased_id(hash: u16, instance: u32, kind: u32) -> u32 {
    (hash as u32) | (instance << HASH_BITS) | (kind << (HASH_BITS + INSTANCE_BITS))
}

const _: () = assert!(
    (AstIdKind::Root as u32).ilog2() < KIND_BITS,
    "variants exceeds kind bits"
);
const _: () = assert!(KIND_BITS + INSTANCE_BITS + HASH_BITS == u32::BITS);

/// Compresses a u64 hash into the space of a u16.
/// From <https://github.com/rust-lang/rust-analyzer/blob/5b852da4c3852d030ebca67746980bc0288a85ec/crates/span/src/ast_id.rs#L157-L172>,
/// with revised wording.
#[inline]
const fn compress_u16_hash(hash: u64) -> u16 {
    // This is essentially the same as `FxHasher` but const-evaluable.
    // Since we use all of the bits, using rustc-hash + truncating isn't a good
    // strategy, as it rotates the hash to transfer entropy from higher bits to
    // lower (as is used in hash maps, which use the lower bits first).
    const K: u16 = 0xecc5;
    let (part1, part2, part3, part4) = (
        hash as u16,
        (hash >> 16) as u16,
        (hash >> 32) as u16,
        (hash >> 48) as u16,
    );
    part1
        .wrapping_add(part2)
        .wrapping_mul(K)
        .wrapping_add(part3)
        .wrapping_mul(K)
        .wrapping_add(part4)
        .wrapping_mul(K)
}

/// Stable reference to an ast node in a source file.
pub struct AstId<N> {
    erased: ErasedAstId,
    // `to_node` is what yields the actual AST node, rather than a `SemanticLoc`
    // owning the AST node. This also allows `SemanticLoc` to be `Send + Sync`.
    _node: PhantomData<fn() -> N>,
}

impl<N> AstId<N> {
    /// Upcast into a compatible supertype id.
    #[inline]
    pub fn upcast<M: AstIdNode>(self) -> AstId<M>
    where
        N: Into<M>,
    {
        AstId {
            erased: self.erased(),
            _node: PhantomData,
        }
    }

    #[inline]
    pub fn erased(self) -> ErasedAstId {
        self.erased
    }
}

impl<N> Clone for AstId<N> {
    fn clone(&self) -> Self {
        Self {
            erased: self.erased.clone(),
            _node: PhantomData,
        }
    }
}

impl<N> Copy for AstId<N> {}

impl<N> PartialEq for AstId<N> {
    fn eq(&self, other: &Self) -> bool {
        self.erased == other.erased
    }
}

impl<N> Eq for AstId<N> {}

impl<N> std::hash::Hash for AstId<N> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.erased.hash(state);
    }
}

impl<N> std::fmt::Debug for AstId<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AstId::<{}>({:?})", type_name::<N>(), self.erased)
    }
}

#[derive(Default)]
struct AstIdGen(FxHashMap<(AstIdKind, u16), u32>);

impl AstIdGen {
    fn next(&mut self, kind: AstIdKind, data: impl std::hash::Hash) -> ErasedAstId {
        use std::hash::BuildHasher;
        let hash = FxBuildHasher.hash_one(&data);
        let initial_hash = compress_u16_hash(hash);
        let mut hash = initial_hash;

        // Find instance index, adjusting hash if we run out of items
        let instance = loop {
            match self.0.entry((kind, hash)) {
                std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                    let i = occupied_entry.get_mut();

                    // 2^11 (2048) should be enough bits for items.
                    if *i < (1 << INSTANCE_BITS) - 1 {
                        *i += 1;
                        break *i;
                    }
                }
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(0);
                    break 0;
                }
            }

            hash = hash.wrapping_add(1);

            if hash == initial_hash {
                panic!("too many items in the same file: your too long")
            }
        };

        let kind = kind as u32;
        ErasedAstId(pack_erased_id(hash, instance, kind))
    }
}

#[derive(Default)]
pub struct AstIdMap {
    entries: Arena<(SyntaxNodePtr, ErasedAstId)>,
    constvar_to_decl: FxHashMap<ArenaIdx, SyntaxNodePtr>,

    from_ptr: hashbrown::HashTable<ArenaIdx>,
    from_id: hashbrown::HashTable<ArenaIdx>,
}

impl PartialEq for AstIdMap {
    fn eq(&self, other: &Self) -> bool {
        self.entries == other.entries
    }
}

impl Eq for AstIdMap {}

impl std::fmt::Debug for AstIdMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AstIdMap")
            .field("entries", &self.entries)
            .finish()
    }
}

type ArenaIdx = la_arena::Idx<(SyntaxNodePtr, ErasedAstId)>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HasItems {
    ModuleLike,
    Yes,
    No,
}

impl AstIdMap {
    pub fn from_source(root: &SyntaxNode) -> Self {
        assert!(root.parent().is_none());
        let mut id_gen = AstIdGen::default();
        let mut map = AstIdMap::default();

        map.entries
            .alloc((SyntaxNodePtr::new(root), ROOT_ERASED_AST_ID));

        // Block stack to keep track of whether or not to allocate a stable id for a given block.
        let mut blocks = vec![];

        // We walk the tree in topo order to ensure that parent nodes have lower ids
        // than their respective children nodes, which helps with stability.
        topo_visit(root, |event, parent_idx| {
            match event {
                toc_syntax::WalkEvent::Enter(node) => {
                    if ast::StmtList::can_cast(node.kind()) {
                        let has_items = if node
                            .parent()
                            .is_some_and(|parent| parent.kind().is_module_like())
                        {
                            HasItems::ModuleLike
                        } else {
                            HasItems::No
                        };

                        // add to block stack
                        blocks.push((SyntaxNodePtr::new(&node), has_items));
                        return InLayer::Same;
                    }

                    if !ErasedAstId::might_alloc(&node) {
                        // Find deeper items to explore
                        return InLayer::Same;
                    }

                    let parent = parent_idx.map(|it| map.index_to_id(it));

                    if let Some((block_ptr, has_items)) = blocks.last_mut() {
                        if !ErasedAstId::maybe_might_alloc(&node, *has_items) {
                            // maybe-allocated node can't be allocated, so skip it and its children entirely.
                            return InLayer::Skip;
                        } else if *has_items == HasItems::No {
                            // Only allocate blocks if they contain items, if they themselves aren't owned by items.
                            //
                            // For better stability, we never anchor items to blocks and instead to the containing item.
                            let block_ast_id = hash_impls::block_ast_id(
                                block_ptr.to_node(&root),
                                &mut id_gen,
                                parent,
                            )
                            .expect("must be a block node");
                            map.entries.alloc((*block_ptr, block_ast_id));

                            *has_items = HasItems::Yes;
                        }
                    }

                    let syntax_ptr = SyntaxNodePtr::new(&node);
                    let ast_id = ErasedAstId::ast_id_for(&node, &mut id_gen, parent)
                        .expect("node must have an ast id");
                    let idx = map.entries.alloc((syntax_ptr, ast_id));

                    if syntax_ptr.kind() == SyntaxKind::ConstVarDeclName
                        && let node = syntax_ptr.to_node(&root)
                        && let Some(constvar_decl) =
                            node.ancestors().find_map(ast::ConstVarDecl::cast)
                    {
                        let constvar_ptr = SyntaxNodePtr::new(constvar_decl.syntax());
                        map.constvar_to_decl.insert(idx, constvar_ptr);
                    }

                    InLayer::Next(idx)
                }
                toc_syntax::WalkEvent::Leave(node) => {
                    if ast::StmtList::can_cast(node.kind()) {
                        assert_eq!(
                            blocks.pop().map(|it| it.0),
                            Some(SyntaxNodePtr::new(&node)),
                            "left a block statement that was never entered"
                        )
                    }

                    InLayer::Same
                }
            }
        });
        assert!(blocks.is_empty(), "remaining block statements in stack");

        map.from_ptr = hashbrown::HashTable::with_capacity(map.entries.len());
        map.from_id = hashbrown::HashTable::with_capacity(map.entries.len());

        // Build ptr <-> id bidi mappings
        for (index, (ptr, id)) in map.entries.iter() {
            let ptr_hash = Self::hash_ptr(ptr);
            let id_hash = Self::hash_id(id);

            // ptr -> index
            if let hashbrown::hash_table::Entry::Vacant(vacant_entry) = map.from_ptr.entry(
                ptr_hash,
                |other| index == *other,
                |index| Self::hash_ptr(&map.entries[*index].0),
            ) {
                vacant_entry.insert(index);
            } else {
                unreachable!()
            }

            // id -> index
            if let hashbrown::hash_table::Entry::Vacant(vacant_entry) = map.from_id.entry(
                id_hash,
                |other| index == *other,
                |index| Self::hash_id(&map.entries[*index].1),
            ) {
                vacant_entry.insert(index);
            } else {
                unreachable!()
            }
        }

        map.entries.shrink_to_fit();

        map
    }

    pub fn root(&self) -> AstId<ast::Source> {
        let erased = self.index_to_id(ArenaIdx::from_raw(la_arena::RawIdx::from_u32(0)));

        AstId {
            erased,
            _node: PhantomData,
        }
    }

    pub fn lookup<N: AstIdNode>(&self, node: &N) -> AstId<N> {
        self.lookup_for_ptr(AstPtr::new(node))
    }

    /// Looks up the [`AstId`] for an AST node that may not always have a stable AST id.
    pub fn lookup_for_maybe<N: MaybeAstIdNode>(&self, node: &N) -> Option<AstId<N>> {
        let erased_ptr = SyntaxNodePtr::new(node.syntax());
        let erased = self.try_lookup_erased_id(erased_ptr)?;

        Some(AstId {
            erased,
            _node: PhantomData,
        })
    }

    pub fn lookup_for_ptr<N: AstIdNode>(&self, ptr: AstPtr<N>) -> AstId<N> {
        let erased_ptr = ptr.syntax_node_ptr();

        let erased_id = self.lookup_erased_id(erased_ptr);

        AstId {
            erased: erased_id,
            _node: PhantomData,
        }
    }

    pub fn get<N: AstNode<Language = toc_syntax::Lang>>(&self, id: AstId<N>) -> AstPtr<N> {
        let ptr = self.get_for_erased(id.erased);
        let Some(ptr) = AstPtr::try_from_raw(ptr) else {
            panic!("can't find AstId {id:?} in map")
        };

        ptr
    }

    pub fn get_for_erased(&self, erased_id: ErasedAstId) -> SyntaxNodePtr {
        let Some(idx) = self.from_id.find(Self::hash_id(&erased_id), |idx| {
            self.entries[*idx].1 == erased_id
        }) else {
            panic!(
                "can't find ErasedAstId {erased_id:?} in map:\n{:?}",
                self.entries.iter().map(|(_, it)| it).collect::<Vec<_>>()
            );
        };

        self.entries[*idx].0
    }

    fn lookup_erased_id(&self, erased_ptr: SyntaxNodePtr) -> ErasedAstId {
        self.try_lookup_erased_id(erased_ptr).unwrap_or_else(|| {
            panic!(
                "can't find SyntaxNodePtr {erased_ptr:?} in map:\n{:?}",
                self.entries.iter().map(|(_, it)| it).collect::<Vec<_>>()
            )
        })
    }

    fn try_lookup_erased_id(&self, erased_ptr: SyntaxNodePtr) -> Option<ErasedAstId> {
        let idx = self.from_ptr.find(Self::hash_ptr(&erased_ptr), |idx| {
            self.entries[*idx].0 == erased_ptr
        })?;
        Some(self.entries[*idx].1)
    }

    fn index_to_id(&self, index: ArenaIdx) -> ErasedAstId {
        self.entries[index].1
    }

    fn hash_ptr(ptr: &SyntaxNodePtr) -> u64 {
        use std::hash::BuildHasher;
        FxBuildHasher.hash_one(ptr)
    }

    fn hash_id(id: &ErasedAstId) -> u64 {
        use std::hash::BuildHasher;
        FxBuildHasher.hash_one(id)
    }
}

enum InLayer<V> {
    /// Children will be visited in the next layer iteration with the provided parent value.
    Next(V),
    /// Children will be visited in the same layer as the current node.
    Same,
    /// Childeren will not be visited in the same layer as the current node.
    Skip,
}

/// Walks the subtree in topological order, calling `filter` for each node.
///
/// Nodes for which `filter` return true are put in the same layer, and
/// the children of those nodes form the candidates for the next layer.
/// All other nodes are explored depth-first.
///
/// The size of the expand queue is bound by the number of filtered nodes.
fn topo_visit<V>(
    node: &SyntaxNode,
    mut filter: impl FnMut(toc_syntax::WalkEvent<SyntaxNode>, Option<V>) -> InLayer<V>,
) where
    V: Copy,
{
    // borrowed from:
    // https://github.com/rust-lang/rust-analyzer/blob/fc848495f45e4741849940a2be437a46b742ce53/crates/hir-expand/src/ast_id_map.rs#L126
    let mut curr_layer = vec![(node.clone(), None)];
    let mut next_layer = vec![];
    while !curr_layer.is_empty() {
        curr_layer.drain(..).for_each(|(node, parent_value)| {
            let mut preorder = node.preorder();
            while let Some(event) = preorder.next() {
                match event {
                    toc_syntax::WalkEvent::Enter(node) => {
                        match filter(toc_syntax::WalkEvent::Enter(node.clone()), parent_value) {
                            InLayer::Next(child_value) => {
                                next_layer
                                    .extend(node.children().map(|it| (it, Some(child_value))));
                                preorder.skip_subtree();
                            }
                            InLayer::Same => {
                                //
                            }
                            InLayer::Skip => {
                                preorder.skip_subtree();
                            }
                        }
                    }
                    leave @ toc_syntax::WalkEvent::Leave(_) => _ = filter(leave, parent_value),
                }
            }
        });
        std::mem::swap(&mut curr_layer, &mut next_layer);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn module_inner_block_is_not_allocated() {
        let root = toc_parser::parse(
            r"
            module abc end abc
        ",
        )
        .take()
        .0
        .syntax();

        let Some(mod_abc) = root.descendants().find_map(ast::ModuleDecl::cast) else {
            panic!("missing `module` item")
        };
        let Some(block) = mod_abc.stmt_list() else {
            panic!("missing stmt list")
        };

        let map = AstIdMap::from_source(&root);
        let abc_block_id = map.lookup_for_maybe(&block);

        assert_eq!(
            abc_block_id, None,
            "module inner block has an id {abc_block_id:#?}"
        );
    }

    #[test]
    fn different_names_are_different_hashes() {
        let root = toc_parser::parse(
            r"
            module abc end abc
            module def end def
        ",
        )
        .take()
        .0
        .syntax();

        let mods = root
            .descendants()
            .flat_map(ast::ModuleDecl::cast)
            .collect::<Vec<_>>();
        let [abc_mod, def_mod] = mods.as_slice() else {
            panic!("not exactly 2 modules")
        };

        let map = AstIdMap::from_source(&root);
        let abc_id = map.lookup(abc_mod).erased();
        let def_id = map.lookup(def_mod).erased();

        assert_ne!(abc_id.hash_value(), def_id.hash_value(), "hashes are equal");
    }

    #[test]
    fn same_names_are_disambiguated() {
        let root = toc_parser::parse(
            r"
            module uwu end uwu
            module uwu end uwu
        ",
        )
        .take()
        .0
        .syntax();

        let mods = root
            .descendants()
            .flat_map(ast::ModuleDecl::cast)
            .collect::<Vec<_>>();
        let [uwu1_mod, uwu2_mod] = mods.as_slice() else {
            panic!("not exactly 2 modules")
        };

        let map = AstIdMap::from_source(&root);
        let uwu1_id = map.lookup(uwu1_mod).erased();
        let uwu2_id = map.lookup(uwu2_mod).erased();

        assert_eq!(
            uwu1_id.hash_value(),
            uwu2_id.hash_value(),
            "name hashes are not the same"
        );
        assert_ne!(
            uwu1_id.instance(),
            uwu2_id.instance(),
            "instances must be different"
        );
    }

    #[test]
    fn constvar_in_root_is_allocated() {
        let root = toc_parser::parse(&format!(
            r"
            const a, b, c : int := 1
            var d, e, f : int
            "
        ))
        .take()
        .0
        .syntax();

        let the_const = root
            .descendants()
            .find_map(|it| ast::ConstVarDecl::cast(it).filter(|it| it.const_token().is_some()))
            .expect("missing `const` statement");
        let the_var = root
            .descendants()
            .find_map(|it| ast::ConstVarDecl::cast(it).filter(|it| it.var_token().is_some()))
            .expect("missing `var` statement");

        let const_names: Vec<_> = the_const
            .syntax()
            .descendants()
            .flat_map(ast::ConstVarDeclName::cast)
            .collect();
        assert!(!const_names.is_empty(), "missing `const` names");

        let var_names: Vec<_> = the_var
            .syntax()
            .descendants()
            .flat_map(ast::ConstVarDeclName::cast)
            .collect();
        assert!(!var_names.is_empty(), "missing `var` names");

        let map = AstIdMap::from_source(&root);

        for name in &const_names {
            assert_ne!(
                map.lookup_for_maybe(name),
                None,
                "item name `var` {name:?} does not have an id"
            );
        }

        for name in &var_names {
            assert_ne!(
                map.lookup_for_maybe(name),
                None,
                "item name `var` {name:?} does not have an id"
            );
        }
    }

    #[test]
    fn constvar_in_module_top_level_is_allocated() {
        const MODULE_LIKES: &[&str] = &["module", "class", "monitor", "monitor class"];

        for like in MODULE_LIKES {
            let root = toc_parser::parse(&format!(
                r"{like} thing
                    const a, b, c : int := 1
                    var d, e, f : int
                end thing"
            ))
            .take()
            .0
            .syntax();

            let the_const = root
                .descendants()
                .find_map(|it| ast::ConstVarDecl::cast(it).filter(|it| it.const_token().is_some()))
                .expect("missing `const` statement");
            let the_var = root
                .descendants()
                .find_map(|it| ast::ConstVarDecl::cast(it).filter(|it| it.var_token().is_some()))
                .expect("missing `var` statement");

            let const_names: Vec<_> = the_const
                .syntax()
                .descendants()
                .flat_map(ast::ConstVarDeclName::cast)
                .collect();
            assert!(!const_names.is_empty(), "missing `const` names");

            let var_names: Vec<_> = the_var
                .syntax()
                .descendants()
                .flat_map(ast::ConstVarDeclName::cast)
                .collect();
            assert!(!var_names.is_empty(), "missing `var` names");

            let map = AstIdMap::from_source(&root);

            for name in &const_names {
                assert_ne!(
                    map.lookup_for_maybe(name),
                    None,
                    "item name `var` {name:?} does not have an id in `{like}`"
                );
            }

            for name in &var_names {
                assert_ne!(
                    map.lookup_for_maybe(name),
                    None,
                    "item name `var` {name:?} does not have an id in `{like}`"
                );
            }
        }
    }

    #[test]
    fn constvar_in_local_is_not_allocated() {
        const MODULE_LIKES: &[&str] = &["module", "class", "monitor", "monitor class"];

        for like in MODULE_LIKES {
            let root = toc_parser::parse(&format!(
                r"{like} thing
                    begin
                        const a, b, c : int := 1
                        var d, e, f : int
                    end
                end thing"
            ))
            .take()
            .0
            .syntax();

            let the_const = root
                .descendants()
                .find_map(|it| ast::ConstVarDecl::cast(it).filter(|it| it.const_token().is_some()))
                .expect("missing `const` statement");
            let the_var = root
                .descendants()
                .find_map(|it| ast::ConstVarDecl::cast(it).filter(|it| it.var_token().is_some()))
                .expect("missing `var` statement");

            let const_names: Vec<_> = the_const
                .syntax()
                .descendants()
                .flat_map(ast::ConstVarDeclName::cast)
                .collect();
            assert!(!const_names.is_empty(), "missing `const` names");

            let var_names: Vec<_> = the_var
                .syntax()
                .descendants()
                .flat_map(ast::ConstVarDeclName::cast)
                .collect();
            assert!(!var_names.is_empty(), "missing `var` names");

            let map = AstIdMap::from_source(&root);

            for name in &const_names {
                assert_eq!(
                    map.lookup_for_maybe(name),
                    None,
                    "item name `var` {name:?} has an id in `{like}`"
                );
            }

            for name in &var_names {
                assert_eq!(
                    map.lookup_for_maybe(name),
                    None,
                    "item name `var` {name:?} has an id in `{like}`"
                );
            }
        }
    }

    #[test]
    fn bind_in_module_top_level_is_allocated() {
        const MODULE_LIKES: &[&str] = &["module", "class", "monitor", "monitor class"];

        for like in MODULE_LIKES {
            let root = toc_parser::parse(&format!(
                r"{like} thing
                    bind a to b
                end thing"
            ))
            .take()
            .0
            .syntax();

            let Some(bind_item) = root.descendants().find_map(ast::BindItem::cast) else {
                panic!("missing `bind` item")
            };

            let map = AstIdMap::from_source(&root);

            assert_ne!(
                map.lookup_for_maybe(&bind_item),
                None,
                "item `bind` {bind_item:?} does not have an id in `{like}`"
            );
        }
    }

    #[test]
    fn bind_in_local_is_not_allocated() {
        const MODULE_LIKES: &[&str] = &["module", "class", "monitor", "monitor class"];

        for like in MODULE_LIKES {
            let root = toc_parser::parse(&format!(
                r"{like} thing
                    begin
                        bind a to b
                    end
                end thing"
            ))
            .take()
            .0
            .syntax();

            let Some(bind_item) = root.descendants().find_map(ast::BindItem::cast) else {
                panic!("missing `bind` item")
            };

            let map = AstIdMap::from_source(&root);

            assert_eq!(
                map.lookup_for_maybe(&bind_item),
                None,
                "item `bind` {bind_item:?} has an id in `{like}`"
            );
        }
    }

    #[test]
    fn block_with_items_is_allocated() {
        let root = toc_parser::parse(
            r"
            begin
                module a end a
            end
        ",
        )
        .take()
        .0
        .syntax();

        let Some(block) = root
            .descendants()
            .find_map(ast::BlockStmt::cast)
            .and_then(|it| it.stmt_list())
        else {
            panic!("missing block statement")
        };

        let map = AstIdMap::from_source(&root);
        let block_id = map.lookup_for_maybe(&block);

        assert!(block_id.is_some(), "block container id was not allocated");
    }

    #[test]
    fn block_with_no_items_not_allocated() {
        let root = toc_parser::parse(
            r"
            begin
                put 1
            end
        ",
        )
        .take()
        .0
        .syntax();

        let Some(block) = root
            .descendants()
            .find_map(ast::BlockStmt::cast)
            .and_then(|it| it.stmt_list())
        else {
            panic!("missing block statement")
        };

        let map = AstIdMap::from_source(&root);
        let block_id = map.lookup_for_maybe(&block);

        assert!(
            block_id.is_none(),
            "extra AstId {block_id:#?} was allocated"
        );
    }
}
