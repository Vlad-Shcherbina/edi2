use once_cell::unsync::OnceCell;
use fnv::{FnvHashMap, FnvHashSet};
use crate::text_layout::TextLayout;
use crate::slotmap::SlotMap;

new_key_type!(pub NodeKey);
pub type Nodes = SlotMap<NodeKey, Node>;
new_key_type!(pub BlockKey);
pub type Blocks = SlotMap<BlockKey, Block>;
new_key_type!(pub CBlockKey);
pub type CBlocks = SlotMap<CBlockKey, CBlock>;

#[derive(Debug, Clone)]
pub enum Line {
    Text {
        text: String,
        monospace: bool,
    },
    Node {
        local_header: String,
        node: NodeKey,
    }
}

pub struct LineWithLayout {
    pub line: Line,
    pub layout: OnceCell<TextLayout>,
}

impl LineWithLayout {
    pub fn new(line: Line) -> Self {
        LineWithLayout {
            line,
            layout: OnceCell::new(),
        }
    }
}

impl std::fmt::Debug for LineWithLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LineWithLayout")
            .field("line", &self.line)
            .finish()
    }
}

pub struct Node {
    pub lines: Vec<LineWithLayout>,
    pub blocks: FnvHashSet<BlockKey>,
    pub cblocks: FnvHashSet<CBlockKey>,
    pub parents: FnvHashMap<NodeKey, u32>,  // multiset
    pub db_key: i64,
}

pub struct Block {
    pub depth: i16,
    pub parent_idx: Option<(BlockKey, usize)>,
    pub node: NodeKey,
    pub children: Vec<BlockChild>,
    pub collapsed: Option<CForest>,  // None if expanded
}

pub enum BlockChild {
    Block(BlockKey),  // owned key
    Leaf,
}

// collapsed forest
// - line numbers start at 1 (same as block children numbers)
// - elems that are .is_trivial() are omitted
pub struct CForest(pub FnvHashMap<usize, CBlockKey>);

// collapsed block
pub struct CBlock {
    pub node: NodeKey,
    pub expanded: bool,
    pub children: CForest,
}

#[derive(Clone, Copy, PartialEq)]
pub enum CBlockParent {
    Block(BlockKey),
    CBlock(CBlockKey),
}

impl CForest {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        CForest(FnvHashMap::default())
    }

    pub fn check(&self, node: &Node, cblocks: &CBlocks) {
        for &v in self.0.values() {
            assert!(!cblocks[v].is_trivial());
        }
        for (i, line) in node.lines.iter().enumerate() {
            match line.line {
                Line::Text {..} => assert!(!self.0.contains_key(&(i + 1))),
                Line::Node { node, ..} => {
                    if let Some(&child_cblock) = self.0.get(&(i + 1)) {
                        assert_eq!(cblocks[child_cblock].node, node);
                    }
                }
            }
        }
    }
}

impl CBlock {
    pub fn is_trivial(&self) -> bool {
        !self.expanded && self.children.0.is_empty()
    }
}

pub fn check_cblock(
    cblock: CBlockKey,
    cblocks: &CBlocks, nodes: &Nodes,
) {
    let cb = &cblocks[cblock];
    cb.children.check(&nodes[cb.node], cblocks);
}

pub fn check_block(
    block: BlockKey,
    blocks: &Blocks, cblocks: &CBlocks, nodes: &Nodes,
) {
    let b = &blocks[block];
    match b.parent_idx {
        Some((parent, idx)) => {
            assert_eq!(blocks[parent].depth + 1, blocks[block].depth);
            match blocks[parent].children[idx] {
                BlockChild::Leaf => panic!(),
                BlockChild::Block(b) => assert_eq!(b, block),
            }
        }
        None => assert_eq!(b.depth, 0),
    }

    assert!(nodes[b.node].blocks.contains(&block));

    match b.children[0] {
        BlockChild::Leaf => {}
        _ => panic!(),
    }    
    if b.is_expanded() {
        assert_eq!(b.children.len(), 1 + nodes[b.node].lines.len());
        for (child, line) in b.children[1..].iter().zip(&nodes[b.node].lines) {
            match (child, &line.line) {
                (BlockChild::Leaf, Line::Text {..}) => {}
                (&BlockChild::Block(b), &Line::Node { node, .. }) =>
                    assert_eq!(blocks[b].node, node),
                _ => panic!(),
            }
        }
    } else {
        assert_eq!(b.children.len(), 1);
        b.collapsed.as_ref().unwrap().check(&nodes[b.node], cblocks);
    }
}

type BlockChildRef = (BlockKey, usize);

impl Line {
    pub fn new_empty() -> Self {
        Line::Text {
            text: String::new(), monospace: false,
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Line::Text { text, .. } => text.is_empty(),
            Line::Node { .. } => false,
        }
    }

    pub fn text(&self) -> &str {
        match self {
            Line::Text { text, .. } => text,
            Line::Node { local_header, .. } => local_header,
        }
    }
    
    pub fn text_mut(&mut self) -> &mut String {
        match self {
            Line::Text { text, .. } => text,
            Line::Node { local_header, .. } => local_header,
        }
    }

    pub fn slice(&self, start: usize, end: usize) -> Line {
        match *self {
            Line::Text { ref text, monospace } => Line::Text {
                text: text[start..end].to_owned(),
                monospace,
            },
            Line::Node { ref local_header, ..} => Line::Text {
                text: local_header[start..end].to_owned(),
                monospace: false,
            },
        }
    }
}

impl Block {
    pub fn is_expanded(&self) -> bool {
        self.collapsed.is_none()
    }

    pub fn node_line_idx(&self, idx: usize, blocks: &Blocks) -> Option<(NodeKey, usize)> {
        match self.children[idx] {
            BlockChild::Leaf =>
                if idx == 0 {
                    if let Some((parent, idx)) = self.parent_idx {
                        let parent = &blocks[parent];
                        assert!(idx > 0);
                        Some((parent.node, idx - 1))
                    } else {
                        None  // header of the root block
                    }
                } else {
                    Some((self.node, idx - 1))
                }
            BlockChild::Block(_) => panic!("not a leaf"),
        }
    }
}

pub fn last_leaf(block: BlockKey, blocks: &Blocks) -> BlockChildRef {
    let b = &blocks[block];
    match *b.children.last().unwrap() {
        BlockChild::Leaf => {
            let idx = b.children.len() - 1;
            (block, idx)
        }
        BlockChild::Block(b) => last_leaf(b, blocks),
    }
}

pub fn prev_leaf(
    (mut block, mut idx): BlockChildRef,
    blocks: &Blocks,
) -> Option<BlockChildRef> {
    loop {
        let b = &blocks[block];

        if idx == 0 {
            match b.parent_idx {
                Some((parent_block, i)) => {
                    block = parent_block;
                    idx = i;
                    continue;
                }
                None => return None,
            }
        }

        return match b.children[idx - 1] {
            BlockChild::Block(block) => {
                Some(last_leaf(block, blocks))
            }
            BlockChild::Leaf => {
                if idx == 1 && b.depth == 0 {
                    None  // don't return header of the root block
                } else {
                    Some((block, idx - 1))
                }
            }
        }
    }
}

pub fn next_leaf(
    (mut block, mut idx): BlockChildRef,
    blocks: &Blocks,
) -> Option<BlockChildRef> {
    loop {
        let b = &blocks[block];

        if idx + 1 == b.children.len() {
            match b.parent_idx {
                Some((parent_block, i)) => {
                    block = parent_block;
                    idx = i;
                    continue;
                }
                None => return None,
            }
        }

        return match b.children[idx + 1] {
            BlockChild::Block(block) => {
                match blocks[block].children[0] {
                    BlockChild::Leaf => {},
                    BlockChild::Block {..} => panic!(),
                }
                Some((block, 0))
            }
            BlockChild::Leaf => {
                Some((block, idx + 1))
            }
        }
    }
}

pub fn reachable_from(root: NodeKey, node: NodeKey, nodes: &Nodes) -> bool {
    let mut visited = FnvHashSet::default();
    visited.insert(node);
    let mut q = vec![node];
    while let Some(u) = q.pop() {
        if u == root {
            return true;
        }
        for &p in nodes[u].parents.keys() {
            if !visited.contains(&p) {
                visited.insert(p);
                q.push(p);
            }
        }
    }
    false
}