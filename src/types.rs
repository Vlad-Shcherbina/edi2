use once_cell::unsync::OnceCell;
use crate::text_layout::TextLayout;
use crate::slotmap::SlotMap;

new_key_type!(pub NodeKey);
pub type Nodes = SlotMap<NodeKey, Node>;
new_key_type!(pub BlockKey);
pub type Blocks = SlotMap<BlockKey, Block>;

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

pub struct Node {
    pub lines: Vec<LineWithLayout>,
    pub blocks: Vec<BlockKey>,
}

pub struct Block {
    pub depth: i16,
    pub parent_idx: Option<(BlockKey, usize)>,
    pub node: NodeKey,
    pub children: Vec<BlockChild>,
    pub expanded: bool,
}

pub enum BlockChild {
    Block(BlockKey),  // owned key
    Leaf,
}

pub fn check_block(block: BlockKey, blocks: &Blocks, nodes: &Nodes) {
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

    let cnt = nodes[b.node].blocks.iter().filter(|&&bb| bb == block).count();
    assert_eq!(cnt, 1);

    match b.children[0] {
        BlockChild::Leaf => {}
        _ => panic!(),
    }    
    if b.expanded {
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
    }
}

type BlockChildRef = (BlockKey, usize);

impl Line {
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
