use std::rc::Rc;
use std::cell::RefCell;
use once_cell::unsync::OnceCell;
use crate::owned_ref::{Owned, Refed};
use crate::text_layout::TextLayout;

pub enum Line {
    Text {
        text: String,
        monospace: bool,
    },
    Node {
        local_header: String,
        node: Rc<RefCell<Node>>,
    }
}

pub struct LineWithLayout {
    pub line: Line,
    pub layout: OnceCell<TextLayout>,
}

pub struct Node {
    pub lines: Vec<LineWithLayout>,
    pub blocks: Vec<Refed<Block>>,
}

pub struct Block {
    pub depth: i16,
    pub parent_idx: Option<(Refed<Block>, usize)>,
    pub node: Rc<RefCell<Node>>,
    pub children: Vec<BlockChild>,
    pub expanded: bool,
}

pub enum BlockChild {
    Block(Owned<Block>),
    Leaf,
}

type BlockChildRef = (Refed<Block>, usize);

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
}

impl Block {
    pub fn node_line_idx(&self, idx: usize) -> Option<(Rc<RefCell<Node>>, usize)> {
        match self.children[idx] {
            BlockChild::Leaf =>
                if idx == 0 {
                    if let Some((ref parent, idx)) = self.parent_idx {
                        let parent = parent.borrow();
                        assert!(idx > 0);
                        Some((Rc::clone(&parent.node), idx - 1))
                    } else {
                        None  // header of the root block
                    }
                } else {
                    Some((Rc::clone(&self.node), idx - 1))
                }
            BlockChild::Block(_) => panic!("not a leaf"),
        }
    }
}

pub fn last_leaf(block: Refed<Block>) -> BlockChildRef {
    let b = block.borrow();
    match b.children.last().unwrap() {
        BlockChild::Leaf => {
            let idx = b.children.len() - 1;
            drop(b);
            (block, idx)
        }
        BlockChild::Block(ref b) => last_leaf(b.make_ref()),
    }
}

pub fn prev_leaf((mut block, mut idx): BlockChildRef) -> Option<BlockChildRef> {
    loop {
        let b = block.borrow();

        if idx == 0 {
            match b.parent_idx.clone() {
                Some((parent_block, i)) => {
                    drop(b);
                    block = parent_block;
                    idx = i;
                    continue;
                }
                None => return None,
            }
        }

        return match b.children[idx - 1] {
            BlockChild::Block(ref block) => {
                Some(last_leaf(block.make_ref()))
            }
            BlockChild::Leaf => {
                if idx == 1 && b.depth == 0 {
                    None  // don't return header of the root block
                } else {
                    drop(b);
                    Some((block, idx - 1))
                }
            }
        }
    }
}

pub fn next_leaf((mut block, mut idx): BlockChildRef) -> Option<BlockChildRef> {
    loop {
        let b = block.borrow();

        if idx + 1 == b.children.len() {
            match b.parent_idx.clone() {
                Some((parent_block, i)) => {
                    drop(b);
                    block = parent_block;
                    idx = i;
                    continue;
                }
                None => return None,
            }
        }

        return match b.children[idx + 1] {
            BlockChild::Block(ref block) => {
                let block = block.make_ref();
                match block.borrow().children[0] {
                    BlockChild::Leaf => {},
                    BlockChild::Block {..} => panic!(),
                }
                Some((block, 0))
            }
            BlockChild::Leaf => {
                drop(b);
                Some((block, idx + 1))
            }
        }
    }
}
