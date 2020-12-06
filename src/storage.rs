#![allow(dead_code)]  // TODO: remove

use std::convert::TryFrom;
use fnv::FnvHashMap;
use crate::*;

#[derive(serde::Serialize, serde::Deserialize)]
enum DiskLine {
    Text {
        text: String,
        monospace: bool,
    },
    Node {
        local_header: String,
        node_db_key: i64,
    }
}

impl DiskLine {
    fn from(line: &Line, nodes: &Nodes) -> Self {
        match line {
            Line::Text { text, monospace } =>
                DiskLine::Text {
                    text: text.clone(),
                    monospace: *monospace,
                 },
            Line::Node { local_header, node } => 
                DiskLine::Node {
                    local_header: local_header.clone(),
                    node_db_key: nodes[*node].db_key,
                },
        }
    }
    fn into(self, db_key_to_node_key: &FnvHashMap<i64, NodeKey>) -> Line {
        match self {
            DiskLine::Text { text, monospace } =>
                Line::Text { text, monospace },
            DiskLine::Node { local_header, node_db_key } =>
                Line::Node {
                    local_header,
                    node: db_key_to_node_key[&node_db_key],
                }
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
struct DiskNode {
    lines: Vec<DiskLine>,
}

impl DiskNode {
    fn from(node: &Node, nodes: &Nodes) -> Self {
        let lines = node.lines.iter()
            .map(|line| DiskLine::from(&line.line, nodes))
            .collect();
        DiskNode { lines }
    }

    fn into(
        self,
        db_key: i64,
        db_key_to_node_key: &FnvHashMap<i64, NodeKey>,
    ) -> Node {
        let lines = self.lines.into_iter()
            .map(|line| LineWithLayout::new(line.into(db_key_to_node_key)))
            .collect();
        Node {
            lines,
            blocks: Default::default(),
            cblocks: Default::default(),
            parents: Default::default(),
            db_key,
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
struct DiskBlock {
    expanded: bool,
    children: Vec<(u32, DiskBlock)>,
    // Same as in CForest:
    //  - line numbers start at 1
    //  - entries where !expanded && children.is_empty() are omitted
}

impl DiskBlock {
    fn from_block(b: BlockKey, blocks: &Blocks, cblocks: &CBlocks) -> Self {
        let b = &blocks[b];
        match &b.collapsed {
            Some(cforest) => DiskBlock {
                expanded: false,
                children: DiskBlock::from_cforest(cforest, cblocks),
            },
            None => {
                let mut children = vec![];
                for (line, child) in b.children.iter().enumerate() {
                    match *child {
                        BlockChild::Leaf => {},
                        BlockChild::Block(cb) => children.push((
                            u32::try_from(line).unwrap(),
                            DiskBlock::from_block(cb, blocks, cblocks),
                        )),
                    }
                }
                children.sort_by_key(|&(line, _)| line);
                DiskBlock {
                    expanded: true,
                    children,
                }
            }
        }
    }

    fn from_cforest(cf: &CForest, cblocks: &CBlocks) -> Vec<(u32, DiskBlock)> {
        let mut res: Vec<(u32, DiskBlock)> = cf.0.iter()
            .map(|(&line, &c)| (
                u32::try_from(line).unwrap(),
                DiskBlock::from_cblock(c, cblocks),
            ))
            .collect();
        res.sort_by_key(|&(line, _)| line);
        res
    }

    fn from_cblock(cb: CBlockKey, cblocks: &CBlocks) -> Self {
        let cb = &cblocks[cb];
        let children = DiskBlock::from_cforest(&cb.children, cblocks);
        DiskBlock {
            expanded: cb.expanded,
            children,
        }
    }

    fn into_cblock(
        self, node: NodeKey,
        cblocks: &mut CBlocks,
        nodes: &mut Nodes,
    ) -> CBlockKey {
        let children: FnvHashMap<usize, CBlockKey> = self.children.into_iter()
            .map(|(line, child)| {
                let line = usize::try_from(line).unwrap();
                let child_node = match nodes[node].lines[line].line {
                    Line::Text { .. } => panic!(),
                    Line::Node { node, .. } => node,
                };
                (line, child.into_cblock(child_node, cblocks, nodes))
            }).collect();
        let cb = cblocks.insert(CBlock {
            node,
            expanded: self.expanded,
            children: CForest(children),
        });
        let was_new = nodes[node].cblocks.insert(cb);
        assert!(was_new);
        cb
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
struct DiskCur {
    path: Vec<usize>,
    line: usize,
    pos: usize,
    y_offset: f32,
}

impl DiskCur {
    fn from(cur: &Cur, blocks: &Blocks, y_offset: f32) -> Self {
        let mut path = Vec::new();
        let mut b = cur.block;
        while let Some((parent, idx)) = blocks[b].parent_idx {
            path.push(idx);
            b = parent;
        }
        path.reverse();
        DiskCur {
            path,
            line: cur.line,
            pos: cur.pos,
            y_offset,
        }
    }

    fn into(self, root_block: BlockKey, blocks: &Blocks) -> Cur {
        let mut b = root_block;
        for p in self.path {
            b = match blocks[b].children[p] {
                BlockChild::Leaf => panic!(),
                BlockChild::Block(b) => b,
            };
        }
        Cur {
            block: b,
            line: self.line,
            pos: self.pos,
            anchor_x: 0.0,
            sel: None,
        }
    }
}
