use std::convert::TryFrom;
use fnv::FnvHashMap;
use rusqlite::{Transaction, params, OptionalExtension};
use crate::*;

#[derive(serde::Serialize, serde::Deserialize)]
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
pub struct DiskNode {
    lines: Vec<DiskLine>,
}

impl DiskNode {
    pub fn from(node: &Node, nodes: &Nodes) -> Self {
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
#[derive(Debug, PartialEq)]
pub struct DiskBlock {
    expanded: bool,
    children: Vec<(u32, DiskBlock)>,
    // Same as in CForest:
    //  - line numbers start at 1
    //  - entries where !expanded && children.is_empty() are omitted
}

impl DiskBlock {
    pub fn from_block(b: BlockKey, blocks: &Blocks, cblocks: &CBlocks) -> Self {
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
                assert!(line > 0);
                let child_node = match nodes[node].lines[line - 1].line {
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
#[derive(Debug, PartialEq)]
pub struct DiskCur {
    path: Vec<usize>,
    line: usize,
    pos: usize,
    y_offset: f32,
}

impl DiskCur {
    pub fn from(cur: &Cur, blocks: &Blocks, y_offset: f32) -> Self {
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

pub fn save_cur(cur: &DiskCur, tx: &Transaction) {
    let data = bincode::serialize(cur).unwrap();
    let cur2: DiskCur = bincode::deserialize(&data).unwrap();
    assert_eq!(*cur, cur2);
    tx.prepare("INSERT OR REPLACE INTO misc (key, data) VALUES ('cur', ?)")
        .unwrap()
        .execute(params![data])
        .unwrap();
}

pub fn save_tree(tree: &DiskBlock, tx: &Transaction) {
    let data = bincode::serialize(tree).unwrap();
    let tree2: DiskBlock = bincode::deserialize(&data).unwrap();
    assert_eq!(*tree, tree2);
    tx.prepare("INSERT OR REPLACE INTO misc (key, data) VALUES ('tree', ?)")
        .unwrap()
        .execute(params![data])
        .unwrap();
}

pub fn save_node(db_key: i64, node: &DiskNode, tx: &Transaction) {
    let data = bincode::serialize(node).unwrap();
    let node2: DiskNode = bincode::deserialize(&data).unwrap();
    assert_eq!(*node, node2);
    tx.prepare("INSERT OR REPLACE INTO node (key, data) VALUES (?, ?)")
        .unwrap()
        .execute(params![db_key, data])
        .unwrap();
}

pub struct AppInit {
    pub blocks: Blocks,
    pub cblocks: CBlocks,
    pub nodes: Nodes,
    pub db_key_to_node_key: FnvHashMap<i64, NodeKey>,
    pub root_block: BlockKey,
    pub cur: Cur,
    pub y_offset: f32
}

pub fn load_or_create(tx: &Transaction) -> AppInit {
    // Misc table entries:
    //   version: "1"
    //   tree: ...
    //   cur: ...
    //   root_node: i64
    tx.execute_batch("
        CREATE TABLE IF NOT EXISTS
        misc (
            key TEXT NOT NULL PRIMARY KEY,
            data BLOB
        );

        CREATE TABLE IF NOT EXISTS
        node (
            key INTEGER PRIMARY KEY,
            data BLOB
        )
    ").unwrap();
    let version: Option<String> = tx
        .prepare("SELECT data FROM misc WHERE key = 'version'").unwrap()
        .query_row(params![], |row| row.get(0))
        .optional().unwrap();

    match version {
        None => {
            println!("Initializing new db");

            tx.prepare("INSERT OR ABORT INTO misc (key, data) VALUES ('version', '1')")
                .unwrap()
                .execute(params![])
                .unwrap();

            let tree = DiskBlock {
                expanded: true,
                children: vec![],
            };
            save_tree(&tree, tx);

            let cur = DiskCur {
                path: vec![],
                line: 1,
                pos: 0,
                y_offset: 0.0,
            };
            save_cur(&cur, tx);

            tx.prepare("INSERT OR ABORT INTO misc (key, data) VALUES ('root_node', 1)")
                .unwrap()
                .execute(params![])
                .unwrap();

            let node = DiskNode {
                lines: vec![DiskLine::Text { text: String::new(), monospace: false }],
            };
            save_node(1, &node, tx);
        }
        Some(version) => assert_eq!(version, "1"),
    }

    let mut nodes = Nodes::new();
    let mut blocks = Blocks::new();
    let mut cblocks = CBlocks::new();

    let db_key_to_node_key: FnvHashMap<i64, NodeKey> =
        tx.prepare("SELECT key FROM node").unwrap()
        .query_map(params![], |row| {
            let key: i64 = row.get(0)?;
            let n = nodes.insert(Node {
                lines: vec![],
                parents: Default::default(),
                blocks: Default::default(),
                cblocks: Default::default(),
                db_key: -1,
            });
            Ok((key, n))
        })
        .unwrap().map(Result::unwrap).collect();
    tx.prepare("SELECT key, data FROM node").unwrap()
    .query_map(params![], |row| {
        let key: i64 = row.get(0)?;
        let data = row.get_raw(1).as_blob()?;
        let node: DiskNode = bincode::deserialize(data).unwrap();
        let node = node.into(key, &db_key_to_node_key);
        nodes[db_key_to_node_key[&key]] = node;
        Ok(())
    })
    .unwrap().for_each(Result::unwrap);

    for &node in db_key_to_node_key.values() {
        let num_lines = nodes[node].lines.len();
        for i in 0..num_lines {
            match nodes[node].lines[i].line {
                Line::Text { .. } => {}
                Line::Node { node: child, .. } =>
                    *nodes[child].parents.entry(node).or_default() += 1,
            }
        }
    }

    let root_node: i64 =
        tx.prepare("SELECT data FROM misc WHERE key = 'root_node'").unwrap()
        .query_row(params![], |row| row.get(0)).unwrap();
    let root_node = db_key_to_node_key[&root_node];

    let tree: Vec<u8> =
        tx.prepare("SELECT data FROM misc WHERE key = 'tree'").unwrap()
        .query_row(params![], |row| row.get(0))
        .unwrap();
    let tree: DiskBlock = bincode::deserialize(&tree).unwrap();

    let tree = tree.into_cblock(root_node, &mut cblocks, &mut nodes);
    let was_there = nodes[root_node].cblocks.remove(&tree);
    assert!(was_there);
    let tree = cblocks.remove(tree);
    assert!(tree.expanded);

    let root_block = blocks.insert(Block {
        parent_idx: None,
        depth: 0,
        node: root_node,
        children: vec![BlockChild::Leaf],
        collapsed: Some(tree.children),
    });
    let was_new = nodes[root_node].blocks.insert(root_block);
    assert!(was_new);

    expand_block(
        root_block,
        &mut blocks, &mut cblocks, &mut nodes,
        &mut Unsaved::new());

    let cur: Vec<u8> =
        tx.prepare("SELECT data FROM misc WHERE key = 'cur'").unwrap()
        .query_row(params![], |row| row.get(0))
        .unwrap();
    let cur: DiskCur = bincode::deserialize(&cur).unwrap();
    let y_offset = cur.y_offset;
    let cur = cur.into(root_block, &blocks);

    AppInit {
        nodes,
        blocks,
        cblocks,
        db_key_to_node_key,
        root_block,
        cur,
        y_offset,
    }
}
