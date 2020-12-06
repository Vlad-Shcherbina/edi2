use fnv::FnvHashSet;
use once_cell::unsync::OnceCell;
use crate::types::*;
use crate::Unsaved;


// Note that this operation is not a part of Edit enum,
// because we don't undo it. Nodes are never explicitly
// deleted. Instead, they become unreachable, and maybe
// garbage-collected (though currently it's not done).
pub fn create_empty_node(nodes: &mut Nodes) -> NodeKey {
    nodes.insert(Node {
        lines: vec![],
        blocks: Default::default(),
        cblocks: Default::default(),
        parents: Default::default(),
    })
}

pub enum Edit {
    SpliceLineText {
        node: NodeKey,
        line_idx: usize,
        start_pos: usize,
        end_pos: usize,
        substring: String,
    },
    SpliceNodeLines {
        node: NodeKey,
        start_line: usize,
        end_line: usize,
        lines: Vec<Line>,
    }
}

pub fn apply_edit(e: Edit,
    blocks: &mut Blocks, cblocks: &mut CBlocks, nodes: &mut Nodes,
    undo_buf: &mut Vec<Edit>,
    unsaved: &mut Unsaved,
) {
    match e {
        Edit::SpliceLineText {
            node, line_idx,
            start_pos, end_pos,
            substring,
        } => {
            splice_line_text(
                node, line_idx,
                start_pos, end_pos,
                &substring,
                nodes, undo_buf, unsaved);
        }
        Edit::SpliceNodeLines {
            node,
            start_line, end_line,
            lines,
        } => {
            splice_node_lines(
                node, start_line, end_line, lines,
                blocks, cblocks, nodes, undo_buf, unsaved);
        }
    }
}

pub fn splice_line_text(
    node: NodeKey, line_idx: usize,
    start_pos: usize, end_pos: usize,
    substring: &str,
    nodes: &mut Nodes,
    undo_buf: &mut Vec<Edit>,
    unsaved: &mut Unsaved,
) -> String {
    unsaved.nodes.insert(node);

    let line = &mut nodes[node].lines[line_idx];
    let text = line.line.text_mut();

    let old_substring = text[start_pos..end_pos].to_owned();
    text.replace_range(start_pos..end_pos, substring);
    line.layout = OnceCell::new();

    undo_buf.push(Edit::SpliceLineText {
        node,
        line_idx,
        start_pos,
        end_pos: start_pos + substring.len(),
        substring: old_substring.clone(),
    });

    old_substring
}

fn for_each_block_descendant(
    block: BlockKey,
    blocks: &Blocks, cblocks: &CBlocks,
    f: &mut impl FnMut(BlockKey),
    cf: &mut impl FnMut(CBlockKey),
) {
    f(block);
    for child in &blocks[block].children {
        match *child {
            BlockChild::Leaf => {}
            BlockChild::Block(b) => for_each_block_descendant(b, blocks, cblocks, f, cf),
        }
    }
    if let Some(collapsed) = blocks[block].collapsed.as_ref() {
        for &child in collapsed.0.values() {
            for_each_cblock_descendant(child, cblocks, cf);
        }
    }
}

fn for_each_cblock_descendant(
    cblock: CBlockKey,
    cblocks: &CBlocks,
    cf: &mut impl FnMut(CBlockKey),
) {
    cf(cblock);
    for &child in cblocks[cblock].children.0.values() {
        for_each_cblock_descendant(child, cblocks, cf);
    }
}

pub fn splice_node_lines(
    node: NodeKey,
    start_line: usize,
    end_line: usize,
    lines: Vec<Line>,
    blocks: &mut Blocks, cblocks: &mut CBlocks, nodes: &mut Nodes,
    undo_buf: &mut Vec<Edit>,
    unsaved: &mut Unsaved,
) -> Vec<Line> {
    unsaved.nodes.insert(node);

    let num_old_lines = end_line - start_line;
    let num_new_lines = lines.len();

    for line in &lines {
        match *line {
            Line::Text { .. } => {}
            Line::Node { node: child, .. } => {
                *nodes[child].parents.entry(node).or_default() += 1;
            }
        }
    }

    let old_lines = nodes[node].lines.splice(
        start_line..end_line,
        lines.into_iter().map(
            |line| LineWithLayout { line, layout: OnceCell::new() }));
    let old_lines: Vec<Line> = old_lines.map(|line| line.line).collect();

    for line in &old_lines {
        match *line {
            Line::Text { .. } => {}
            Line::Node { node: child, .. } => {
                let t = nodes[child].parents.remove(&node).unwrap();
                assert!(t > 0);
                let t = t - 1;
                if t > 0 {
                    nodes[child].parents.insert(node, t);
                }
            }
        }
    }

    undo_buf.push(Edit::SpliceNodeLines {
        node,
        start_line,
        end_line: start_line + num_new_lines,
        lines: old_lines.clone(),
    });

    let mut bs: FnvHashSet<BlockKey> = nodes[node].blocks.clone();
    let mut cbs: FnvHashSet<CBlockKey> = nodes[node].cblocks.clone();
    while let Some(&block) = bs.iter().next() {
        bs.remove(&block);

        if let Some(collapsed) = blocks[block].collapsed.as_mut() {
            let old_children = splice_cforest(
                collapsed,
                start_line + 1, end_line + 1, num_new_lines);
            for old_child in old_children {
                for_each_cblock_descendant(old_child, cblocks,
                    &mut |cb| { cbs.remove(&cb); },
                );
                crate::destroy_cblock(old_child, cblocks, nodes);
            }

            assert_eq!(blocks[block].children.len(), 1);
            continue;
        }

        let depth = blocks[block].depth;

        let mut children = std::mem::replace(&mut blocks[block].children, vec![]);
        for child in &children[end_line + 1..] {
            match *child {
                BlockChild::Leaf => {}
                BlockChild::Block(bb) => {
                    let idx = &mut blocks[bb].parent_idx.as_mut().unwrap().1;
                    *idx -= num_old_lines;
                    *idx += num_new_lines;
                }
            }
        }

        let new_lines: Vec<_> = 
            nodes[node].lines[start_line..start_line + num_new_lines]
            .iter()
            .map(|line| line.line.clone())
            .collect();
        let new_lines: Vec<_> = new_lines.into_iter().enumerate()
            .map(|(i, line)| {
                match line {
                    Line::Text { .. } => BlockChild::Leaf,
                    Line::Node { node, .. } => {
                        let child_block = blocks.insert(Block {
                            depth: depth + 1,
                            node,
                            parent_idx: Some((block, start_line + 1 + i)),
                            children: vec![BlockChild::Leaf],
                            collapsed: Some(CForest::new()),
                        });
                        let was_new = nodes[node].blocks.insert(child_block);
                        assert!(was_new);
                        BlockChild::Block(child_block)
                    }
                }
            })
            .collect();

        let old_children = children.splice(start_line + 1 .. end_line + 1, new_lines);
        let old_children: Vec<_> = old_children.collect();
        for old_child in old_children {
            match old_child {
                BlockChild::Leaf => {},
                BlockChild::Block(bb) => {
                    for_each_block_descendant(bb, blocks, cblocks,
                        &mut |b| { bs.remove(&b); },
                        &mut |cb| { cbs.remove(&cb); },
                    );
                    crate::destroy_block(bb, blocks, cblocks, nodes);
                }
            }
        }

        blocks[block].children = children;
    }

    while let Some(&cblock) = cbs.iter().next() {
        cbs.remove(&cblock);
        let old_children = splice_cforest(
            &mut cblocks[cblock].children,
            start_line + 1, end_line + 1, num_new_lines);
        for old_child in old_children {
            for_each_cblock_descendant(old_child, cblocks,
                &mut |cb| { cbs.remove(&cb); },
            );
            crate::destroy_cblock(old_child, cblocks, nodes);
        }
    }

    old_lines
}

fn splice_cforest(
    cforest: &mut CForest, start_line: usize, end_line: usize, num_new_lines: usize,
) -> Vec<CBlockKey> {
    let mut result = vec![];
    cforest.0 = cforest.0.drain().filter_map(|(i, cb)| {
        if i < start_line {
            Some((i, cb))
        } else if i < end_line {
            result.push(cb);
            None
        } else {
            let i = i - (end_line - start_line);
            let i = i + num_new_lines;
            Some((i, cb))
        }
    }).collect();
    result
}