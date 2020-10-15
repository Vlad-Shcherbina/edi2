use std::collections::HashSet;
use once_cell::unsync::OnceCell;
use crate::types::*;

fn for_each_block_descendant(
    block: BlockKey,
    blocks: &Blocks,
    f: &mut impl FnMut(BlockKey),
) {
    f(block);
    for child in &blocks[block].children {
        match *child {
            BlockChild::Leaf => {}
            BlockChild::Block(b) => for_each_block_descendant(b, blocks, f),
        }
    }
}

pub fn splice_node_lines(
    node: NodeKey,
    start_line: usize,
    end_line: usize,
    lines: Vec<Line>,
    blocks: &mut Blocks, nodes: &mut Nodes,
) -> Vec<Line> {
    let num_old_lines = end_line - start_line;
    let num_new_lines = lines.len();
    let old_lines = nodes[node].lines.splice(
        start_line..end_line,
        lines.into_iter().map(
            |line| LineWithLayout { line, layout: OnceCell::new() }));
    let old_lines: Vec<Line> = old_lines.map(|line| line.line).collect();

    let mut bs = HashSet::with_capacity(nodes[node].blocks.len());
    for &block in &nodes[node].blocks {
        let was_new = bs.insert(block);
        assert!(was_new);
    }
    while let Some(&block) = bs.iter().next() {
        bs.remove(&block);

        // TODO: it is possible that this block was destroyed on previous iterations
        let b = &mut blocks[block];
        if !b.expanded {
            continue;
        }
        let depth = b.depth;

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
                            expanded: false,
                        });
                        nodes[node].blocks.push(child_block);
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
                    for_each_block_descendant(bb, blocks, &mut |b| {
                        bs.remove(&b);
                    });
                    crate::destroy_block(bb, blocks, nodes);
                }
            }
        }

        blocks[block].children = children;
    }

    old_lines
}
