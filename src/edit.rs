use once_cell::unsync::OnceCell;
use crate::types::*;

pub fn splice_node_lines(
    node: NodeKey,
    start_line: usize,
    end_line: usize,
    lines: Vec<Line>,
    blocks: &mut Blocks, nodes: &mut Nodes,
) {
    let num_old_lines = end_line - start_line;
    let num_new_lines = lines.len();
    nodes[node].lines.splice(
        start_line..end_line,
        lines.into_iter().map(
            |line| LineWithLayout { line, layout: OnceCell::new() }));
    for &block in &nodes[node].blocks {
        let b = &mut blocks[block];
        if !b.expanded {
            continue;
        }
        if num_old_lines > 0 {
            todo!("remove old children")
        }

        let mut children = std::mem::replace(&mut blocks[block].children, vec![]);
        for i in end_line + 1 .. children.len() {
            match children[i] {
                BlockChild::Leaf => {}
                BlockChild::Block(bb) => {
                    let idx = &mut blocks[bb].parent_idx.as_mut().unwrap().1;
                    *idx -= num_old_lines;
                    *idx += num_new_lines;
                }
            }
        }

        children.splice(start_line + 1 .. end_line + 1,
            nodes[node].lines[start_line..start_line + num_new_lines]
                .iter().map(|line| match line.line {
                    Line::Text {..} => BlockChild::Leaf,
                    Line::Node {..} => todo!(),
                }));

        blocks[block].children = children;
    }
}
