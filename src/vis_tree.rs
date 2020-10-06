use crate::text_layout::{TextLayout, CursorCoord};
use crate::gfx::INDENT;

pub enum VisTree {
    Leaf {
        layout: TextLayout,
    },
    Node {
        children: Vec<VisTree>,
    },
}

impl VisTree {
    pub fn len(&self) -> usize {
        match self {
            VisTree::Leaf { layout } => layout.text.len(),
            VisTree::Node { .. } => 1,
        }
    }

    pub fn last_line_height(&self) -> f32 {
        match self {
            VisTree::Leaf { layout } =>
                layout.cursor_coord(layout.text.len()).height,
            VisTree::Node { children } =>
                children.last().unwrap().last_line_height(),
        }
    }

    pub fn size(&self) -> (f32, f32) {
        let mut w = 0.0f32;
        let mut h = 0.0f32;
        match self {
            VisTree::Leaf { layout } => {
                let rects = layout.hit_test_text_range(
                    0, layout.text.len(), /*include newline*/true);
                for rect in rects {
                    w = w.max(rect.left + rect.width);
                    h = h.max(rect.top + rect.height);
                }
            }
            VisTree::Node { children } => {
                for child in children {
                    let (ww, hh) = child.size();
                    w = w.max(ww);
                    h += hh;
                }
                w += INDENT;
            }
        }
        (w, h)
    }

    pub fn cursor_coord(&self, pos: usize) -> CursorCoord {
        match self {
            VisTree::Leaf { layout } => layout.cursor_coord(pos),
            VisTree::Node { .. } => {
                // TODO: get correct cursor height from child
                match pos {
                    0 => CursorCoord { x: 0.0, top: 0.0, height: 18.0 },
                    1 => {
                        let (w, h) = self.size();
                        CursorCoord {
                            x: w,
                            top: h - 18.0,
                            height: 18.0,
                        }
                    }
                    _ => panic!("{}", pos),
                }
            }
        }
    }
}

pub trait VisTreeVisitor {
    fn visit(&mut self, path: &[usize], tree: &VisTree, x: f32, y: f32);
}

impl VisTree {
    fn accept(
        &self,
        visitor: &mut dyn VisTreeVisitor,
        path: &mut Vec<usize>,
        x: f32, y: &mut f32,
    ) {
        visitor.visit(path, self, x, *y);
        match self {
            VisTree::Leaf { layout } => {
                *y += layout.height;
            }
            VisTree::Node { children } => {
                Self::forest_accept(children, visitor, path, x + INDENT, y);
            }
        }
    }

    pub fn forest_accept(
        trees: &[Self],
        visitor: &mut dyn VisTreeVisitor,
        path: &mut Vec<usize>,
        x: f32, y: &mut f32,
    ) {
        for (i, tree) in trees.iter().enumerate() {
            path.push(i);
            tree.accept(visitor, path, x, y);
            path.pop();
        }
    }    
}

fn push_path_to_first_leaf<'a>(
    t: &'a VisTree,
    path: &mut Vec<usize>,
) -> &'a VisTree {
    match t {
        VisTree::Leaf { .. } => t,
        VisTree::Node { children } => {
            match &children[0] {
                t @ VisTree::Leaf { .. } => {
                    path.push(0);
                    t
                }
                VisTree::Node { .. } => panic!(),
            }
        }
    }
}

fn push_path_to_last_leaf<'a>(
    t: &'a VisTree,
    path: &mut Vec<usize>,
) -> &'a VisTree {
    match t {
        VisTree::Leaf { .. } => t,
        VisTree::Node { children } => {
            let mut f = children;
            loop {
                path.push(f.len() - 1);
                match &f.last().unwrap() {
                    leaf @ VisTree::Leaf { .. } => break leaf,
                    VisTree::Node { children } => f = children,
                }
            }
        }
    }
}


pub fn next_leaf<'a>(
    mut f: &'a [VisTree],
    path: &mut Vec<usize>, line: &mut usize,
) -> Option<&'a VisTree> {
    let mut next = None;
    for (i, &idx) in path.iter().enumerate() {
        if idx + 1 < f.len() {
            next = Some((i, idx + 1, &f[idx + 1]));
        }
        match &f[idx] {
            VisTree::Leaf { .. } => panic!(),
            VisTree::Node { children } => f = children,
        }
    }
    let tree = if *line + 1 < f.len() {
        *line += 1;
        &f[*line]
    } else {
        match next {
            Some((len, idx, tree)) => {
                path.truncate(len);
                *line = idx;
                tree
            }
            None => return None,
        }
    };
    path.push(*line);
    let leaf = push_path_to_first_leaf(tree, path);
    *line = path.pop().unwrap();
    Some(leaf)
}

pub fn prev_leaf<'a>(
    mut f: &'a [VisTree],
    path: &mut Vec<usize>, line: &mut usize,
) -> Option<&'a VisTree> {
    let mut next = None; 
    for (i, &idx) in path.iter().enumerate() {
        if idx > 0 {
            next = Some((i, idx - 1, &f[idx - 1]));
        }
        match &f[idx] {
            VisTree::Leaf { .. } => panic!(),
            VisTree::Node { children } => f = children,
        }
    }
    let tree = if *line > 0 {
        *line -= 1;
        &f[*line]
    } else {
        match next {
            Some((len, idx, tree)) => {
                path.truncate(len);
                *line = idx;
                tree
            }
            None => return None,
        }
    };
    path.push(*line);
    let leaf = push_path_to_last_leaf(tree, path);
    *line = path.pop().unwrap();
    Some(leaf)
}
