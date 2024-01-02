use std::{rc::Rc, vec};

#[derive(Debug, Clone)]
pub struct StackNodeData {
    pub symbol: String,
    pub state: usize,
}

#[derive(Debug)]
struct StackNode {
    pub data: StackNodeData,
    pub prev: Option<Rc<StackNode>>,
    pub len: usize,
}

#[derive(Debug)]
pub struct Stack {
    buffer: Vec<Option<Rc<StackNode>>>,
    len: usize,
    cap: usize,
}

impl Stack {
    pub fn new(symbol: &str, state: usize) -> Self {
        Self {
            buffer: vec![Some(Rc::new(StackNode {
                data: StackNodeData {
                    symbol: symbol.to_string(),
                    state,
                },
                prev: None,
                len: 1,
            }))],
            len: 1,
            cap: 1,
        }
    }

    pub fn push(&mut self, symbol: &str, state: usize, pos: usize) {
        let curr = &self.buffer[pos].take().unwrap();
        let new_node = StackNode {
            data: StackNodeData {
                symbol: symbol.to_string(),
                state,
            },
            prev: Some(curr.clone()),
            len: curr.len + 1,
        };
        self.buffer[pos] = Some(Rc::new(new_node));
    }

    pub fn add_new_root(&mut self, parent: usize) -> usize {
        if self.len >= self.cap {
            self.cap *= 2;
            self.buffer.resize(2 * self.cap, None);
        }

        self.buffer[self.len] = self.buffer[parent].clone();
        let result = self.len;
        self.len += 1;
        result
    }

    pub fn top(&self, pos: usize) -> &StackNodeData {
        return &self.buffer[pos].as_ref().unwrap().data;
    }

    pub fn pop(&mut self, pos: usize) -> StackNodeData {
        let curr = self.buffer[pos].take().unwrap();
        let prev = curr.prev.clone();
        self.buffer[pos] = prev;

        return curr.data.clone();
    }

    pub fn remove_root(&mut self, pos: usize) {
        self.buffer[pos] = None;
    }

    pub fn roots_count(&self) -> usize {
        self.buffer.len()
    }

    pub fn len(&self, pos: usize) -> usize {
        if let Some(n) = self.buffer[pos].as_ref() {
            return n.len;
        } else {
            return 0;
        }
    }

    pub fn get_alived(&self) -> Vec<usize> {
        let mut result = vec![];
        self.buffer.iter().enumerate().for_each(|(i, c)| {
            if c.is_some() {
                result.push(i)
            }
        });
        result
    }
}
