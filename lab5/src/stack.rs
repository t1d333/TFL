use std::{fmt::Display, rc::Rc, vec};

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
    buffer: Vec<(Option<Rc<StackNode>>, usize)>,
    len: usize,
    cap: usize,
}

impl Stack {
    pub fn new(symbol: &str, state: usize, pos: usize) -> Self {
        Self {
            buffer: vec![(Some(Rc::new(StackNode {
                data: StackNodeData {
                    symbol: symbol.to_string(),
                    state,
                },
                prev: None,
                len: 1,
            })), pos)],
            len: 1,
            cap: 1,
        }
    }

    pub fn push(&mut self, symbol: &str, state: usize, pos: usize, pointer: usize) {
        let curr = &self.buffer[pos].0.take().unwrap();
        let new_node = StackNode {
            data: StackNodeData {
                symbol: symbol.to_string(),
                state,
            },
            prev: Some(curr.clone()),
            len: curr.len + 1,
        };
        self.buffer[pos] = (Some(Rc::new(new_node)), pointer);
    }

    pub fn add_new_root(&mut self, parent: usize) -> usize {
        if self.len >= self.cap {
            self.cap *= 2;
            self.buffer.resize(2 * self.cap, (None, 0));
        }

        self.buffer[self.len] = self.buffer[parent].clone();
        let result = self.len;
        self.len += 1;
        result
    }

    pub fn top(&self, pos: usize) -> (&StackNodeData, usize) {
        return (&self.buffer[pos].0.as_ref().unwrap().data, self.buffer[pos].1);
    }

    pub fn pop(&mut self, pos: usize) -> (StackNodeData , usize){
        let curr = self.buffer[pos].0.take().unwrap();
        let pointer = self.buffer[pos].1;
        let prev = curr.prev.clone();
        self.buffer[pos] = (prev, pointer);

        return (curr.data.clone(), pointer);
    }

    pub fn remove_root(&mut self, pos: usize) {
        self.buffer[pos] = (None, 0);
    }

    pub fn roots_count(&self) -> usize {
        self.buffer.len()
    }

    pub fn len(&self, pos: usize) -> usize {
        if let Some(n) = self.buffer[pos].0.as_ref() {
            return n.len;
        } else {
            return 0;
        }
    }

    pub fn get_alived(&self) -> Vec<usize> {
        let mut result = vec![];
        self.buffer.iter().enumerate().for_each(|(i, c)| {
            if c.0.is_some() {
                result.push(i)
            }
        });
        result
    }
}

impl Display for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = format!("ALIVE ROOTS COUNT: {}", self.get_alived().len());

        for (i, node) in self.buffer.iter().enumerate() {
            let mut tmp = node.0.clone();

            if tmp.is_some() {
                result = format!("{}\nSTACK NUMBER {}; CURR POS: {}: ", result, i, node.1);
            }

            while let Some(curr) = tmp {
                result = format!(
                    "{} (STATE: {}, SYMBOL: \"{}\")",
                    result, curr.data.state, curr.data.symbol
                );
                tmp = curr.prev.clone();
            }
        }

        result = format!("{}\n", result);

        write!(f, "{}", result)
    }
}
