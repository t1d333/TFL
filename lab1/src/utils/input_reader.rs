use std::{collections::HashMap, io};

#[derive(Debug)]
pub struct Reader {}

impl Reader {
    pub fn read<W>(reader: W) -> HashMap<String, String>
    where
        W: io::BufRead,
    {
        let mut result = HashMap::new();

        reader.lines().for_each(|line| {
            let s = line.expect("failed to unwrap");
            let v: Vec<&str> = s.split("->").collect();
            result.insert(v[0].trim().to_string(), v[1].trim().to_string());
        });

        result
    }
}
