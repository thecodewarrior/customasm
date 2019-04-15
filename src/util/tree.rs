use std::fmt;

#[derive(Clone)]
pub struct TreeNode {
    pub name: String,
    pub children: Vec<TreeNode>
}

impl TreeNode {
    pub fn new(name: String) -> TreeNode {
        TreeNode {
            name: name,
            children: Vec::new()
        }
    }

    pub fn add_node(&mut self, prefix: &str, child: TreeNode) {
        let mut child = child;
        child.name.insert_str(0, prefix);
        self.children.push(child)
    }

    pub fn add_leaf(&mut self, name: String) {
        self.children.push(TreeNode::new(name))
    }

    fn push_str(&self, depth: usize, f: &mut fmt::Formatter) -> fmt::Result {
        let fill = "    "; // " │ "
        let initial_prefix: String;
        let prefix: String;
        if depth >= 1 {
            initial_prefix = fill.repeat(depth-1) + " ╚═ ";
            prefix = fill.repeat(depth) + " ";
        } else {
            initial_prefix = String::new();
            prefix = fill.repeat(depth);
        };
        let mut i = 0;
        for line in self.name.split('\n').collect::<Vec<&str>>() {
            writeln!(f, "{}", if i == 0 { initial_prefix.clone() } else { prefix.clone() } + line)?;
            i += 1;
        }
        for child in &self.children {
            child.push_str(depth + 1, f)?;
        }
        Ok(())
    }
}

impl fmt::Display for TreeNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.push_str(0, f)
    }
}
