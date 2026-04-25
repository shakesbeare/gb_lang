pub trait TreeNode {
    fn repr(&self) -> String;
}

impl<T> TreeNode for T
where
    T: AsRef<str> + std::fmt::Debug,
{
    fn repr(&self) -> String {
        format!("{:?}", self)
    }
}

impl<T> TreeNode for [T]
where
    T: TreeNode,
{
    fn repr(&self) -> String {
        let mut out = String::from("[");
        for (i, item) in self.iter().enumerate() {
            out.push_str(&item.repr());
            if i < self.len() - 1 {
                out.push_str(", ");
            }
        }
        out.push(']');
        out
    }
}
