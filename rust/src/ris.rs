use graph::{Graphlike, Node, Edge};
use std::collections::VecDeque;

pub struct ReverseBFS<'a, F: 'a>
{
    queue: VecDeque<&'a Node>,
    graph: &'a Graphlike,
    activator: &'a F,
}

impl<'a, F> ReverseBFS<'a, F> {
    pub fn new(graph: &'a Graphlike, start: &'a Node, activator: &'a F) -> ReverseBFS<'a, F>
        where for<'r> F: Fn(&'r Edge) -> bool
    {
        let mut q = VecDeque::new();
        q.push_back(start);

        ReverseBFS { queue: q, graph: graph, activator: activator }
    }

}

impl<'a, F> Iterator for ReverseBFS<'a, F>
    where for<'r> F: Fn(&'r &Edge) -> bool
{
    type Item = &'a Node;

    fn next(&mut self) -> Option<&'a Node> {
        let next_node = self.queue.pop_front();

        if let Some(node) = next_node {
            if let Some(neighbors) = self.graph.in_neighbors(node) {
                for &Edge {ref from, ..} in neighbors.iter().filter(self.activator) {
                    self.queue.push_back(from);
                }
            }
        }

        next_node
    }
}
