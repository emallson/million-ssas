use graph::{Graphlike, Node, Edge, Weight};
use std::collections::VecDeque;
use std::collections::BTreeSet;

extern crate rand;
use rand::distributions::{IndependentSample, Range};

pub struct ReverseBFS<'a, F>
    where F: for<'r> Fn(&'r Edge) -> bool + 'static
{
    queue: VecDeque<&'a Node>,
    graph: &'a Graphlike,
    visited: Vec<bool>,
    activator: F,
}

impl<'a, F> ReverseBFS<'a, F>
    where F: for<'r> Fn(&'r Edge) -> bool
{
    pub fn new(graph: &'a Graphlike, start: &'a Node, activator: F) -> Self
    {
        let mut q = VecDeque::new();
        q.push_back(start);

        ReverseBFS { queue: q, graph: graph, visited: vec![false; graph.count_nodes()], activator: activator }
    }

}

impl<'a, F> Iterator for ReverseBFS<'a, F>
    where F: for<'r> Fn(&'r Edge) -> bool
{
    type Item = &'a Node;

    fn next(&mut self) -> Option<&'a Node> {
        let next_node = self.queue.pop_front();

        if let Some(node) = next_node {
            self.visited[node.0 as usize] = true;
            if let Some(neighbors) = self.graph.in_neighbors(node) {
                // for &Edge {ref from, ..} in neighbors.iter().filter(&self.activator) {
                //     self.queue.push_back(from);
                // }
                let f = &self.activator;
                let visited = &self.visited;
                self.queue.append(&mut neighbors.iter().filter_map(| edge: &'a Edge | {
                    if !visited[edge.from.0 as usize] && f(&edge) {
                        Some(&edge.from)
                    } else { None }
                }).collect::<VecDeque<&'a Node>>())
            }
        }

        next_node
    }
}

pub fn sample_ic<'a>(graph: &'a Graphlike, start: &'a Node) -> BTreeSet<&'a Node> {
    let uniform = Range::new(0f32, 1f32);

    ReverseBFS::new(graph, start, move |&Edge { ref weight, .. }| {
        let mut rng = rand::thread_rng();
        let Weight(w) = *weight;
        uniform.ind_sample(&mut rng) <= w
    }).collect::<BTreeSet<&Node>>()
}

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
mod test {
    use super::quickcheck::{quickcheck};
    use std::collections::BTreeSet;
    use std::collections::VecDeque;
    use graph::{CTVMGraph, Graphlike, Node};
    use rand;
    use super::*;

    #[test]
    fn bfs_connected() {
        fn reverse_connected(graph: CTVMGraph) -> bool {
            if graph.count_nodes() == 0 {
                return true;
            }

            let mut rng = rand::thread_rng();
            let start = rand::sample(&mut rng, graph.nodes(), 1)[0];

            let result = ReverseBFS::new(&graph, &start, |_| true).take(10).collect::<BTreeSet<&Node>>();
            let mut connected = BTreeSet::new();
            let mut q = VecDeque::new();
            q.push_back(start);

            // basically re-implemented reverse BFS inline here.
            // Perhaps poor style, but I know not another way to check
            // the results. Maybe start solution-first?
            //
            // regardless, this checks that the iterator version works
            // correctly, since the while-loop version is more
            // conventional and easier to inspect
            while !q.is_empty() {
                let node = q.pop_front().unwrap();
                if connected.contains(node) {
                    continue;
                }
                connected.insert(node);
                for other in result.iter().cloned() {
                    if graph.edge_weights(other, node).is_some() {
                        q.push_back(other);
                    }
                }
            }

            connected == result
        }

        quickcheck(reverse_connected as fn(CTVMGraph) -> bool);
    }
}
