extern crate petgraph;
extern crate rand;

mod ris;

use petgraph::visit::{Graphlike, Visitable, NeighborIter};
use ris::{IC, sample};

use petgraph::Graph;

fn main() {
    let mut graph = Graph::<f32,f32>::new();

    let a = graph.add_node(0.0);
    let b = graph.add_node(0.0);
    let c = graph.add_node(0.0);
    let d = graph.add_node(0.0);

    graph.add_edge(a, b, 1.0);
    graph.add_edge(b, c, 1.0);
    graph.add_edge(b, d, 1.0);
    graph.add_edge(c, a, 1.0);
    graph.add_edge(d, b, 1.0);

    let s = sample(&graph, a);
    // for node in &s {
    //     println!("{}", node.index());
    // }
}
