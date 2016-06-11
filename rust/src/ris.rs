extern crate petgraph;
extern crate rand;

use petgraph::{Graph, Bfs, Directed, EdgeType, EdgeDirection};
use petgraph::visit::{Reversed, NeighborIter, Graphlike, Visitable};
use petgraph::graph::{NodeIndex, IndexType, Neighbors, Edges, Edge};

use rand::{thread_rng, Rng};

use std::collections::HashSet;
use std::hash::Hash;
use std::cmp::Eq;

pub struct IC<G>(pub G);

impl <'a, G: Graphlike> Graphlike for IC<&'a G>
{
    type NodeId = G::NodeId;
}

impl <'a, 'b, N, Ty, Ix> NeighborIter<'a> for IC<&'b Graph<N, f32, Ty, Ix>> where
    Ty: EdgeType,
    Ix: IndexType,
{
    type Iter = Neighbors<'a, f32, Ix>;

    fn neighbors(&'a self, n: NodeIndex<Ix>) -> Self::Iter
    {
        let existing_edges = self.0.edges_directed(n, EdgeDirection::Incoming)
            .filter_map(| pair | {
                let (node, weight) = pair;
                if thread_rng().gen::<f32>() <= *weight {
                    Some(self.0.find_edge(node, n))
                } else {
                    None
                }
            }).collect::<Vec<Edge<_, Ix>>>();

        Neighbors {
            iter: Edges {
                skip_start: n,
                edges: existing_edges,
                next: [existing_edges.peek(); 2]
            }
        }
    }
}

impl <'a, V: Visitable> Visitable for IC<&'a V> {
    type Map = V::Map;
    fn visit_map(&self) -> V::Map {
        self.0.visit_map()
    }
}

pub fn sample<'a, G: Graphlike + Visitable + NeighborIter<'a>>(g: &'a G, s: G::NodeId) -> HashSet<G::NodeId> where
    G::NodeId: Hash + Eq,
{
    let mut bfs = Bfs::new(g, s);
    let mut set = HashSet::new();

    while let Some(u) = bfs.next(g) {
        set.insert(u);
    }
    return set;
}
