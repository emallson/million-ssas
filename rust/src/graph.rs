#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
use self::quickcheck::{Arbitrary, Gen};

use std::ops::Add;

use std::collections::HashMap;
use std::collections::HashSet;

#[derive(PartialEq, Eq, Hash, Clone, Debug, PartialOrd, Ord)]
pub struct Node(pub u64);

#[cfg(test)]
impl Arbitrary for Node {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        Node(u64::arbitrary(g))
    }
}

#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub struct Weight(pub f32);

#[cfg(test)]
impl Arbitrary for Weight {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        Weight(f32::arbitrary(g))
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Edge {
    pub from: Node,
    pub to: Node,
    pub weight: Weight
}

#[cfg(test)]
impl Arbitrary for Edge {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        Edge {
            from: Node::arbitrary(g),
            to: Node::arbitrary(g),
            weight: Weight::arbitrary(g)
        }
    }
}

/// Describes a basic weighted, directed graph.
///
/// Implementations of this trait are free to add additional
/// functionality. See `CTVMGraph` for an example.
pub trait Graphlike {
    /// Returns the number of nodes in the graph.
    fn count_nodes(&self) -> usize;
    /// Returns the number of edges in the graph.
    fn count_edges(&self) -> usize;

    /// Returns the complete node list.
    fn nodes(&self) -> &HashSet<Node>;
    /// Returns the out-neighbors of a node.
    fn neighbors(&self, node: &Node) -> Option<&Vec<Edge>>;
    /// Returns the in-neighbors of a node.
    /// # Implementation Note
    /// This should not involve a scan of all node's edge lists.
    fn in_neighbors(&self, node: &Node) -> Option<&Vec<Edge>>;

    /// Add a node to the graph. Duplicates aren't allowed. Returns
    /// `True` if the node was added, `False` if it was already
    /// present.
    fn add_node(&mut self, node: Node) ;
    /// Add an edge to the graph. Duplicate edges are *not* checked
    /// for, but technically allowed.
    fn add_edge(&mut self, edge: Edge);
    /// Remove an edge from the graph. If multiple edges are present,
    /// remove exactly one arbitrarily. Returns `True` if an edge was
    /// removed.
    fn del_edge(&mut self, from: Node, to: Node) -> bool;

    /// Returns the weights of each edge (from, to). Typically will be
    /// just 0 or 1 edge.
    fn edge_weights(&self, from: &Node, to: &Node) -> Option<Vec<&Weight>>;
}

#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
pub struct Cost(pub f32);

impl Add for Cost {
    type Output = Self;

    fn add(self, Cost(rhs): Self) -> Self::Output {
        let Cost(lhs) = self;
        Cost(lhs + rhs)
    }
}

#[cfg(test)]
impl Arbitrary for Cost {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        Cost(f32::arbitrary(g))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Benefit(pub f32);

#[cfg(test)]
impl Arbitrary for Benefit {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        Benefit(f32::arbitrary(g))
    }
}

/// A graph implementation that makes *strong* assumptions about the
/// input in order to avoid using HashMaps/HashSets.
///
/// We assume:
/// 1. The node ids are a range 0..n-1.
/// 2. Every edge refers only to the existing nodes.
#[derive(PartialEq, Clone, Debug)]
pub struct SequentialGraph {
    nodes: HashSet<Node>,
    edges: Vec<Vec<Edge>>,
    in_edges: Vec<Vec<Edge>>,
    num_edges: usize,
    costs: Vec<Cost>,
    benefits: Vec<Benefit>,
}

impl SequentialGraph {
    pub fn new(nodes: &usize, edges: &usize) -> Self {
        return SequentialGraph {
            nodes: HashSet::new(),
            edges: vec![Vec::new(); *nodes],
            in_edges: vec![Vec::new(); *nodes],
            num_edges: 0,
            costs: vec![Cost(0f32); *nodes],
            benefits: vec![Benefit(0f32); *nodes]
        }
    }

    pub fn add_weighted_node(&mut self, node: Node, cost: Cost, benefit: Benefit) {
        let id = node.0 as usize;
        self.add_node(node);
        self.costs[id] = cost;
        self.benefits[id] = benefit;
    }

    pub fn get_cost(&self, node: &Node) -> Option<&Cost> {
        Some(&self.costs[node.0 as usize])
    }

    pub fn get_benefit(&self, node: &Node) -> Option<&Benefit> {
        Some(&self.benefits[node.0 as usize])
    }
}

impl Graphlike for SequentialGraph {
    fn count_nodes(&self) -> usize {
        self.nodes.len()
    }

    fn count_edges(&self) -> usize {
        self.num_edges
    }

    fn nodes(&self) -> &HashSet<Node> {
        &self.nodes
    }

    fn add_node(&mut self, node: Node) {
        self.nodes.insert(node);
    }

    fn add_edge(&mut self, edge: Edge) {
        self.edges[edge.from.0 as usize].push(edge.clone());
        self.in_edges[edge.to.0 as usize].push(edge);
        self.num_edges += 1;
    }

    fn del_edge(&mut self, from: Node, to: Node) -> bool {
        assert!(false);
        false
    }

    fn neighbors(&self, node: &Node) -> Option<&Vec<Edge>> {
        Some(&self.edges[node.0 as usize])
    }

    fn in_neighbors(&self, node: &Node) -> Option<&Vec<Edge>> {
        Some(&self.in_edges[node.0 as usize])
    }

    fn edge_weights(&self, from: &Node, to: &Node) -> Option<Vec<&Weight>> {
        Some(self.edges[from.0 as usize].iter().filter_map(| &Edge { to: ref node, ref weight, ..} | if node == to { Some(weight) } else { None }).collect())
    }
}

/// A graph specialized for the Cost-Aware Targeted Viral Marketing
/// problem.
#[derive(PartialEq, Clone, Debug)]
pub struct CTVMGraph {
    nodes: HashSet<Node>,
    edges: HashMap<Node, Vec<Edge>>,
    in_edges: HashMap<Node, Vec<Edge>>,
    num_edges: usize,
    costs: HashMap<Node, Cost>,
    benefits: HashMap<Node, Benefit>,
}

impl Graphlike for CTVMGraph {
    fn count_nodes(&self) -> usize {
        self.nodes.len()
    }

    fn count_edges(&self) -> usize {
        self.num_edges
    }

    fn nodes(&self) -> &HashSet<Node> {
        &self.nodes
    }

    fn neighbors(&self, node: &Node) -> Option<&Vec<Edge>> {
        self.edges.get(node)
    }

    fn in_neighbors(&self, node: &Node) -> Option<&Vec<Edge>> {
        self.in_edges.get(node)
    }

    fn add_node(&mut self, node: Node) {
        self.nodes.insert(node);
    }

    fn add_edge(&mut self, edge: Edge) {
        let Edge {ref from, ref to, ..} = edge;
        assert!(self.nodes.contains(from));
        assert!(self.nodes.contains(to));

        if self.edges.contains_key(from) {
            self.edges.get_mut(from).unwrap().push(edge.clone());
        } else {
            self.edges.insert(from.clone(), vec![edge.clone()]);
        }

        if self.in_edges.contains_key(to) {
            self.in_edges.get_mut(to).unwrap().push(edge.clone());
        } else {
            self.in_edges.insert(to.clone(), vec![edge.clone()]);
        }

        self.num_edges += 1;
    }

    fn del_edge(&mut self, from: Node, to: Node) -> bool {
        let deleted_out = match self.edges.get_mut(&from) {
            Some(vec) => match vec.iter().position(|&Edge {to: ref node, ..}| *node == to) {
                Some(index) => {
                    vec.swap_remove(index);
                    true
                },
                None => false
            },
            None => false
        };

        let deleted_in = match self.in_edges.get_mut(&to) {
            Some(vec) => match vec.iter().position(|&Edge {from: ref node, ..}| *node == from) {
                Some(index) => {
                    vec.swap_remove(index);
                    true
                },
                None => false
            },
            None => false
        };

        if deleted_out && deleted_in {
            self.num_edges -= 1;
        };

        deleted_out && deleted_in
    }

    fn edge_weights(&self, from: &Node, to: &Node) -> Option<Vec<&Weight>> {
        match self.edges.get(&from) {
            Some(vec) => Some(vec.iter().filter_map(
                |&Edge {to: ref node, ref weight, ..}|
                if node == to {
                    Some(weight)
                } else {
                    None
                }).collect()),
            None => None
        }
    }
}

impl CTVMGraph {

    pub fn new() -> CTVMGraph {
        return CTVMGraph {
            nodes: HashSet::new(),
            edges: HashMap::new(),
            in_edges: HashMap::new(),
            costs: HashMap::new(),
            benefits: HashMap::new(),
            num_edges: 0
        }
    }

    pub fn add_weighted_node(&mut self, node: Node, cost: Cost, benefit: Benefit) {
        self.add_node(node.clone());
        self.costs.insert(node.clone(), cost);
        self.benefits.insert(node.clone(), benefit);
    }

    pub fn get_cost(&self, node: &Node) -> Option<&Cost> {
        self.costs.get(node)
    }

    pub fn get_benefit(&self, node: &Node) -> Option<&Benefit> {
        self.benefits.get(node)
    }
}

#[cfg(test)]
impl Arbitrary for CTVMGraph {

    fn arbitrary<G: Gen>(gen: &mut G) -> Self {
        let edges = Vec::<Edge>::arbitrary(gen);
        let mut g = CTVMGraph::new();
        let nodes = edges.iter().flat_map(|&Edge { ref from, ref to, ..}|
                                          vec![from.clone(), to.clone()])
            .collect::<HashSet<Node>>();

        for n in nodes {
            g.add_weighted_node(n, Cost::arbitrary(gen), Benefit::arbitrary(gen));
        }

        for e in edges {
            g.add_edge(e)
        }

        return g;
    }
}

#[cfg(test)]
mod test {
    use super::quickcheck::{quickcheck, TestResult};
    use std::collections::HashSet;
    use super::*;

    // Tests in this module use CTVMGraph as a concrete way to test
    // the trait Graphlike because there isn't another concrete impl.

    #[test]
    fn node_eq() {
        fn neq(a: Node, b: Node) -> bool {
            let Node(ac) = a;
            let Node(bc) = b;
            a == b || ac != bc
        }

        quickcheck(neq as fn(Node, Node) -> bool);
    }

    #[test]
    fn edge_eq() {
        fn eq(a: Edge, b: Edge) -> bool {
            let &Edge {from: ref af, to: ref at, weight: ref aw} = &a;
            let &Edge {from: ref bf, to: ref bt, weight: ref bw} = &b;
            a == b || af != bf || at != bt || aw != bw
        }

        quickcheck(eq as fn(Edge, Edge) -> bool);
    }

    #[test]
    fn graph_add_nodes() {
        fn added_nodes_are_in_list(nodes: HashSet<Node>) -> bool {
            let mut g = CTVMGraph::new();

            for v in nodes.iter().cloned() {
                g.add_node(v);
            }

            g.nodes().iter().cloned().collect::<HashSet<Node>>() == nodes
        }

        quickcheck(added_nodes_are_in_list as fn(HashSet<Node>) -> bool);
    }

    #[test]
    fn graph_add_edges() {
        fn added_edges_are_in_neighbors(edges: Vec<Edge>) -> bool {
            let nodes = edges.iter().flat_map(|&Edge { ref from, ref to, ..}|
                                              vec![from.clone(), to.clone()])
                .collect::<HashSet<Node>>();
            let mut g = CTVMGraph::new();

            for v in nodes {
                g.add_node(v);
            }

            for e in edges.iter().cloned() {
                g.add_edge(e);
            }

            edges.iter().all(|ref e| g.neighbors(&e.from).unwrap().contains(&e)) &&
                edges.iter().all(|ref e| g.in_neighbors(&e.to).unwrap().contains(&e))
        }
        quickcheck(added_edges_are_in_neighbors as fn(Vec<Edge>) -> bool);
    }

    #[test]
    fn graph_edge_weight() {
        fn edge_weight_correct_edge(edges: Vec<Edge>) -> bool {
            let nodes = edges.iter().flat_map(|&Edge { ref from, ref to, ..}|
                                              vec![from.clone(), to.clone()])
                .collect::<HashSet<Node>>();
            let mut g = CTVMGraph::new();

            for v in nodes {
                g.add_node(v);
            }

            for e in edges.iter().cloned() {
                g.add_edge(e);
            }

            edges.iter().all(|&Edge { ref from, ref to, ref weight }|
                             g.edge_weights(from, to).unwrap().contains(&weight))
        }
        quickcheck(edge_weight_correct_edge as fn(Vec<Edge>) -> bool);
    }

    #[test]
    fn graph_counts() {
        fn correct_count_nodes(nodes: HashSet<Node>) -> bool {
            let mut g = CTVMGraph::new();

            for v in nodes.iter().cloned() {
                g.add_node(v);
            }

            g.count_nodes() == nodes.len()
        }

        fn correct_count_edges(edges: Vec<Edge>) -> bool {
            let nodes = edges.iter().flat_map(|&Edge { ref from, ref to, ..}|
                                              vec![from.clone(), to.clone()])
                .collect::<HashSet<Node>>();
            let mut g = CTVMGraph::new();

            for v in nodes {
                g.add_node(v);
            }

            for e in edges.iter().cloned() {
                g.add_edge(e);
            }

            g.count_edges() == edges.len()
        }

        quickcheck(correct_count_nodes as fn(HashSet<Node>) -> bool);
        quickcheck(correct_count_edges as fn(Vec<Edge>) -> bool);
    }

    #[test]
    fn graph_ctvm_weights() {
        fn correct_weights(nodes: HashSet<Node>, costs: Vec<Cost>, benefits: Vec<Benefit>) -> TestResult {
            if nodes.len() > costs.len() || nodes.len() > benefits.len() {
                return TestResult::discard()
            }

            let mut g = CTVMGraph::new();
            let vnodes = nodes.iter().collect::<Vec<&Node>>();

            for i in 0..nodes.len() {
                g.add_weighted_node(vnodes[i].clone(), costs[i].clone(), benefits[i].clone());
            }

            TestResult::from_bool((0..nodes.len()).all(|i| *g.get_cost(vnodes[i]).unwrap() == costs[i] &&
                                                       *g.get_benefit(vnodes[i]).unwrap() == benefits[i]))
        }

        quickcheck(correct_weights as fn(HashSet<Node>, Vec<Cost>, Vec<Benefit>) -> TestResult)
    }
}
