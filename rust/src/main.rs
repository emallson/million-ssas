mod graph;
mod ris;

extern crate rand;

use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use std::str::FromStr;

use graph::*;
use ris::*;
use rand::{thread_rng, sample};

use std::collections::HashSet;

fn main()
{
    let f = File::open("/home/emallson/Code/cikm16/datasets/_processed/ca-GrQc.txt").unwrap();
    let mut reader = BufReader::new(f);

    let mut graph = CTVMGraph::new();
    let mut first_line = String::new();

    reader.read_line(&mut first_line).unwrap();

    let first_row = first_line.split_whitespace().map(|s| usize::from_str(s).unwrap()).collect::<Vec<usize>>();
    assert!(first_row.len() == 2);
    let (nodes, edges) = (first_row[0], first_row[1]);

    let mut lines = reader.lines();

    for res in lines.by_ref().take(nodes) {
        match res {
            Ok(line) => {
                let row = line.split_whitespace().collect::<Vec<&str>>();
                assert!(row.len() == 3);
                graph.add_weighted_node(Node(row[0].parse().unwrap()), Cost(row[1].parse().unwrap()), Benefit(row[2].parse().unwrap()));
            },
            Err(e) => panic!(e)
        }
    }

    for res in lines.by_ref().take(edges) {
        match res {
            Ok(line) => {
                let row = line.split_whitespace().collect::<Vec<&str>>();
                assert!(row.len() == 3);
                graph.add_edge(Edge {
                    from: Node(row[0].parse().unwrap()),
                    to: Node(row[1].parse().unwrap()),
                    weight: Weight(row[2].parse().unwrap())
                });
            },
            Err(e) => panic!(e)
        }
    }

    let mut rng = thread_rng();
    let samples = sample(&mut rng, graph.nodes(), 10).iter().map(|&start| sample_ic (&graph, start)).collect::<Vec<HashSet<&Node>>>();
    println!("{}", samples.len());
}
