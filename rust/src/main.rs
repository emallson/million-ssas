mod graph;
mod ris;

extern crate rand;

use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use std::str::FromStr;

use graph::*;
use ris::*;
use rand::{thread_rng};
use rand::distributions::{Weighted, WeightedChoice, IndependentSample};

use std::collections::BTreeSet;

fn max_cover<'a>(sets: &Vec<BTreeSet<&Node>>, budget: &Cost, graph: &'a SequentialGraph) -> (BTreeSet<&'a Node>, f32) {

    let mut seeds = BTreeSet::new();
    let mut total_cost = Cost(0f32);
    let mut total_coverage = 0u32;
    let mut nodes = graph.nodes().iter().collect::<Vec<&Node>>();
    // cache the values of the objective function (coverage_cache) and
    // which entries of coverage_cache each node impacts (rr_cache)
    //
    // INVARIANT: At each point in time, coverage_cache[k] should be
    // exactly equal to the number of RR sets covered by k that are
    // not covered by the seed set.
    let mut coverage_cache = vec![0u32; graph.count_nodes()];
    let mut rr_cache = vec![Vec::new(); graph.count_nodes()];
    let mut covered = vec![false; sets.len()];

    // for each RR set, add 1 to the coverage of each node contained
    // and add the RR set index to the rr_cache for the node.
    for (i, rr) in sets.iter().enumerate() {
        for &&Node(node) in rr {
            coverage_cache[node as usize] += 1;
            rr_cache[node as usize].push(i);
        }
    }

    nodes.sort_by_key(|&&Node(n)| coverage_cache[n as usize]);

    while total_cost < *budget  && !nodes.is_empty() {
        // loop until we find an affordable node; pop() is O(1) and
        // reduces the workload of future sorts
        while let Some(node) = nodes.pop() {
            let cost = graph.get_cost(node).unwrap();
            // only add the node if we can afford it
            if *cost + total_cost <= *budget {
                seeds.insert(node);
                total_cost = total_cost + *cost;
                total_coverage += coverage_cache[node.0 as usize];
                assert!(total_coverage as usize <= sets.len());

                let node_rrs = rr_cache[node.0 as usize].clone();
                // for each RR set covered by `node`...
                for i in node_rrs {
                    // if the RR set is not already covered
                    if !covered[i] {
                        // for each node `other` in that RR set...
                        for &&Node(other) in &sets[i] {
                            // reduce its cached coverage by 1
                            coverage_cache[other as usize] -= 1;
                        }
                        covered[i] = true;
                    }
                }

                assert!(coverage_cache[node.0 as usize] == 0);

                nodes.sort_by_key(|&&Node(n)| coverage_cache[n as usize]);
                break;
            }
        }
    }

    (seeds, graph.count_nodes() as f32 * total_coverage as f32 / sets.len() as f32)
}

fn estimate_inf(wc: &WeightedChoice<&Node>, graph: &SequentialGraph, seeds: &BTreeSet<&Node>, epsilon_2: f32, delta_2: f32, t_max: u32) -> f32 {
    let lambda_2 = 1.0 + (2.0 + 2.0 * epsilon_2 / 3.0) * (1.0 + epsilon_2) * (1.0/delta_2).ln() / epsilon_2.powf(2.0);
    let mut cov = 0;

    let mut rng = thread_rng();

    for t in 1..t_max+1 {
        let sample = sample_ic(graph, wc.ind_sample(&mut rng));
        if seeds.intersection(&sample).count() > 0 {
            cov += 1;
        }

        if cov as f32 >= lambda_2 {
            return graph.count_nodes() as f32 * cov as f32 / t as f32;
        }
    }

    println!("Failed to reach coverage threshold in {} iterations. Cov: {}, Lambda_2: {}", t_max, cov, lambda_2);

    return -1.0;
}

fn binom(n: u32, k: u32) -> f32 {
    let mut choose = 1.0;
    for i in 1..k+1 {
        choose *= (n as f32 + 1.0 - i as f32)/i as f32;
    }
    return choose;
}

fn main()
{
    let f = File::open("/home/emallson/Code/cikm16/datasets/_processed/ca-GrQc.txt").unwrap();
    let mut reader = BufReader::new(f);

    let mut first_line = String::new();

    reader.read_line(&mut first_line).unwrap();

    let first_row = first_line.split_whitespace().map(|s| usize::from_str(s).unwrap()).collect::<Vec<usize>>();
    assert!(first_row.len() == 2);
    let (nodes, edges) = (first_row[0], first_row[1]);

    let mut graph = SequentialGraph::new(&nodes, &edges);

    let mut lines = reader.lines();
    println!("{} {}", nodes, edges);

    for res in lines.by_ref().take(nodes) {
        match res {
            Ok(line) => {
                let row = line.split_whitespace().collect::<Vec<&str>>();
                assert!(row.len() == 3);
                graph.add_weighted_node(Node(row[0].parse::<u64>().unwrap()-1), Cost(row[1].parse().unwrap()), Benefit(row[2].parse().unwrap()));
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
                    from: Node(row[0].parse::<u64>().unwrap() - 1),
                    to: Node(row[1].parse::<u64>().unwrap() - 1),
                    weight: Weight(row[2].parse().unwrap())
                });
            },
            Err(e) => panic!(e)
        }
    }

    let mut rng = thread_rng();
    // the rand crate requires u32 weights, so we re-scale the
    // benefits by 1/sum(benefits) and then multiply by MAX/2 to put
    // them on reasonably distinct integers.
    let total_benefit = graph.nodes().iter().fold(0f32, | accum, ref node | {
        accum + graph.get_benefit(node).unwrap().0
    });
    let mut weights: Vec<Weighted<&Node>> = graph.nodes().iter().filter_map(| node | {
        match graph.get_benefit(&node) {
            Some(&Benefit(b)) => Some(Weighted {
                weight: ((b / total_benefit) * (std::u32::MAX/2) as f32) as u32,
                item: node
            }),
            None => None
        }
    }).collect();
    let wc = WeightedChoice::new(&mut weights);

    let n = graph.count_nodes() as u32;
    let k = 50;
    let epsilon = 0.1;
    let delta = 1.0 / graph.count_nodes() as f32;
    let epsilon_1 = epsilon/6.0;
    let epsilon_2 = epsilon/2.0;
    let epsilon_3 = epsilon/4.0 * (1.0 - 1.0 / std::f32::consts::E);
    let lambda_1 = (1.0 + epsilon_1) * (1.0 + epsilon_2) * (2.0 + 2.0 * epsilon_3 / 3.0) * (3.0 / delta).ln() / epsilon_3.powf(2.0);
    let sample_bound = (8.0 + 2.0 * epsilon) * graph.count_nodes() as f32 * ((2.0/delta).ln() + binom(n, k))/epsilon.powf(2.0);
    let t_max_base = ((1.0 + epsilon_2) / (1.0 - epsilon_2)) * (epsilon_3.powf(2.0) / epsilon_2.powf(2.0));

    println!("Lambda_1 = {}", lambda_1);

    let mut samples: Vec<BTreeSet<&Node>> = (0..lambda_1 as u32).map(|_| {
        let node = wc.ind_sample(&mut rng);
        sample_ic(&graph, node)
    }).collect();

    let mut round = 0;
    while (samples.len() as f32) < sample_bound {
        let (seeds, cov_inf) = max_cover(&samples, &Cost(k as f32), &graph);
        round += 1;
        println!("Round {}", round);

        if cov_inf * (samples.len() as f32 / n as f32) >= lambda_1 {
            let est_inf = estimate_inf(&wc, &graph, &seeds, epsilon_2, delta / 3.0, (samples.len() as f32 * t_max_base) as u32);

            println!("Cov: {}, Est: {}", cov_inf, est_inf);
            if cov_inf <= (1.0 + epsilon_1) * est_inf {
                println!("Total samples: {}", samples.len());
                for &Node(n) in seeds {
                    print!("{} ", n);
                }
                println!("");
                break;
            }
        }

        if (2 * samples.len()) as f32 >= sample_bound {
            println!("Unable to solve within sample bound.");
            break;
        }

        let rsize = samples.len();
        samples.extend((0..rsize).map(|_| {
            sample_ic(&graph, wc.ind_sample(&mut rng))
        }));
    }
}
