This is a collection of graph algorithms in Haskell, currently supporting:
(1.) representations such as:
     - adjacency list;
     - COO format;
     - CSR format;
     - adjacency matrix;
(2.) various convertors between them;
(3.) graph algorithms such as:
     - renumbering;
     - single source shortest path;
     - subgraph extraction;
     - graph contraction;
     - k-cores;
     - topological sort;
     - srongly connected components;

Features goals: graph algorithms on the GPU (via `accelerate` library).
Author: Andrei Schaffer, andreis@iprogramthr4iam.com
