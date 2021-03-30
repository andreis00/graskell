This is a (toy) collection of graph algorithms in Haskell, currently supporting:

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

Future goals: graph algorithms on the GPU (via `accelerate` library).
Author: Andrei Schaffer, andreis@iprogramthr4iam.com

The project is built with `stack`. Main functionality is in `src/Graph.hs`.Specific tests in `app/Main.hs`. QuickCheck tests in `test/Spec.hs`.
