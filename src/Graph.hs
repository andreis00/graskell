{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graph where

import Data.List as L
import Data.Bits
import qualified Data.Map as M

-- graph type is a list of
-- tuples of vertices and 
-- their corresponding 
-- adjacency list of tuples of
-- outgoing vertices and the
-- corresponding edge information
--
type Graph a w = [(a,[(a,w)])]

-- this uses extractor to extract
-- toplogical info of given vertex
--
topo :: Eq a1 => [(a1, [a2])] -> a1 -> (a2 -> a3) -> [a3]
topo graph vertex extractor = if null graph then []
                              else if not $ elem vertex (map fst graph) then []
                                   else map extractor $ snd $ head $ filter (\pair -> fst pair == vertex) graph

-- given generic graph
-- renumber vertices in an increasing contiguous sequence of integers
--
renumber :: (Ord a) => Graph a t -> Graph Int t
renumber graph = foldl ( \accs (v, lps) -> accs ++ [( get_just $ M.lookup v map_vs, map (\ (src, w) -> ( get_just $ M.lookup src map_vs, w)) lps ) ] ) [] graph
                 where
                      all_vs = unique $ foldl (\accs (v, lps) -> (v:accs) ++ (map fst lps)) [] graph
                      map_vs = M.fromList $ zip all_vs [0..]
                      get_just = ( \(Just x) -> x )

-- convertor to CSR format (row_offsets, column_indices, values):
--
to_csr :: Ord a => Graph a w -> ([Int], [Int], [w])
to_csr graph = (row_offsets, col_indices, values)
               where
                    -- sorted_graph = sortBy (\(v1, _) (v2, _) -> compare v1 v2) graph -- no need to sort; `renumber` will take care of that
                    row_offsets = scanl (\n ls -> n + (length $ snd ls)) 0 numbered_graph
                    numbered_graph = renumber graph
                    col_indices = concat $ map (\p -> map fst $ snd p) numbered_graph
                    values = concat $ map (\p -> map snd $ snd p) numbered_graph


-- partition set ci according to exclusive scan ro 
--
partition_wscan :: [a] -> [Int] -> [[a]]
partition_wscan ci ro = map (\(pos, n) -> (take n . drop pos) ci) $ map (\k->(ro!!k, ro!!(k+1) - ro!!k)) [0..nvs-1]
                        where
                             nvs = length ro - 1

-- partition both ci and vs sets according to exclusive scan ro 
--
partition_wscan' :: ([Int], [a], [w]) -> [[(a, w)]]
partition_wscan' (ro, ci, vs) = map (\ (ls1, ls2) -> zip ls1 ls2) $ zip grp_cols grp_weights
                                where
                                    grp_cols = partition_wscan ci ro
                                    grp_weights = partition_wscan vs ro 

-- convertor to COO format ([(i,j,v)]: i = row index, j = column index, v = weight value of edge between (i,j))
--
to_coo ::  Ord a => Graph a w -> [(Int, Int, w)]
to_coo graph = (concat . map ( \ (row_cw, row) -> map (\ (col, weight) -> (row, col, weight) ) row_cw)) (zip cw_grps [0..])
               where
                    cw_grps = partition_wscan' $ to_csr graph


-- check CSR consistency:
--
check_csr :: ([Int], [Int], [w]) -> Bool
check_csr (ro, ci, vs) = (nvs > 0) && (head ro == 0) && non_decreasing &&  within_range && (length ci == nnz) && (length vs == nnz) && (sum_nz == nnz)
                                   && ((length $ concat edges_per_row) == length ci) -- this may not hold true for multi-graphs
                         where
                              nvs = (length ro) - 1
                              nnz = last ro
                              within_range = all (\col_indx -> col_indx < nvs) ci
                              non_decreasing = all (\k -> ro!!k <= ro!!(k+1) ) [0..nvs-1]
                              -- check sum of elements per row:
                              sum_nz = foldr (\k acc -> acc + (ro!!(k+1) - ro!!k)) 0 [0..nvs-1]
                              -- check for each row that there aren't column index repetitions:
                              edges_per_row = map unique $ partition_wscan ci ro

-- uniques of a set utility:
--
unique :: Ord a => [a] -> [a]
unique = map head . group . sort


-- convertor from CSR:
--
from_csr :: Num a => ([Int], [a], [w]) -> Graph a w
from_csr (row_offsets, col_indices, values) = map (\v_index -> (fromIntegral v_index, zip (group_cols!!v_index) (group_vals!!v_index)) ) [0..n_vertices-1]
                                              where
                                                   adj_diff ls = map (\k -> ls!!(k+1) - ls!!k) [0..length ls - 2]
                                                   ns_per_row = adj_diff row_offsets
                                                   n_vertices = length ns_per_row
                                                   extract n ls = (take n ls, drop n ls)
                                                   bucketize ls lengths = map fst $ tail lls
                                                                          where
                                                                               lls = foldl (\accs n -> accs ++ [extract n $ snd $ last accs]) [([],ls)] lengths
                                                   group_cols = bucketize col_indices ns_per_row
                                                   group_vals = bucketize values ns_per_row

-- check if graph is undirected:
--                                                   
check_undir :: Eq a => Graph a w -> Bool
check_undir graph = all (\ (src, _) -> all (\dst -> dst `has_edge_to` src) (adjacency graph src) ) graph
                    where
                         has_edge_to src dst = let src_entry = filter ( \ (v, lsp) -> v == src) graph
                                               in
                                                 (not $ null src_entry) && dst `elem` adjacency graph (fst $ head src_entry) 


check_self_loops :: Eq a => Graph a w -> Bool
check_self_loops graph = any ( \ (src, lsp) -> src `elem` (adjacency graph src)) graph

-- this gives the list of outgoing vertices
-- for given vertex
--
adjacency :: Eq a => Graph a w -> a -> [a]
adjacency graph vertex = topo graph vertex fst

-- gives the list of outgoing pairs (v, edge)
-- from given vertex;
-- Note: id = identity function id x = x
--
full_adjacency :: Eq a => Graph a w -> a -> [(a,w)]
full_adjacency graph vertex = topo graph vertex id

edges :: Eq a => Graph a w -> a -> [w]
edges graph vertex = topo graph vertex snd

-- this gives the list of incoming vertices
-- for given vertex
--
incidency :: Eq a => Graph a w -> a -> [a]
incidency graph vertex = map fst (filter (\pair -> vertex `elem` (map fst $ snd pair)) graph)

-- remove vertex from graph
-- (curried);
-- note: 
-- vertex must also be removed 
-- from all the adjacency lists
--                              
remove_vertex :: Eq a => a -> Graph a w -> Graph a w
remove_vertex vertex graph = map (\pair -> (fst pair, filter (\(x, _) -> x /= vertex) $ snd pair)) $ filter (\pair -> fst pair /= vertex) graph

-- remove highest degree vertex:
--
remove_highest :: Eq a => Graph a w -> (Graph a w -> a -> Int) -> Graph a w
remove_highest graph f_degree = remove_vertex v_to_remove graph
                                where
                                     v_to_remove = maximumBy (\v1 v2 -> compare (f_degree graph v1) (f_degree graph v2)) vs
                                     vs = map fst graph

-- remove disconnected vertices:
--
remove_isolated :: Eq a => Graph a w -> Graph a w
remove_isolated graph = filter (\ (v,_) -> (not . elem v) vs_isolated) graph
                       where
                            vs_out_deg0 = map fst $ filter (\ (_, lst_out) -> null lst_out) graph
                            is_sink v = any (\ (_, lst_sinks) -> elem v $ map fst lst_sinks) graph 
                            vs_isolated = filter (\vout_d0 -> (not . is_sink) vout_d0) vs_out_deg0


-- generate all paths in graph
-- from vertex start;
-- cyclic graphs should be okay
-- because starting vertices are being
-- removed from graph while traversing
--
paths_from :: Eq a => Graph a w -> a -> [[a]]
paths_from graph start = if null next_lst then [[start]]
                         else map (start:) lst_paths -- prepend start to each lst in lst_paths
                         where
                              next_lst = adjacency graph start
                              lst_paths = foldl (\accs v -> (paths_from (remove_vertex start graph) v) ++ accs) [] next_lst

-- generate all paths between start and stop
--
paths_from_to :: Eq a => Graph a w -> a -> a -> [[a]]
paths_from_to graph start stop = map (\ls -> truncate_at stop ls) paths_with_stop
                                 where
                                      paths_from_start = paths_from graph start
                                      paths_with_stop = filter (\ls -> stop `elem` ls) paths_from_start
                                      get_index vertex lst = head [i | i <- [0..length lst -1], lst!!i == vertex]
                                      truncate_at x lst = let index = get_index x lst in take (index+1) lst

-- generate all full paths (v_src, v_dest, edge) in graph
-- from vertex start;
-- (work in progress)
--
-- full_paths_from :: Eq a => Graph a w -> a -> [[(a,a,w)]]
-- full_paths_from graph start = if null next_lst then [[(start,?,?)]]
--                         else map (start:) lst_paths -- ???
--                         where
--                              next_lst = map (\(v,e) -> (start, v, e)) $ full_adjacency graph start
--                              lst_paths = foldl (\accs t -> (full_paths_from (remove_vertex start graph) (fst t)) ++ accs) [] next_lst

-- shortest path between 2 vertices
--
shortest_path :: Eq a => Graph a w -> a -> a -> [a]
shortest_path graph start stop = minimumBy (\xs ys -> compare (length xs) (length ys)) $ paths_from_to graph start stop

-- given a path of vertices in a graph,
-- generate the list of vertices and
-- corresponding outgoing edges 
-- that make the path
--
edges_of_path :: Eq a => Graph a w -> [a] -> [(a,w)]
edges_of_path graph lstv = if length lstv < 2 || null pnext then []
                           else
                               (start, edge):(edges_of_path graph tlstv) 
                           where
                                edge = snd $ head $ pnext
                                pnext = filter (\p -> fst p == next) adj_start
                                adj_start = full_adjacency graph start
                                start = head lstv
                                tlstv = tail lstv
                                next = head tlstv


-- cheapest path between 2 vertices
--
cheapest_path :: (Eq a, Ord w, Num w) => Graph a w -> a -> a -> (w -> w -> w) -> [a]
cheapest_path graph start stop reductor = minimumBy (\xs ys -> let
                                                                  costx = foldl (\acc p -> reductor acc $ snd p) 0 $ edges_of_path graph xs;
                                                                  costy = foldl (\acc p -> reductor acc $ snd p) 0 $ edges_of_path graph ys;
                                                               in compare costx costy) $ paths_from_to graph start stop


-- subgraph: by vertex list, vs
--
subgraph :: Eq a => Graph a w -> [a] -> Graph a w
subgraph graph vs = foldl (\gr v -> remove_vertex v gr) graph complement_vs -- fold (reduce) graph by removing all vertices not in vs
                    where
                         complement_vs = [fst tpl | tpl <- graph, not $ fst tpl `elem` vs] -- complement of vs


-- out-degree of vertex
--
out_degree :: Eq a => Graph a w -> a -> Int
out_degree graph v = length $ snd $ head [x | x <- graph, fst x == v]

-- in-degree of vertex
--
in_degree :: Eq a => Graph a w -> a -> Int
in_degree graph v = foldl (\accs tpl -> if v `elem` (map fst $ snd tpl) then (accs+1) else accs) 0 graph

-- full degree: in + out
--
full_degree :: Eq a => Graph a w -> a -> Int
full_degree graph v = in_degree graph v + out_degree graph v



-- contract 2 vertices into one; args: 
-- original: graph
-- the destination vertex for contraction: vstays
-- the source vertex for contraction: vleaves
-- a function to combine the edges: combiner
--
contract :: (Eq a, Ord a, Eq w, Ord w) => Graph a w -> a -> a -> (w -> w -> w) -> Graph a w
contract graph vstays vleaves combiner = if null (find_entry graph vstays) || null (find_entry graph vleaves) || vstays == vleaves then graph
                                         else
                                             -- create new entry from vstays and merged adjacency list
                                             -- into the graph with new adjacency info minus the entries
                                             -- for the 2 combined vertices:
                                             --
                                             (vstays, adj_merged):(filter (\pair -> fst pair /= vstays && fst pair /= vleaves) graph_adj_new)
                                         where
                                              -- merge the groups in grps_union by reducing all elements
                                              -- in a group into one via combiner
                                              --
                                              adj_merged = map (reduce combiner) grps_union
                                              --helper function: reduce a given group via given reducer
                                              --
                                              reduce reducer lstp = foldl (\(vacc, eacc) (v, e) -> (v, reducer eacc e)) (head lstp) (tail lstp)
                                              -- group by vertex the sorted adj_union
                                              --
                                              grps_union = groupBy (\(v1, e1) (v2, e2) -> v1 == v2) $ sort adj_union
                                              -- union of adjacency lists of the two contracted vertices:
                                              --
                                              adj_union = concat $ map snd $ filter (\pair -> fst pair == vstays || fst pair == vleaves) graph_adj_new
                                              -- generate graph with new adjacency info by mapping the replacer below on given graph:
                                              --
                                              graph_adj_new = map (\pair -> let parent = fst pair in (parent, replace (snd pair) vleaves vstays)) graph
                                              -- find entry in graph g for vertex v
                                              --
                                              find_entry g v = filter (\pair -> fst pair == v) g
                                              --
                                              -- replacer: for each element in adjacency list of pairs, lstp:
                                              -- if v has edges to both vin, vout: replace (vin, e1), (vout, e2) with (vin, combiner e1 e2)
                                              -- else if v has edge to vout then replace (vout, e) with (vin, e) else nothing;
                                              --
                                              replace lstp vout vin = let lstv = (map fst lstp) in
                                                                      if vout `elem` lstv then
                                                                          let no_io_p  = [p | p<-lstp, fst p /= vout && fst p /= vin];
                                                                              e_out = snd $ head [p | p<-lstp, fst p == vout];
                                                                              e_in  = snd $ head [p | p<-lstp, fst p == vin]
                                                                          in
                                                                              if vin `elem` lstv then no_io_p ++ [(vin, combiner e_in e_out)]
                                                                              else no_io_p ++ [(vin, e_out)]
                                                                      else lstp


-- k-cores algorithm
-- remove recursively all vertices
-- of (in+out) degree less than given deg
--
k_cores :: Eq a => Graph a w -> Int -> Graph a w
k_cores graph deg = if not $ null lst_less_deg then k_cores subg_deg_more deg else subg_deg_more
                    where
                         subg_deg_more = subgraph graph lst_ge_deg
                         lst_ge_deg = map fst $ filter (\pair -> not $ elem (fst pair) lst_less_deg) graph
                         lst_less_deg = map fst $ filter (\pair -> let v = fst pair in in_degree graph v + out_degree graph v < deg) graph

-- k-cores algorithm with given degree function
-- remove recursively all vertices
-- of (in+out) degree less than given deg
--
k_cores' :: Eq a => Graph a w -> Int -> (Graph a w -> a -> Int) -> Graph a w
k_cores' graph deg deg_extractor = if not $ null lst_less_deg then k_cores' subg_deg_more deg deg_extractor else subg_deg_more
                                   where
                                        subg_deg_more = subgraph graph lst_ge_deg
                                        lst_ge_deg = map fst $ filter (\pair -> not $ elem (fst pair) lst_less_deg) graph
                                        lst_less_deg = map fst $ filter (\pair -> let v = fst pair in deg_extractor graph v < deg) graph

-- degeneracy = k-core number = max(deg) 
-- s.t. not null k_cores graph deg;
-- (invariant to isomorphism) 
--
degeneracy :: Eq a => Graph a w -> Int
degeneracy graph = (head $ take 1 [deg | deg <- [1..], null $ k_cores graph deg]) - 1

-- idea: compute k-core number of each vertex v;
-- i.e., largest deg for which v belongs to (k_cores graph deg);
-- this should be invariant to isomorphism;


-- per-vertex k-core number
-- given a graph, generate the list of 
-- (vertex, k-core#(vertex)) pairs
-- (inefficient)
--
vertex_k_cores :: Eq a => Graph a w -> [(a, Int)]
vertex_k_cores graph = foldl (\accs new_lst -> [p | p <- accs, not $ elem (fst p) (map fst new_lst)] ++ new_lst) (head lst_lsts) (tail lst_lsts)
                       where
                            lst_lsts = lstv_k_cores graph 1
                            -- given a graph and a starting crt_deg
                            -- generate the list of lists of
                            -- (vertex, deg) pairs for each deg
                            -- increasing from crt_deg up to the
                            -- point where the k_core graph deg == []
                            --
                            lstv_k_cores graph crt_deg  =  if not $ null crt_core then
                                  crt_lst_cores : (lstv_k_cores graph (crt_deg+1))
                               else
                                  []
                               where
                                    crt_lst_cores = map (\(v,_) -> (v, crt_deg)) crt_core
                                    crt_core = k_cores graph crt_deg

-- topological sort
-- utilities:
-- see https://www.geeksforgeeks.org/topological-sorting/
--
newtype Stack a = Stack [a] deriving Show

pop :: Stack a -> Stack a
pop (Stack ls) = Stack (tail ls)

push :: Stack a -> a -> Stack a
push (Stack ls) x = Stack (x:ls)

top ::Stack a -> a
top (Stack ls) = head ls

was_visited :: Ord vertex_t => vertex_t -> M.Map vertex_t Bool -> Bool
was_visited x visited = if (M.lookup x visited == Nothing) || (M.lookup x visited == Just False) then False else True

set_visited :: Ord vertex_t => M.Map vertex_t Bool -> vertex_t -> Bool -> M.Map vertex_t Bool
set_visited map_v_b v flag = if M.lookup v map_v_b == Nothing then
                                M.insert v flag map_v_b
                             else
                                M.update (\_ -> Just flag) v map_v_b

accumulate_visit :: Ord vertex_t => Graph vertex_t w_t -> (Stack vertex_t, M.Map vertex_t Bool) -> [vertex_t] -> (Stack vertex_t, M.Map vertex_t Bool)
accumulate_visit graph (stack, visited) = foldl ( \ (acc_stack, acc_visited) w -> 
                                                    if (was_visited w acc_visited) then (acc_stack, acc_visited)
                                                    else topo_util graph w acc_stack acc_visited
                                                ) (stack, visited)

topo_util :: Ord vertex_t => Graph vertex_t w_t -> vertex_t -> Stack vertex_t -> M.Map vertex_t Bool -> (Stack vertex_t, M.Map vertex_t Bool)
topo_util graph v stack map_visited = (new_stack, new_visited)
         where
            adj_v = adjacency graph v
            visited = set_visited map_visited v True
            adj_not_visited = filter ( \ x -> not (was_visited x visited) ) adj_v
            pair_st_mp = accumulate_visit graph (stack, visited) adj_not_visited   
            new_stack = push (fst pair_st_mp) v
            new_visited = snd pair_st_mp
            
topo_sort :: Ord vertex_t => Graph vertex_t weight_t -> [vertex_t]
topo_sort graph = ls
         where
           vertex_set = map fst graph
           (Stack ls, _) = accumulate_visit graph (empty_stack, empty_map) vertex_set
           empty_stack = Stack [] :: Stack vertex_t
           empty_map = M.fromList [] -- M.fromList $ zip vertex_set $ repeat False


-- Strongly Connected Components via Adjacency Matrix 
-- (see Gilbert's "Graph Algorithms in the Language of Linear Algebra") 
{-
Let semiring (&, |) := (Combine, Reduce), where Combine == logical AND, Reduce == logical OR;
Let C^k = A (&, |) ... (&, |) A (k times);

Compute C = I | C | C^2 | C^3 | ...
until C doesn't change anymore;
if A is non-symmetric then let R := C & transpose(C); else leave R = C;

Extract components as contiguous square blocks in R;  
-}
-- row-wise representation:
--
type Matrix a = [[a]]

-- multiply 2 matrices with given semiring (combine, reduce)
-- neutral_r = identity element for reduction
--
multiply :: (Num a, Eq a) => (a->a->a) -> (a->a->a) -> a -> Matrix a -> Matrix a -> Matrix a
multiply combine reduce neutral_r ma mb = [[inner combine reduce rowa colb | colb <- transpose mb] | rowa <- ma]
                                where
                                     inner cf rf r c = foldl (\acc x -> rf acc x) neutral_r [cf (r!!k) (c!!k) | k <-[0..l-1]]
                                                 where
                                                      l = length r

-- map binary operator to elements of 2 matrices:
--
map_op :: (a->a->a) -> Matrix a -> Matrix a -> Matrix a
map_op f m1 m2 = map (\(ls1, ls2) -> map (\(x, y) -> f x y) $ zip ls1 ls2) $ zip m1 m2


-- Identity matrix of order n:
--
eye :: a -> a -> Int -> Matrix a
eye one zero n = foldl (\accs k -> accs ++ [rotate_r start k]) [] [1..n]
                 where
                      start = (take (n-1) $ repeat zero) ++ [one]
                      rotate_r lst k = let n = length lst - k in drop n lst ++ take n lst 

-- Iterate Strongly Connected Components:
--
-- args: 
--      ma = adjacency matrix A;
--      m0 = identity matrix: I (eye)
--      mr = I + A
--
run_scc  :: (Num a, Bits a) => Matrix a -> Matrix a -> Matrix a -> Int -> (Int, Matrix a)
run_scc ma m0 mr counter = if mrun == mr then (counter, mr) else run_scc ma m0 mrun (counter+1)
               where
                    mres1 = multiply (.&.) (.|.) 0 mr ma
                    mrun = map_op (.|.) m0 mres1


-- Strongly Connected Components wrapper:
-- args:
--      adjacency (boolean) matrix;
--      returns (nr_iterations, C && C^T)
--
strongly_cc :: (Num a, Bits a) => Matrix a -> (Int, Matrix a)
strongly_cc ma = (counter, map_op (.&.) c ct)
                 where
                      i_n = eye 1 0 nrows
                      starter = map_op (.|.) i_n ma
                      (counter, c) = run_scc ma i_n starter 1
                      ct = transpose c
                      nrows = length ma

-- extract labels from SCC matrix (C^Ct)
-- Note: labels are _not_ forming 
-- a sorted or even contiguous set
--
labels_scc :: (Num a, Eq a) => Matrix a -> [Maybe Int]
labels_scc mcct = map (\ls -> let fs = first ls in if null fs then Nothing else Just (head fs)) mcct
                  where
                       ncols = length mcct
                       first ls = take 1 $ filter (\k -> ls!!k /= 0) [0..ncols-1]

-- convert to adjacency matrix utility:
--
to_adj_matrix :: [(Int, Int, w)] -> Int -> [[Int]]
to_adj_matrix ijvs nrows = map (\i -> let ci = map (\(_,j,_) -> j) (filter (\(k,_,_) -> k==i) ijvs) 
                                      in map (\k -> if k `elem` ci then 1 else 0) [0..nrows-1]) [0..nrows-1]


-- make graph from arbitrary adjacency matrix
--
from_adj_matrix :: (Num a, Bits a) => [[a]] -> w -> Graph Int w
from_adj_matrix bmatrix val = from_csr (row_offsets, col_indices, replicate num_edges val)
                          where
                              nrows = maximum $ map length bmatrix
                              square_bmatrix = [ row | i <- [0..nrows-1], let row = [ if (i<length bmatrix && j < length (bmatrix!!i)) then (bmatrix!!i)!!j else 0 | j <- [0..nrows-1]]]
                              ncols_per_row = map (\i -> length $ filter (\elem -> elem /=0) (square_bmatrix!!i)) [0..nrows-1]
                              row_offsets = scanl (+) 0 ncols_per_row
                              col_sets = map ( \i -> filter ( \j -> (square_bmatrix!!i)!!j /=0 ) [0..nrows-1] ) [0..nrows-1]
                              col_indices = foldr (\ls accs -> ls ++ accs) [] col_sets
                              num_edges = length col_indices


-- extract SCC labels from Graph
-- Note: labels are _not_ forming 
-- a sorted or even contiguous set
--
labels_scc' :: (Num a, Eq a, Ord a) => Graph a w -> [Maybe Int]
labels_scc' graph = labels_scc graph_matrix
                    where
                        graph_coo = to_coo graph
                        graph_matrix = to_adj_matrix graph_coo nrows
                        nrows = length graph -- TODO: check validity of adjacency list for sink vertices
