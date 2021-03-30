module Main where

import Graph

{-
Main module tests some specific cases that are hard 
or impossible to get tested with QuickCheck 
-}

-- a few simple graph examples:
--
graph_test :: Graph Char Int
graph_test = [('A',[('C',1),('D',2)]), ('B',[('A',3),('C',4),('N',105)]), ('C',[('N',6)]), ('D',[('C',7)]), ('M',[('A',8),('B',9)]), ('N',[])]

-- for k-cores testing:
--
new_graph :: Graph Int Float
new_graph = [(0,[(1,1.0),(2,2.13)]), (1, [(2,2.13),(5,5.17)]), (2, [(3,3.01),(4,4.5),(5,5.17),(6,6.08)]), (3,[(4,4.5),(6,6.08),(7,7.11)]), (4,[(6,6.08),(7,7.11)]), (5, [(6,6.08),(8,8.9)]), (6, [(7,7.11),(8,8.9)]), (7, []), (8, [])]

topo_graph_t1 :: Graph Int Int
topo_graph_t1 = [(0,[]), (1,[]), (2,[(3,0)]), (3,[(1,0)]), (4,[(0,0), (1,0)]), (5,[(0,0),(2,0)])]
-- topo sort: [5,4,2,3,1,0]

topo_graph_t2 :: Graph Char Int
topo_graph_t2 = [ ('a',[('d',1)]), ('f',[('b',1), ('a',1)]), ('b',[('d',1)]), ('d',[('c',1)]), ('e',[])] 


assert :: Bool -> String -> String -> IO ()
assert ftest str_success str_fail = if ftest then putStrLn str_success
                                    else putStrLn str_fail

main :: IO ()
main = do
         assert (shortest_path graph_test 'M' 'N' == "MBN") "shortest path Passed." "shortest path Failed." 
         putStrLn "Done!\n"
