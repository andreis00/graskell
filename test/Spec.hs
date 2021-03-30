import Test.QuickCheck
import Data.Bits
import Graph

prop_MakeGraph :: [[Int]] -> Bool
prop_MakeGraph bmatrix = if null bmatrix then True -- ignore empty matrices
                         else check_csr graph_csr
                         where
                             graph = from_adj_matrix bmatrix 1.3
                             graph_csr = to_csr graph 

main :: IO ()
main = do
         quickCheckWith stdArgs { maxSuccess = 100} prop_MakeGraph

         putStrLn "Done!\n"
