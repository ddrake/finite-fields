import Data.List
import qualified Data.Map as M

-- RESULTS:
-- Constructing each of the p^(n+1) possible polynomials of degree n and evaluating each polynomial at each of the 
-- p possible domain values gives the surprising (to me) result that the graphs of the functions are distinct as long
-- as n < p

-- example: In Z5, the 5^5 possible polynomials of degree 4 are all distinct functions 
-- and hence span the entire space of 5^5 possible graphs

--*Main> let ms = mranges coeff55 [0..4]
--*Main> length . nub $ ms
--3125


-- It seems clear that this nice result comes from the fact that p is prime so Zp is a field
-- If we test all the 6^6 (46,656) polynomials of degree 5 in Z6, we find only 3*6^2 = 108 distinct graphs
-- Each of these graphs is common to 2*6^3 = 432 distinct polynomials

--*Main> :l polynomials.hs 
--[1 of 1] Compiling Main             ( polynomials.hs, interpreted )
--Ok, modules loaded: Main.
--*Main> let ms = mranges coeff66 [0..5]
--*Main> let hs = histo ms coeff66
--*Main> let mhs = M.fromList hs
--*Main> M.size mhs
--108
--*Main> M.toList mhs
-- [([0,0,0,0,0,0],432),([0,0,2,0,0,2],432),([0,0,4,0,0,4],432),([0,1,0,3,4,3],432),([0,1,2,3,4,5],432),([0,1,4,3,4,1],432),
--  ([0,2,0,0,2,0],432),([0,2,2,0,2,2],432),([0,2,4,0,2,4],432),([0,3,0,3,0,3],432),([0,3,2,3,0,5],432),([0,3,4,3,0,1],432),
--  ([0,4,0,0,4,0],432),([0,4,2,0,4,2],432),([0,4,4,0,4,4],432),([0,5,0,3,2,3],432),([0,5,2,3,2,5],432),([0,5,4,3,2,1],432),
--  ([1,0,1,4,3,4],432),([1,0,3,4,3,0],432),([1,0,5,4,3,2],432),([1,1,1,1,1,1],432),([1,1,3,1,1,3],432),([1,1,5,1,1,5],432),
--  ([1,2,1,4,5,4],432),([1,2,3,4,5,0],432),([1,2,5,4,5,2],432),([1,3,1,1,3,1],432),([1,3,3,1,3,3],432),([1,3,5,1,3,5],432),
--  ([1,4,1,4,1,4],432),([1,4,3,4,1,0],432),([1,4,5,4,1,2],432),([1,5,1,1,5,1],432),([1,5,3,1,5,3],432),([1,5,5,1,5,5],432),
--  ([2,0,0,2,0,0],432),([2,0,2,2,0,2],432),([2,0,4,2,0,4],432),([2,1,0,5,4,3],432),([2,1,2,5,4,5],432),([2,1,4,5,4,1],432),
--  ([2,2,0,2,2,0],432),([2,2,2,2,2,2],432),([2,2,4,2,2,4],432),([2,3,0,5,0,3],432),([2,3,2,5,0,5],432),([2,3,4,5,0,1],432),
--  ([2,4,0,2,4,0],432),([2,4,2,2,4,2],432),([2,4,4,2,4,4],432),([2,5,0,5,2,3],432),([2,5,2,5,2,5],432),([2,5,4,5,2,1],432),
--  ([3,0,1,0,3,4],432),([3,0,3,0,3,0],432),([3,0,5,0,3,2],432),([3,1,1,3,1,1],432),([3,1,3,3,1,3],432),([3,1,5,3,1,5],432),
--  ([3,2,1,0,5,4],432),([3,2,3,0,5,0],432),([3,2,5,0,5,2],432),([3,3,1,3,3,1],432),([3,3,3,3,3,3],432),([3,3,5,3,3,5],432),
--  ([3,4,1,0,1,4],432),([3,4,3,0,1,0],432),([3,4,5,0,1,2],432),([3,5,1,3,5,1],432),([3,5,3,3,5,3],432),([3,5,5,3,5,5],432),
--  ([4,0,0,4,0,0],432),([4,0,2,4,0,2],432),([4,0,4,4,0,4],432),([4,1,0,1,4,3],432),([4,1,2,1,4,5],432),([4,1,4,1,4,1],432),
--  ([4,2,0,4,2,0],432),([4,2,2,4,2,2],432),([4,2,4,4,2,4],432),([4,3,0,1,0,3],432),([4,3,2,1,0,5],432),([4,3,4,1,0,1],432),
--  ([4,4,0,4,4,0],432),([4,4,2,4,4,2],432),([4,4,4,4,4,4],432),([4,5,0,1,2,3],432),([4,5,2,1,2,5],432),([4,5,4,1,2,1],432),
--  ([5,0,1,2,3,4],432),([5,0,3,2,3,0],432),([5,0,5,2,3,2],432),([5,1,1,5,1,1],432),([5,1,3,5,1,3],432),([5,1,5,5,1,5],432),
--  ([5,2,1,2,5,4],432),([5,2,3,2,5,0],432),([5,2,5,2,5,2],432),([5,3,1,5,3,1],432),([5,3,3,5,3,3],432),([5,3,5,5,3,5],432),
--  ([5,4,1,2,1,4],432),([5,4,3,2,1,0],432),([5,4,5,2,1,2],432),([5,5,1,5,5,1],432),([5,5,3,5,5,3],432),([5,5,5,5,5,5],432)]

-- make a polynomial function from a list of coefficients
mpoly :: Int -> [Int] -> Int -> Int
mpoly base cs = \x -> sum (map (\y -> cs!!y*x^y)  [0..(length cs)-1]) `rem` base

-- make a list of polynomial functions from a list of coefficient lists
mpolys :: Int -> [[Int]] -> [Int -> Int]
mpolys base = map (mpoly base)

-- given a list of polynomial candidates, each in the form of a coefficient list,
-- along with a sample graph, return the list of all possible graphs. 
mranges :: [[Int]] -> [Int] -> [[Int]]
mranges cands desired =   let base = length desired
                              xs = [0..base-1]
                              polys = mpolys base cands
                          in  map (\poly -> map poly xs) polys

-- given a list of polynomial candidates, find the ones which produce the desired graph
matches :: [[Int]] -> [Int] -> [[Int]]
matches cands desired = let ranges = mranges cands desired
                        in  filter (== desired) ranges

-- given a list of bins (graphs) and a list of values (coeffs), return a "histogram" in the form of an assoc list
histo :: [[Int]] -> [[Int]] -> [([Int], Int)]
histo bins vals = let nb = nub bins
                      zbv  = zip bins vals
                  in [(b, length . filter (\(bn,v) -> bn == b) $ zbv) | b <- bins]

------------
--  DATA
------------

-- All polynomials of degree 2 over Z5
coeff53 :: [[Int]]
coeff53 = [[x,y,z] | x <- [0..4], y <- [0..4], z <- [0..4]]

coeff54 :: [[Int]]
coeff54 = [[x,y,z,w] | x <- [0..4], y <- [0..4], z <- [0..4], w <- [0..4]]

coeff55 :: [[Int]]
coeff55 = [[x,y,z,w,v] | x <- [0..4], y <- [0..4], z <- [0..4], w <- [0..4], v <- [0..4]]

coeff56 :: [[Int]]
coeff56 = [[x,y,z,w,v,u] | x <- [0..4], y <- [0..4], z <- [0..4], w <- [0..4], v <- [0..4], u <- [0..4]]

coeff63 :: [[Int]]
coeff63 = [[x,y,z] | x <- [0..5], y <- [0..5], z <- [0..5]]

coeff64 :: [[Int]]
coeff64 = [[x,y,z,w] | x <- [0..5], y <- [0..5], z <- [0..5], w <- [0..5]]

coeff65 :: [[Int]]
coeff65 = [[x,y,z,w,v] | x <- [0..5], y <- [0..5], z <- [0..5], w <- [0..5], v <- [0..5]]

coeff66 :: [[Int]]
coeff66 = [[x,y,z,w,v,u] | x <- [0..5], y <- [0..5], z <- [0..5], w <- [0..5], v <- [0..5], u <- [0..5]]

coeff73 :: [[Int]]
coeff73 = [[x,y,z] | x <- [0..6], y <- [0..6], z <- [0..6]]

coeff74 :: [[Int]]
coeff74 = [[x,y,z,w] | x <- [0..6], y <- [0..6], z <- [0..6], w <- [0..6]]

coeff75 :: [[Int]]
coeff75 = [[x,y,z,w,v] | x <- [0..6], y <- [0..6], z <- [0..6], w <- [0..6], v <- [0..6]]

coeff76 :: [[Int]]
coeff76 = [[x,y,z,w,v,u] | x <- [0..6], y <- [0..6], z <- [0..6], w <- [0..6], v <- [0..6], u <- [0..6]]

test1 :: [Int]
test1 = [1,3,0,5,6,2,4]

test2 :: [Int]
test2 = [0,5,6,3,3,6,5]

test3 :: [Int]
test3 = [0,4,3,4,0,5,5]

