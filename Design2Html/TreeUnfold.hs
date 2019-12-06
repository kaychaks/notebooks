module TreeUnfold (unfoldFullTree) where

import Papa
import Data.Tree.Lens
import Data.Tree

-- check if the root of tree matches to the incoming item
check :: Eq a => a -> Tree a -> Bool
check i = (== i) . rootLabel

-- create a traversal of plates as per the depth required by the unfold operation
platesPerDepth :: (Plated a, Applicative f) => Int -> ((a -> f a) -> a -> f a)
platesPerDepth i = foldl' (\g p -> plate . g) id [0..i]

fn :: Eq a => Int -> (Tree a, [a]) -> a -> (Tree a, [a])
fn i (ts,ys) x = if isInLevel 
                then 
                    (ts,ys) -- ^ if new node is already there then move on
                else 
                    (append ts,ys) -- ^ append the new node to the right parent
    where
        newNode = Node x [] -- ^ new node
        parent = ys ^?! ix (i - 1) -- ^ parent index, required in case of a new child
        isInLevel = isJust $ ts ^? platesPerDepth i . filtered (check x) -- ^ check if new node is already there in this level
        append = bool (over (platesPerDepth (i - 1) . filtered (check parent)) (branches <>~ [newNode])) -- ^ add the new node as a new child to a specific parent
                      (over branches (<> [newNode])) -- ^ if first node then just add it there
                      (i == 0) -- ^ check if it's first node


-- unfold a tree from list of lists where each list is for items that signifies
-- the path of an element (last one of the list) right from the top of the tree
-- 
-- the function takes an initial tree as the container tree within which rest of the final tree is constructed
-- 
unfoldFullTree :: Eq a => Tree a -> [[a]] -> Tree a
unfoldFullTree = foldl' (\t ys -> fst $  ifoldl' fn (t,ys) ys)
