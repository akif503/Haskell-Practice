data Binarytree a = 
    Leaf
  | Node (Binarytree a) a (Binarytree a)
  deriving Show

-- Inserting a value in the binarytree

insert :: Ord a => a
           -> Binarytree a
           -> Binarytree a
        
insert b Leaf = Node Leaf b Leaf
insert b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert b left) a right
    | b > a = Node left a (insert b right)

mapTree :: (a -> b) -> Binarytree a -> Binarytree b
mapTree _ Leaf = Leaf
maptree f (Node left a right) = 
    