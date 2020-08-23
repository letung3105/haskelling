module BinaryTree where


data BinaryTree a
    = Nil
    | Leaf a
    | Node a (BinaryTree a) (BinaryTree a)
    deriving (Show)


create :: BinaryTree a
create = Nil


insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Nil = Leaf x
insert x' (Leaf x) | x' <= x = Node x (Leaf x') Nil
                   | x' > x  = Node x Nil (Leaf x')
insert x' (Node x l r) | x' <= x = Node x (insert x' l) r
                       | x' > x  = Node x l (insert x' r)


search :: Ord a => a -> BinaryTree a -> Bool
search x Nil = False
search x' (Leaf x) | x' == x   = True
                   | otherwise = False
search x' (Node x l r) | x' == x = True
                       | x' < x  = search x' l
                       | x' > x  = search x' r

