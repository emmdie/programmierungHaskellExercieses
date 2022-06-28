f :: [Int] -> Int
f [] = 0
f xs = foldr (*) 1 (map (^2) (filter (even) xs)) 

foldleft :: (a -> b -> a) -> a -> [b] -> a
foldleft _ a [] = a
foldleft f a xs = foldleft f (f a (head xs)) (drop 1 xs)

data Tree a = Node a [Tree a]
treeC = Node "c" []
treeD = Node "d" []
treeF = Node "f" []
treeB = Node "b" [treeC, treeD]
treeE = Node "b" [treeF]
treeG = Node "g" []
treeA = Node "a" [treeB, treeE, treeG]

oddTree :: Tree a -> Bool
oddTree (Node _ []) = True
oddTree (Node _ xs) = odd (length xs) && oddTree' xs
   where 
         oddTree' [] = True
         oddTree' xs = oddTree(head xs) && oddTree' (tail xs)

preOrder :: Tree a -> [a]
preOrder (Node x kinderListe) = preorder' kinderListe [x]
 where
   preorder' :: [Tree a]-> [a] -> [a]
   preorder' [] endListe = endListe
   preorder' abarbeitenListe endListe = preorder' ((updateAbarbeitenListe abarbeitenListe) (head abarbeitenListe)) (endListe ++ [getAFromTree(head abarbeitenListe)])
   updateAbarbeitenListe :: [Tree a] -> Tree a -> [Tree a]
   updateAbarbeitenListe alt (Node _ kinderListe) = kinderListe ++ (drop 1 alt)
   getAFromTree :: Tree a -> a
   getAFromTree (Node x _) = x

