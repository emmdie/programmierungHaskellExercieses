isPrefix :: String -> String -> Bool
isPrefix "" x = True
isPrefix x "" = False
isPrefix prefix kette = (head prefix == head kette ) && isPrefix (drop 1 prefix) (drop 1 kette)

return1whenTrue :: Bool -> Int
return1whenTrue x
  | x == True = 1
  | x /= True = 0

countPattern :: String -> String -> Int
countPattern "" "" = 1
countPattern pattern string = return1whenTrue(isPrefix pattern string) + countPattern (pattern) (drop 1 string)

data BinTree = Branch Int BinTree BinTree | Nil deriving Show
unterbaum5 = Branch 5 Nil Nil
unterbaum1 = (Branch 1 Nil Nil)
unterbaum3 = Branch 3 unterbaum1 unterbaum5
mytree = (Branch 0 Nil unterbaum3)

treeIdent :: BinTree -> BinTree -> Bool
treeIdent Nil Nil = True
treeIdent (Branch x bt1 bt2) (Branch y bt3 bt4)
 | x==y && treeIdent bt1 bt3 && treeIdent bt2 bt4 = True
 | otherwise = False

insert :: BinTree -> [Int] -> BinTree
insert tree [] = tree
insert Nil list = insert (Branch (head list) Nil Nil) (drop 1 list)
insert (Branch knotenWert links rechts) list
 | knotenWert <= head list = insert (Branch knotenWert links (insert rechts [head list])) (drop 1 list)
 | otherwise = insert (Branch knotenWert (insert links [head list]) rechts) (drop 1 list)

breitensuche :: BinTree -> [BinTree] -> [Int]->[Int]
breitensuche Nil abarbeitenListe zahlenListe
  | null abarbeitenListe = zahlenListe 
  | otherwise= breitensuche (head abarbeitenListe) (drop 1 abarbeitenListe) zahlenListe
breitensuche baum abarbeitenListe zahlenListe
  | null abarbeitenListe= breitensuche links [rechts] zahlenListe++[zahlenwert]
  | otherwise = breitensuche (head abarbeitenListe) ((drop 1 abarbeitenListe)++[links, rechts])  zahlenListe ++ [zahlenwert]
  where (Branch zahlenwert links rechts) = baum

 -- da wir rekursiv "aus der Tiefe der Traversierung nach oben kommen" müssen wir das ergebnis noch umdrehen 
unwind :: BinTree -> [Int]
unwind tree = reverse (breitensuche tree [] [])
