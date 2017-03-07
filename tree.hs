data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x (EmptyTree) (EmptyTree)

tree :: (Ord a) => a -> Tree a -> Tree a
tree x EmptyTree = singleton x
tree x (Node a left right) | x == a = Node x left right
                           | x < a = Node a (tree x left) right
                           | x > a = Node a left (tree x right)

isTree :: (Ord a) => a -> Tree a -> Bool
isTree x EmptyTree = False
isTree x (Node a left right) | x == a = True
                             | x < a = isTree x left
                             | x > a = isTree x right

ler = do
        x <- readFile "teste.bat"
        putStrLn x           

esc = do
        x <- getLine
        writeFile "teste.bat" x


