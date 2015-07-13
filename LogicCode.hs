module LogicCode
( HuffmanNode,
  truthTable,
  grayCode,
  huffmanCode
) where

data HuffmanNode a = Empty | Leaf (a, Int) | Branch Int (HuffmanNode a) (HuffmanNode a)

--Problem 46 - 49: Problem 48: Truth tables for logical expressions.
--Eg: truthTable 3 (\[a, b, c] -> Logical expr involving a, b, and c)

truthTable :: Int -> ([Bool] -> Bool) -> IO ()
truthTable n fn = printTable $ map (\x -> x ++ [fn x]) (cart n [True, False]) where
  printTable :: [[Bool]] -> IO ()
  printTable = mapM_ (putStrLn . concatMap showSpace) where
    showSpace :: Bool -> String
    showSpace True = show True ++ "  "
    showSpace False = show False ++ " "
  cart :: Int -> [a] -> [[a]]
  cart 1 xs = foldr (\x acc -> [x] : acc) [] xs
  cart l xs = [ x ++ y | x <- cart 1 xs, y <- cart (l - 1) xs]

--Problem 49: Gray codes.
--An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules; for example,
--n = 1: C(1) = ['0','1'].
--n = 2: C(2) = ['00','01','11','10'].
--n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
grayCode :: Int -> [String]
grayCode 1 = ["0", "1"]
grayCode n = map ('0':) (grayCode $ n - 1) ++ (map ('1':) $ reverse $ grayCode (n - 1))

--Problem 50: Huffman Codes.
--Generate a Huffman code list from a given list of alphabets and their frequencies.
huffmanCode :: (Ord a) => [(a, Int)] -> [(a, String)]
huffmanCode ls = sort' $ process "" $ makeTree $ sort $ turnLeaf ls where
  sort :: [HuffmanNode a] -> [HuffmanNode a]
  sort [] = []
  sort (x : xs) =
    let smallerSorted = sort [a | a <- xs, snd' a <= snd' x]
        biggerSorted = sort [a | a <- xs, snd' a > snd' x]
    in  smallerSorted ++ [x] ++ biggerSorted
  turnLeaf :: [(a, Int)] -> [HuffmanNode a]
  turnLeaf = map Leaf
  makeTree :: [HuffmanNode a] -> HuffmanNode a
  makeTree [tr] = tr
  makeTree (m : n : ys) = makeTree $ sort $ Branch (snd' m + snd' n) m n : ys
  process :: String -> HuffmanNode a -> [(a, String)]
  process _ Empty = []
  process str (Leaf (e, _)) = [(e, str)]
  process str (Branch _ ltr rtr) = process (str ++ "0") ltr ++ process (str ++ "1") rtr
  snd' :: HuffmanNode a -> Int
  snd' (Leaf (_, n)) = n
  snd' (Branch n _ _) = n
  sort' :: (Ord a) => [(a, String)] -> [(a, String)]
  sort' [] = []
  sort' (x : xs) =
    let smallerSorted = sort' [a | a <- xs, fst a <= fst x]
        biggerSorted = sort' [a | a <- xs, fst x < fst a]
    in  smallerSorted ++ [x] ++ biggerSorted
