module Chapter12 where

import           Data.Char
import           Data.List
-- id :: a -> a
-- What is the kind of `a`? `*`

-- r :: a -> f a
-- What is the kind of `a` and `f`? `a` has kind `*` and `f` is `* -> *`

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

replaceThe :: String -> String
replaceThe sentence = unwords $ go ws
    where
        ws = words sentence
        go [] = []
        go (w:ws) = case notThe w of
                    Nothing -> go ws
                    Just _  -> w : go ws

startsWithVowel  :: String -> Bool
startsWithVowel ""    = False
startsWithVowel (c:_) = c `elem` "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel sentence = go ws
    where
        ws = words sentence
        go [] = 0
        go [_] = 0
        go (w1:w2:ws) = if (toLower <$> w1) == "the" && startsWithVowel w2 then 1 else 0 + go (w2:ws)


newtype Word' =
  Word' String
  deriving (Eq, Show)


vowels = "aeiou"

isVowel c = c `elem` vowels
mkWord :: String -> Maybe Word'
mkWord s = if vowelCount > nounCount then Nothing else Just $ Word' s
  where
    vowelCount = length $ filter isVowel s
    nounCount = length s - vowelCount

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)


natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger n = go 0 n
  where go acc n = case n of
          Zero -> acc
          Succ n' -> go (acc + 1) n'


integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | n == 0 = Just Zero
  | otherwise = Just $ go Zero n
  where
    go acc n = case n of
      0 -> acc
      otherwise -> Succ $ go acc (n - 1)

isJust :: Maybe a -> Bool
isJust m = case m of
  Nothing -> False
  otherwise -> True

isJust' :: Maybe a -> Bool
isJust' Nothing = False
isJust' (Just _) = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee d f Nothing = d
mayybee d f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe d x = mayybee d id x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just x:xs) = x : catMaybes xs
catMaybes (Nothing:xs) = catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if allJust then Just $ map fromJust xs else Nothing
  where
    allJust = foldl' (&&) True $ map isJust xs
    fromJust (Just x) = x

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight = not . isLeft

lefts' :: [Either a b] -> [a]
lefts' = foldr (\x acc -> case x of
                   Right _ -> acc
                   Left x -> x : acc)
         []

rights' :: [Either a b] -> [b]
rights' = foldr (\x acc -> case x of
                       Right x  -> x : acc
                       Left _ -> acc)
             []
partitionedEithers' :: [Either a b] -> ([a], [b])
partitionedEithers' = foldr (\x (as, bs) -> case x of
                               Left a -> (a:as, bs)
                               Right b  -> (as, b:bs))
                     ([], [])


eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just $ f x
eitherMaybe' _ (Left _) = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa _ (Left a) = fa a
either' _ fb (Right b) = fb b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (\_ -> Nothing) (\x -> Just $ f x) x

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f b
  where b = f a

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case x of
  (Just (a, b')) -> a : myUnfoldr f b'
  otherwise -> []
  where x = f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f a = myUnfoldr (\x -> Just $ (x, f x)) a


data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case x of
                  Nothing -> Leaf
                  Just (a', b, a'') -> Node (unfold f a') b (unfold f a'')
  where x = f a


treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\n' -> if n' > 0 then
                         Just (n' - 1, n - n', n' - 1)
                       else
                         Nothing)
              n
