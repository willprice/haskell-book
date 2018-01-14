import Data.Time
import Data.List

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate $ UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)
  , DbNumber 9001
  , DbString "Hello, World!"
  , DbDate $ UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = map toUTCTime $ filter isDate db
  where isDate (DbDate _) = True
        isDate _ = False
        toUTCTime (DbDate date) = date

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = map toNumber $ filter isNumber db
  where isNumber (DbNumber _) = True
        isNumber _ = False
        toNumber (DbNumber n) = n

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = maximum $ filterDbDate db

sumDb :: [DatabaseItem] -> Integer
sumDb db = sum $ filterDbNumber db

avgDb :: [DatabaseItem] -> Double
avgDb db = mean $ filterDbNumber db
  where mean :: [Integer] -> Double
        mean ls = let len = length ls in
          (fromIntegral $ sum ls) / (fromIntegral len)
