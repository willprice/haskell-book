import System.IO

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age 
    | name /= "" && age > 0 = 
        Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = 
        Left $ PersonInvalidUnknown $
            "Name was " ++ show name ++
            " Age was: " ++ show age

printPersonError :: PersonInvalid -> IO ()
printPersonError err = putStrLn $ show err

printPerson :: Person -> IO ()
printPerson person = putStrLn $ "Yay! Successfully got a person: " ++ show person

gimmePerson :: IO ()
gimmePerson = do
    hSetBuffering stdout NoBuffering
    putStrLn "Enter the following details"
    putStr "Person: "

    name <- getLine
    putStr "Age: "
    age <- (read :: String -> Integer) <$> getLine
    let person = mkPerson name age
    either printPersonError printPerson person

main :: IO ()
main = gimmePerson
