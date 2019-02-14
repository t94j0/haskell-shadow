import System.IO  
import Data.Dates
import Data.List.Split

data Shadow = Shadow {
  users :: [ShadowEntry]
} deriving (Show)

parseShadowString :: String -> Shadow
parseShadowString x = Shadow  $ map parseShadowEntryString (words x)

parseShadowLocal :: Shadow
parseShadowLocal = do
  shadow <- readFile "/etc/shadow"
  parseShadowString shadow


data ShadowEntry = ShadowEntry {
  username :: String,
  hash :: String,
  dateCreated :: DateTime
} deriving (Show)

epochPlusDays :: Integer -> DateTime
epochPlusDays x = addInterval (DateTime 1970 1 1 0 0 0) (Days x)

stringToInt :: String -> Integer
stringToInt x = read x :: Integer

getValue :: String -> Int -> String
getValue x y = splitOn ":" x !! y

parseShadowEntryString :: String -> ShadowEntry
parseShadowEntryString x = ShadowEntry (getValue x 0) (getValue x 1) (epochPlusDays (stringToInt $ getValue x 2))


main = do
  print parseShadowLocal
