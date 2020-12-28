{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day4 where

import Control.Monad
import Data.List
import Data.List.Split (splitOn)
import Text.RawString.QQ
import Text.Trifecta

toy :: String
toy = [r|ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in|]


reqFields :: [String]
reqFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

charVals :: Parser Char
charVals = choice [lower, digit, char '#']

pkv :: Parser (String, String)
pkv = do
  key <- some lower
  _   <- char ':'
  val <- some charVals
  return (key, val)

pkvs :: Parser [(String, String)]
pkvs = sepBy1 pkv (char ' ')

pbyr :: Parser String
pbyr = do
  yrStr <- replicateM 4 digit <* eof
  let year = read yrStr
  case year >= 1920 && year <= 2002 of
    True -> return yrStr
    _    -> fail "Invalid birth year"

piyr :: Parser String
piyr = do
  yrStr <- replicateM 4 digit <* eof
  let year = read yrStr
  case year >= 2010 && year <= 2020 of
    True -> return yrStr
    _    -> fail "Invalid issue year"

peyr :: Parser String
peyr = do
  yrStr <- replicateM 4 digit <* eof
  let year = read yrStr
  case year >= 2020 && year <= 2030 of
    True -> return yrStr
    _    -> fail "Invalid expiry"

ecl :: [String]
ecl = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

pecl :: Parser String
-- (string "amb" <* eof)
pecl = choice $ (<* eof) . string <$> ecl

ppid :: Parser String
ppid = replicateM 9 digit <* eof

phcl :: Parser String
phcl = do
  hex    <- char '#'
  digits <- replicateM 6 (oneOf "0123456789abcdef")
  eof
  return $ hex : digits

phgt :: Parser String
phgt = do
  height <- integer
  units <- choice [string "cm", string "in"]
  eof
  case units of
    "cm" -> case height >= 150 && height <= 193 of
      True -> return $ show height
      _    -> fail "Height in cm must be between 150 and 193"
    _ -> case height >= 59 && height <= 76 of
      True -> return $ show height
      _    -> fail "Height in inches must be between 59 and 76"

arePresent :: [(String, String)] -> Bool
arePresent kvs =
  let keys = map fst kvs
      diff = reqFields \\ keys
  in (null diff || diff == ["cid"])

parserLookup :: String -> Parser String
parserLookup "byr" = pbyr
parserLookup "iyr" = piyr
parserLookup "eyr" = peyr
parserLookup "hgt" = phgt
parserLookup "hcl" = phcl
parserLookup "ecl" = pecl
parserLookup "pid" = ppid
parserLookup _     = many anyChar

getResults :: [(String, String)] -> [Result String]
getResults = map (\(k, v) ->
              let psr = parserLookup k
              in parseString psr mempty v)

allSuccesses :: [Result String] -> Bool
allSuccesses = all f
  where f (Success _) = True
        f (Failure _) = False

validate :: [(String, String)] -> Bool
validate = allSuccesses . getResults


day4 :: IO ()
day4 = do
  contents <- readFile "input.txt"
  let passports :: [String]
      passports = unwords . lines <$> splitOn "\n\n" contents

  let parseOnePP :: String -> Result [(String, String)]
      parseOnePP = parseString pkvs mempty

  let unwrap :: String -> [(String, String)]
      unwrap pp = foldResult (const []) id (parseOnePP pp)

  -- let numPresent = foldr rf 0 passports
  --       where
  --         rf ppStr acc = fromEnum (arePresent (unwrap ppStr)) + acc
  -- print numPresent

  let present = filter arePresent $ map unwrap passports
  print $ length present

  let numValid = foldr rf 0 present
        where rf tupList acc = fromEnum (validate tupList) + acc
  print numValid


-- for tests
pp1 :: String
pp1 = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"

pp2 :: String
pp2 = "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929"

pp3 :: String
pp3 = "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm"

pp4 :: String
pp4 = "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"

testkv :: [(String, String)]
testkv = [("ecl","gry"),("pid","860033327"),("eyr","2020"),("hcl","#fffffd"),("byr","1937"),("iyr","2017"),("cid","147"),("hgt","200cm")]

invalid :: String
invalid = [r|eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007|]

valid :: String
valid = [r|pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719|]
