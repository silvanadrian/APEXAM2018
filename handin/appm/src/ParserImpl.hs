module ParserImpl where

-- put your parser in this file. Do not change the types of the following
-- exported functions
import           Data.Char
import           Defs
import           Utils
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import           Text.Parsec.String

parseVersion :: String -> Either ErrMsg Version
parseVersion str = case parse (do   res <- (many parseVersionN)
                                    return res)
                                              "Parse Error"
                                              str of
                                     Left a  -> Left (show a)
                                     Right b -> Right ((V b))

parseVersionN :: Parser VNum
parseVersionN = do
                  number <- read <$> (many1 (satisfy isDigit))
                  string <- option "" (many letter)
                  _ <- optional (char '.')
                  return (VN number string)


parseDatabase :: String -> Either ErrMsg Database
parseDatabase db = case parse (do   res <- (many parsePackage)
                                    eof
                                    return res)
                            "Parse Error"
                            db of
                   Left a  -> Left (show a)
                   Right b -> Right (DB b)

-- Parse Packages
parsePackage :: Parser Pkg
parsePackage = do
                    _ <- whitespace
                    _ <- caseString "package"
                    _ <- whitespace
                    _ <- string "{"
                    pname <- parseName
                    version <- try parseStringVersion <|> return (V [VN 1 ""])
                    description <- try parseDescription <|> return ""
                    deps <- many ( choice[try parseRequires, try parseConflicts])
                    _ <- string "}"
                    return Pkg {name = pname,ver = version , desc = description,
                    -- filter self referential Constraints
                    deps = filter (\(name, _) -> name /= pname) (cleanConst(concat(deps)))}

-- Parse Package name
parseName :: Parser PName
parseName = do
               _ <- whitespace
               _ <-  caseString "name"
               _ <- whitespace
               name <- many1 (letter <|> digit <|> char '-')
               _ <- optional (string ";")
               return (P name)

parseStringVersion :: Parser Version
parseStringVersion = do
                        _ <- whitespace
                        _ <- caseString "version"
                        _ <- whitespace
                        version <- many1 (digit <|> letter <|> char '.')
                        optional (string ";")
                        case parseVersion version of
                           Right a -> return a
                           _       -> fail "Version wasn't possible to parse"

parseDescription :: Parser String
parseDescription = do
                        _ <- whitespace
                        _ <- caseString "description"
                        _ <- whitespace
                        _ <- char '"'
                        description <- many1 letter
                        _ <- char '"'
                        _ <- optional (string ";")
                        return description

parseRequires :: Parser Constrs
parseRequires = do
                    _ <- whitespace
                    _ <- caseString "requires"
                    _ <- whitespace
                    pconsts <- many (parsePConstr(True))
                    _ <- optional (string ";")
                    return (concat(pconsts))

parseConflicts :: Parser Constrs
parseConflicts = do
                    _ <- whitespace
                    _ <- caseString "conflicts"
                    _ <- whitespace
                    pconsts <- many (parsePConstr(False))
                    _ <- (optional (string ";"))
                    return (concat(pconsts))


parsePConstr :: Bool -> Parser Constrs
parsePConstr req = do
                      _ <- whitespace
                      _ <- optional (string ",")
                      _ <- optional whitespace
                      name <- many1 letter
                      _ <- optional (whitespace)
                      lower <- option minV (parseVersionLow)
                      max <- option maxV (parseVersionHigh)
                      return [((P name), (req, lower, max))]


parseVersionLow :: Parser Version
parseVersionLow = do
                            _ <- string "<"
                            _ <- whitespace
                            version <- many1 (digit <|> letter <|> char '.')
                            case parseVersion version of
                                Right a -> return a
                                _ -> fail "Version wasn't possible to parse"

parseVersionHigh :: Parser Version
parseVersionHigh = do
                    _ <- string ">="
                    _ <- whitespace
                    version <- many1 (digit <|> letter <|> char '.')
                    case parseVersion version of
                            Right a -> return a
                            _ -> fail "Version wasn't possible to parse"

-- Merges parsed constraints to remove duplicates etc.
cleanConst :: Constrs -> Constrs
cleanConst [] = []
cleanConst (x:xs) = case merge xs [x] of
                        Nothing -> []
                        Just a -> a

-- skip whitespace
whitespace = skipMany space

caseChar :: Char -> Parser Char
caseChar c = char (toLower c) <|> char (toUpper c)

-- Match any case of the characters
caseString :: String -> Parser String
caseString s = try (mapM caseChar s) <?> "\"" ++ s ++ "\""
