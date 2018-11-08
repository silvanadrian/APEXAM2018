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
                    deps <- many ( choice[try parseRequires,try parseConflicts])
                    _ <- string "}"
                    return Pkg {name = pname,ver = version , desc = description,
                    -- filter self referential Constraints
                    deps = filter (\(name, _) -> name /= pname) (concat(deps))}

-- Parse Package name
parseName :: Parser PName
parseName = do
               _ <- whitespace
               _ <-  caseString "name"
               _ <- whitespace
               _ <- optional (char '"')
               name <- many1 (letter <|> digit <|> char '-')
               _ <- optional (char '"')
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


escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

parseDescription :: Parser String
parseDescription = do
                        _ <- whitespace
                        _ <- caseString "description"
                        _ <- whitespace
                        char '"'
                        description <- many character
                        char '"'
                        _ <- optional (string ";")
                        return $ concat(description)

parseRequires :: Parser Constrs
parseRequires = do
                    _ <- whitespace
                    _ <- caseString "requires"
                    _ <- whitespace
                    pconsts <- many (choice[try(parsePConstrH(True)),
                                            try (parseSConstrL(True)),
                                            try(parseSConstrH(True))
                                            ])
                    _ <- optional (string ";")
                    return (concat(pconsts))

parseConflicts :: Parser Constrs
parseConflicts = do
                    _ <- whitespace
                    _ <- caseString "conflicts"
                    _ <- whitespace
                    pconsts <- many (choice[try(parsePConstrH(False)),
                                            try (parseSConstrL(False)),
                                            try(parseSConstrH(False))
                                            ])
                    _ <- (optional (string ";"))
                    return (concat(pconsts))

parseSConstrL :: Bool -> Parser Constrs
parseSConstrL req = do
                      name <- many1 letter
                      _ <- whitespace
                      version <-  parseVersionLow
                      return [((P name), (req, minV, version))]

parseSConstrH :: Bool -> Parser Constrs
parseSConstrH req = do
                      name <- many1 letter
                      _ <- whitespace
                      version <-  parseVersionHigh
                      return [((P name), (req, version, maxV))]

parsePConstrH :: Bool -> Parser Constrs
parsePConstrH req = do
                      name <- many1 letter
                      _ <- whitespace
                      lower <-  parseVersionLow
                      _ <- optional (whitespace)
                      _ <- string ","
                      _ <- optional (whitespace)
                      _ <- many1 letter
                      _ <- whitespace
                      max <- parseVersionHigh
                      case lower <= max of
                        True -> return [((P name), (req, lower, max))]
                        False -> fail "Error"


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


isPrintChar :: Char -> Bool
isPrintChar c
  | ord c >= 32 && ord c <= 126 = True
  | otherwise = False

-- skip whitespace
whitespace = skipMany space

caseChar :: Char -> Parser Char
caseChar c = char (toLower c) <|> char (toUpper c)

-- Match any case of the characters
caseString :: String -> Parser String
caseString s = try (mapM caseChar s) <?> "\"" ++ s ++ "\""
