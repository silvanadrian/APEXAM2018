module ParserImpl where

-- put your parser in this file. Do not change the types of the following
-- exported functions
import           Data.Char
import           Defs
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


-- package {name foo}
-- data Pkg = Pkg {name :: PName,
--                ver :: Version,
--                desc :: String,
--                deps :: Constrs}
-- [Pkg {name = P name, ver = V [VN 1 ""], desc = "", deps = [(P "bar",(True,V [VN 1 ""],V [VN 0 ""]))]}]

parsePackage :: Parser Pkg
parsePackage = do
                    _ <- whitespace
                    _ <- caseString "package {"
                    pname <- parseName
                    version <- parseStringVersion
                    description <- parseDescription
                    deps <- many parseDeps
                    _ <- string "}"
                    return Pkg {name = pname,ver = version , desc = description, deps = (zip (repeat pname) deps)}

parseName :: Parser PName
parseName = do
               _ <- whitespace
               _ <-  caseString "name"
               _ <- string " "
               name <- many1 letter
               _ <- optional (string ";")
               return (P name)

parseStringVersion :: Parser Version
parseStringVersion = do
                        _ <- whitespace
                        _ <- caseString "version"
                        _ <- string " "
                        version <- many1 (digit <|> letter <|> char '.')
                        optional (string ";")
                        case parseVersion version of
                           Right a -> return a
                           _       -> fail "Version wasn't possible to parse"
                      <|> return (V [VN 1 ""])

parseDescription :: Parser String
parseDescription = do
                        _ <- whitespace
                        _ <- caseString "description"
                        _ <- string " "
                        description <- many1 letter
                        _ <- optional (string ";")
                        return description
                    <|> return ""

parseDeps :: Parser PConstr
parseDeps = do
                _ <- whitespace
                consts <- caseString "requires" <|> caseString "conflicts"
                return (False, (V [VN 1 ""]),(V [VN 1 ""]))

-- parseRequires :: Parser PConstr
-- parseRequires = do
--                     _ <- whitespace


-- skip whitespace
whitespace = skipMany space

caseChar :: Char -> Parser Char
caseChar c = char (toLower c) <|> char (toUpper c)

-- Match any case of the characters
caseString :: String -> Parser String
caseString s = try (mapM caseChar s) <?> "\"" ++ s ++ "\""
