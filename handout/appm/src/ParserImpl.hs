module ParserImpl where

-- put your parser in this file. Do not change the types of the following
-- exported functions
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Defs

parseVersion :: String -> Either ErrMsg Version
parseVersion str = do
                --string <- whitespace str
                --number <- read <$> many1 digit
                --string <- optional (many1 letter)
                return $  V [VN  0 ""]

-- skip whitespace
whitespace = skipMany space

parseDatabase :: String -> Either ErrMsg Database
parseDatabase db = case parse (do   res <- (many parsePackage)
                                    eof
                                    return res)
                            "Parse Error"
                            db of
                   Left a -> Left (show a)
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
                    _ <- string "package {"
                    pname <- parseName
                    description <- parseDescription
                    version <- parseStringversion
                    parserTrace "label"
                    _ <- string "}"
                    return Pkg {name = pname,ver = V [VN 0 ""] , desc = description, deps = []}

parseName :: Parser PName
parseName = do
               _ <- whitespace
               _ <-  string "name"
               _ <- string " "
               name <- many1 letter
               _ <- optional (string ";")
               return (P name)

parseStringversion :: Parser String
parseStringversion = do
                        _ <- whitespace
                        _ <- string "version"
                        _ <- string " "
                        version <- many1 letter
                        optional (string ";")
                        return version
                      <|> return ""


parseDescription :: Parser String
parseDescription = do
                    _ <- whitespace
                    _ <- string "description"
                    _ <- string " "
                    description <- many1 letter
                    _ <- optional (string ";")
                    return description
                    <|> return ""
