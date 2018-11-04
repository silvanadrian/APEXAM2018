module ParserImpl where

-- put your parser in this file. Do not change the types of the following
-- exported functions
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Defs

parseVersion :: String -> Either ErrMsg Version
parseVersion = undefined

parseDatabase :: String -> Either ErrMsg Database
parseDatabase db = case parse (do   res <- parsePackages
                                    eof
                                    return res)
                            "Parse Error"
                            db of
                   Left a -> Left (show a)
                   Right b -> Right (DB b)


parsePackages :: Parser [Pkg]
parsePackages = parsePackage


-- package {name foo}
-- data Pkg = Pkg {name :: PName,
--                ver :: Version,
--                desc :: String,
--                deps :: Constrs}
-- [Pkg {name = P name, ver = V [VN 1 ""], desc = "", deps = [(P "bar",(True,V [VN 1 ""],V [VN 0 ""]))]}]
parsePackage :: Parser [Pkg]
parsePackage = do
                _ <- string "package {"
                nameAttr <-  string "name"
                _ <- string " "
                name <- string "foo"
                _ <- string "}"
                return [Pkg {name = P name, ver = V [VN 1 ""], desc = "", deps = []}]