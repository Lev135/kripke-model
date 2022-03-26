module Main where

import Prelude hiding (getLine, putStr, putStrLn)

import ModalLogic
import Kripke

import Data.Text (Text)
import Data.Text.IO
import Text.Megaparsec (parse, errorBundlePretty, MonadParsec (eof))
import Text.Megaparsec.Char (string, space1)
import Control.Applicative (Alternative((<|>), many))
import Control.Monad.Extra (untilJustM)
import Text.Megaparsec.Char.Lexer (decimal)

import qualified Models.GridJModel as GJM 

parsePart :: Parser a -> Text -> Either String a
parsePart p s = case parse p "" s of
    Left  e -> Left $ errorBundlePretty e
    Right a -> return a

parseAll :: Parser a -> Text -> Either String a
parseAll p = parsePart (p <* eof)

ioParseAll :: Parser a -> Text -> IO a
ioParseAll p t = case parseAll p t of
    Left  s -> fail s
    Right a -> return a

main = do
    untilJustM $ do
        putStrLn "Input formula or ':q' to exit: "
        fml <- getLine
        case fml of
            ":q" -> return $ Just ()
            _    -> do
                case parseAll GJM.pForm fml of
                    Left  e  -> print e
                    Right mf -> do
                        putStrLn $ GJM.selectedWorldsPretty layerSizes $ evalAll model mf
                return Nothing
    where   
        model = GJM.model layerSizes
        layerSizes = [5, 4, 2, 3]
