module Main where
import Data.Char
import Control.Applicative

data Node = Function [Char] Node
  | Application Node [Node] -- apply [Node]s to Node
  | Identifier Char
  deriving (Show,Eq)


newtype Parser a = Parser {runParser :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap f (Parser p) = 
    Parser $ \input -> do
      (input', x) <- p input
      Just(input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = 
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> 
    p1 input <|> p2 input


ws :: Parser String
ws = spanP isSpace

parseExpr :: Parser Node
parseExpr = function <|> application <|> identifier

application :: Parser Node
application = f <$> 
    (charP '(' *> ws *> many parseExpr <* ws <* charP ')')
  where
    f [x] = x
    f (x:xs) = Application x xs



function :: Parser Node
function = Function <$> (charP 'λ' *> identifiers <* charP '.') <*> parseExpr
    

identifiers :: Parser String
identifiers = spanP (/='.')



notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs 
    then Nothing
    else Just (input', xs)

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (token, rest) = span f input
  in Just (rest, token)

identifier :: Parser Node
--identifier = (\x ->  Identifier $ head x) <$> stringP "\0"
identifier = Parser f 
  where
    f (')':ys) = Nothing
    f ('(':ys) = Nothing
    f (' ':ys) = runParser parseExpr ys
    f (y:ys) = Just (ys, Identifier y)
    f [] = Nothing



--52:00
charP :: Char -> Parser Char
charP x = Parser $ f 
  where
    f (y:ys)
      | y==x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP




main :: IO ()
main = putStrLn "<init>"
