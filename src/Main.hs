{-# LANGUAGE TemplateHaskell #-}

module Main where
import Data.Char
import Data.List
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

--definition :: Parser Node
--definition = Definition <$> ()

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


unwrap :: Maybe a -> a
unwrap (Just a) = a


display :: Node -> String
display (Function args body) = "λ" ++ args ++ ".(" ++ display body ++ ")"
display (Application a bs) = "(" ++ display a ++ " " ++ (intercalate " " $ fmap display bs) ++ ")"
display (Identifier id) = [id]

displayIO :: Node -> IO()
displayIO x = do putStrLn $ display x


substitute :: Node -> Node -> Node -> Node
substitute replaced with (Application a bs) = if Application a bs == replaced then with else
      Application (substitute replaced with a) (fmap (substitute replaced with) bs)

substitute replaced with (Function args body) = if Function args body == replaced then with else
      Function args $ substitute replaced with body

substitute replaced with x = if x==replaced then with else x



betaReduce :: Node -> Node
betaReduce (Function args body) = Function args (betaReduce body)
betaReduce (Application (Function args body) bs) = if length bs == 1 then
      f
    else
      Application f $ tail bs
    where
      g = substitute (Identifier $ head args) (head bs) body
      f = if length args > 1 then Function (tail args) g
          else g
betaReduce x = x


reducible :: Node -> Bool
reducible x = x /= betaReduce x


solve :: Node -> Node
solve x =
  if reducible x then
    solve $ betaReduce x
  else
    x


replace :: Char -> Char -> String -> String
replace a b (x:xs) = (if x==a then b else x):replace a b xs

run :: String -> Node
run x = solve $ (snd . unwrap) $ runParser parseExpr $ replace '\\' 'λ' x

main :: IO ()
main = do
  x <- getLine
  displayIO $ run x
