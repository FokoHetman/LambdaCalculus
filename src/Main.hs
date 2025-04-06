module Main where
import Data.Char

data Node = Function [Char] Node
  | Application [Node] [Node]
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


function :: Parser Node
function = undefined


identifier :: Parser Node
--identifier = (\x ->  Identifier $ head x) <$> stringP "\0"
identifier = Parser $ f 
  where
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
