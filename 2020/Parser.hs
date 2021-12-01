module Parser where
  import qualified Data.List as L
  import qualified Text.Read as R
  import Control.Monad (guard, replicateM)
  import Control.Applicative (Alternative(..))

  newtype Parser a = P (String -> [(a, String)])
  parse (P p) = p

  instance Functor Parser where
    fmap f (P p) = P $ \s -> map (fst f) $ p s
      where fst f (a, b) = (f a, b)

  instance Applicative Parser where
    pure x  = P $ \s -> [(x,s)]
    p <*> q = p >>= (\f -> q >>= (pure . f))

  instance Alternative Parser where
    empty = P $ \s -> []
    (P p) <|> (P q) = P $ \s -> concat [p s, q s]

  instance Monad Parser where
    (P p) >>= f = P $ \s -> concatMap (\(a, s') -> parse (f a) s') (p s)

  check p x = do guard $ p x
                 return x

  end = P $ \s -> if null s then [((),"")] else []

  suffix p = \x -> p >> pure x

  str p = P $ \s -> if L.isPrefixOf p s then [(p, drop (length p) s)] else []
  chr c = one >>= check (==c)

  one = P $ \s -> if null s then [] else [(head s, tail s)]

  digit :: Parser Char
  digit = one >>= check (`elem` "0123456789")

  hexcode :: Parser Char
  hexcode = one >>= check (`elem` "0123456789abcdef")

  four = exactly 4

  exactly :: Int -> Parser a -> Parser [a]
  exactly = replicateM

  int :: String -> Parser Int
  int s = case R.readMaybe s of
            Nothing -> empty 
            Just n -> pure n
