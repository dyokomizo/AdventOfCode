module Day_02 where

data Command = Forward Integer | Down Integer | Up Integer
  deriving (Eq, Ord, Show, Read)
data Position = Position Integer Integer
  deriving (Eq, Ord, Show, Read)

move :: Position -> Command -> Position
move (Position x y) (Forward n) = Position (x + n)  y
move (Position x y) (Down    n) = Position  x      (y + n)
move (Position x y) (Up      n) = Position  x      (y - n)

moves :: [Command] -> Position
moves = foldl move (Position 0 0)

solve (Position x y) = x * y

data Location = Location Integer Integer Integer
  deriving (Eq, Ord, Show, Read)

pilot :: Location -> Command -> Location
pilot (Location x y a) (Forward n) = Location (x + n) (y + a * n)  a
pilot (Location x y a) (Down    n) = Location  x       y          (a + n)
pilot (Location x y a) (Up      n) = Location  x       y          (a - n)

solution :: [Command] -> Integer
solution = multiply . foldl pilot (Location 0 0 0)
  where multiply (Location x y _) = x * y


sample :: [Command]
sample = map read $ ["Forward 5","Down 5","Forward 8","Up 3","Down 8","Forward 2"]

puzzle :: IO [Command]
puzzle = map read <$> lines <$> readFile "Day_02_input.txt"
