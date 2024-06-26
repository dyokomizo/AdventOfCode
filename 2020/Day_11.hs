module Day11 where
  import qualified Data.Matrix as M
  import Data.Matrix ((!))

  import qualified Data.List as L
  import Control.Arrow

  adjacent (i,j) ss = concat [[(i-1,j-1)| i > 1 && j > 1]
                             ,[(i-1,j  )| i > 1         ]
                             ,[(i-1,j+1)| i > 1 && j < m]
                             ,[(i  ,j-1)|          j > 1]
                             ,[(i  ,j+1)|          j < m]
                             ,[(i+1,j-1)| i < n && j > 1]
                             ,[(i+1,j  )| i < n         ]
                             ,[(i+1,j+1)| i < n && j < m]
                             ]
    where n = M.nrows ss
          m = M.ncols ss

  seatP1 ss = M.mapPos rule ss
    where rule p 'L' |                      all (/='#') $ around p = '#'
          rule p '#' | (4 <=) $ length $ filter (=='#') $ around p = 'L'
          rule _ s                                                 = s
          around p = map (ss!) $ adjacent p ss

  seating seat 0 ss             = Left ss
  seating seat n ss | ss == ts  = Right (n,ts)
                    | otherwise = seating seat (n-1) ts
    where ts = seat ss

  ok (Right (_,ss)) = ss

  occupied = length . filter (=='#') . M.toList

  lineOfSight (i,j) ss = [no,ne,ea,se,so,sw,we,nw]
    where no = [(i',j') | (i',j') <- zip (preds (i-1) 1)   (repeat j)]
          ne = [(i',j') | (i',j') <- zip (preds (i-1) 1)   (succs (j+1) m)]
          ea = [(i',j') | (i',j') <- zip (repeat i)        (succs (j+1) m)]
          se = [(i',j') | (i',j') <- zip (succs (i+1) n)   (succs (j+1) m)]
          so = [(i',j') | (i',j') <- zip (succs (i+1) n)   (repeat j)]
          sw = [(i',j') | (i',j') <- zip (succs (i+1) n)   (preds (j-1) 1)]
          we = [(i',j') | (i',j') <- zip (repeat i)        (preds (j-1) 1)]
          nw = [(i',j') | (i',j') <- zip (preds (i-1) 1)   (preds (j-1) 1)]
          n = M.nrows ss
          m = M.ncols ss
  los = lineOfSight

  succs a b | a > b     = []
            | a < b     = enumFromThenTo a (succ a) b
            | otherwise = [a]
  preds a b | a < b     = []
            | a > b     = enumFromThenTo a (pred a) b
            | otherwise = [a]
  
  pointOfView p ss = map (map (ss!)) $ lineOfSight p ss
  pov = pointOfView

  occupiedSeats p ss = length $ filter isOccupied $ map (filter (/='.')) $ pointOfView p ss
    where isOccupied []      = False
          isOccupied ('L':_) = False
          isOccupied ('#':_) = True

  seatP2 ss = M.mapPos rule ss
    where rule p 'L' | occupiedSeats p ss == 0 = '#'
          rule p '#' | occupiedSeats p ss >= 5 = 'L'
          rule _ s                             = s

  xs = M.fromLists
         ["LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLL.LLLLLLLLLLLL",
          "LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLL.LLLLLLL.LLLLLLLLLLLL.LL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLL",
          "LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLL..LLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLL.L.LLLLLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLL",
          "..LLL..L.L..LL...LL......L..LL..L..LL........L.....LLLL.L.L.L.........L.L..L.L.....L..L............",
          "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLLLLL.LLL.LLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLL",
          "LLLLLLL.LLLLL.LLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL",
          "....L..LL...L.L.L.......LL....L.....L..L...L.L.L....LL.L..L...LL...L....L..L...L.............LL..LL",
          "LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLLLLL.L.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL.LLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLL.L.LLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLLL.LLLL..LLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLL.L.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLL",
          "..L...L.L....L........L.......L.L...L..L.LL.........L.......LLL.LL.L...LL....L..L.L.L.LL.....L.....",
          "LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLL",
          "LLLLLLL..LLLLLLLL.LLLLLL.LLLLLLL.LLLL.L.LLLLLLL.LLLLL.LLLLLLL.LLLL.LL.L.LLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLL.L.L.LLLLL.LLLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL.LLLLLLL.LLLLLL.LLLLL..LLLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "L......L..L...L....L...L...L...L...L..LL...L......LL......LL......LLLL..L.....LL...L.LL..L....LL..L",
          "LLLLLLL.LLLLLLLLL.LLLLLL..LLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLL.LLLL.LLLLLLLLL.LLL.LLLLLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLL.LLLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLL.LLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLL.LLLL.LLLLL.LLL.LLLLL.LLLLLLL.LL.L.LLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.L.LLL.LLLLLLL.LLLL.LLLL..LLLLLLL.LLLLLLLLLLLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.L.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "L....L...L....L...L.....L....L.LL..L..L.LL.L..L..LL...LL..L......L...L.LL...L.L.....L.L.LLL.LL.L.L.",
          "LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLL..LLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLL..LLLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLL..LLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "........L........L......L...L...L.L.....L..L..LL.L..L.L.LLL.L..L..L.L.LL.L.LL......L...L..LLL....L.",
          "LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLL.LL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLL.L.LL.LLLLLLLLLLLLLLLLLLLLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLL..LLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL..LLLLLLLLLLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLLL.L..LLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
          "LLLLL.L.LLLL.LLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLL",
          "..LL..L......L.L.L...LL.L.LLL.L.....LLL..LL...L...L.L.LL....L.....L.LL..LLL...LL...L.....LLL.L..LL.",
          "LLLLLLL.LLLLLLL.L.LLLLLL.LLLLLLL..LLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLL.LL.L.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLL..LLLLLLL.LLLLLLLLLLLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLL..LLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "..L..L.L....L.L..L.L.LL......LLL.L...L..L..L..........LL.......L...LL.L.L.L...L.......L........L...",
          "LLLLLLLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLL.L.LLLLLLLLL.LLLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLL.LLL.LLL.LLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLL.LLL.LLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLL.LLL.",
          "LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLL.L.LLLLLLLLL.LLLLL..LLLLLLL.LLLL.LLLL.LLLL.LLLLL.LLLLLLL.LLLL.LLLL.LL.LLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "..L.......L..LL.L..L..LLLLL..L...L.L.L....L...L.LL.L.......L.....LL.........L.L......L.L..L.....L..",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL.LLLL.LLL.LLLLLLLLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL.LLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLL",
          "LLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "L...LL.L..LLLL.LLL.L....L......L.............LL.....L...LL.L...L.L......LL....L......L...LL.L.....L",
          "LLLLLLL.LLLLLLLLL.LLLLLL.L.LLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLL.L.LL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLL.LL.LLLLLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLL",
          "LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLL.LLLLLLLLL.LLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLL",
          "LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLL.LLLL.LLLLL.LLLLLLL.LLLL.L.LLLLLLLLLLL.LLLLLL.L.LLLLLLLLL"
         ]

  ys = M.fromLists
         ["L.LL.LL.LL",
          "LLLLLLL.LL",
          "L.L.L..L..",
          "LLLL.LL.LL",
          "L.LL.LL.LL",
          "L.LLLLL.LL",
          "..L.L.....",
          "LLLLLLLLLL",
          "L.LLLLLL.L",
          "L.LLLLL.LL"
         ]
  zs = M.fromLists
         [".......#.",
          "...#.....",
          ".#.......",
          ".........",
          "..#L....#",
          "....#....",
          ".........",
          "#........",
          "...#....."
         ]

  vs = M.fromLists
         [".............",
          ".L.L.#.#.#.#.",
          "............."
         ]

  ws = M.fromLists
         [".##.##.",
          "#.#.#.#",
          "##...##",
          "...L...",
          "##...##",
          "#.#.#.#",
          ".##.##."
         ]
