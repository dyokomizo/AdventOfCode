module Day10 where
  import qualified Data.List as L
  import Control.Arrow

  diffs js = map (head &&& length) $ L.group $ L.sort $ (3 : zipWith (-) js (0:js))

  arrangements i (j:js@(k:_)) | i + 3 < k = map (j:) jjs
                              | otherwise = map (j:) jjs ++ ijs
    where ijs = arrangements i js
          jjs = arrangements j js
  arrangements _ (j:[]) = [[j]]
  arrangements _  []    = []

  monotonic = go []
    where go []       (x:xs)              = go [x] xs
          go ys@(y:_) (x:xs) | x == y + 1 = go (x:ys) xs
                             | otherwise  = reverse ys : go [x] xs
          go []        []                 = []
          go ys        []                 = [reverse ys]

  routes i = go 1 i . monotonic
    where go n _ []       = n
          go n i (is:iss) = go (n * length (arrangements i is)) (last is) iss

  xs = L.sort
       [99,
        3,
        1,
        11,
        48,
        113,
        131,
        43,
        82,
        19,
        4,
        153,
        105,
        52,
        56,
        109,
        27,
        119,
        147,
        31,
        34,
        13,
        129,
        17,
        61,
        10,
        29,
        24,
        12,
        104,
        152,
        103,
        80,
        116,
        79,
        73,
        21,
        133,
        44,
        18,
        74,
        112,
        136,
        30,
        146,
        100,
        39,
        130,
        91,
        124,
        70,
        115,
        81,
        28,
        151,
        2,
        122,
        87,
        143,
        62,
        7,
        126,
        95,
        75,
        20,
        123,
        63,
        125,
        53,
        45,
        141,
        14,
        67,
        69,
        60,
        114,
        57,
        142,
        150,
        42,
        78,
        132,
        66,
        88,
        140,
        139,
        106,
        38,
        85,
        37,
        51,
        94,
        98,
        86,
        68
       ]

  ys = L.sort
       [16,
        10,
        15,
        5,
        1,
        11,
        7,
        19,
        6,
        12,
        4
       ]

  zs = L.sort
       [28,
        33,
        18,
        42,
        31,
        14,
        46,
        20,
        48,
        47,
        24,
        23,
        49,
        45,
        19,
        38,
        39,
        11,
        1,
        32,
        25,
        35,
        8,
        17,
        7,
        9,
        4,
        2,
        34,
        10,
        3
       ]