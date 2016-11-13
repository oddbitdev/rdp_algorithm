data Point = Point {x::Double, y::Double} deriving Show

data Line = Line {first::Point, second::Point} deriving Show

perpDist :: Point -> Line -> Double
perpDist p l = abs(nom / denom)
    where nom   = ((y(second l) - y(first l)) * x p) - ((x(second l) - x(first l)) * y p)
                  + (x(second l) * y(first l)) - (y(second l) * x(first l))
          denom = sqrt((y(second l) - y(first l))^2 + (x(second l) - x(first l))^2)

farthestPoint :: ((Point, Int), (Point, Int)) -> Line -> (Point, Int)
farthestPoint ((p, i), (p', i')) l = if (perpDist p l) > (perpDist p' l) then (p, i) else (p', i')

maxIndex :: [(Point, Int)] -> Line -> (Point, Int)
maxIndex [a] l = a
maxIndex [a, b] l = farthestPoint (a, b) l
maxIndex (x:xs) l = farthestPoint (x, (maxIndex xs l)) l

findFarthestPoint :: [Point] -> Line -> (Double, Int)
findFarthestPoint [] l   = (0.0, 0)
findFarthestPoint (xs) l = ((perpDist (fst result) l), (snd result))
    where pointsAndIndices = zip xs [0..length xs]
          result           = maxIndex pointsAndIndices l

simplify :: [Point] -> Double -> [Point]
simplify [] eps     = []
simplify [a, b] eps = [a, b]
simplify (xs) eps   = if dmax > eps then (init before) ++ after else [head xs] ++ [last xs]
    where dmaxAndIndex  = findFarthestPoint xs (Line (head xs) (last xs))
          dmax          = fst dmaxAndIndex
          index         = snd dmaxAndIndex
          before        = simplify (take (index+1) xs) eps
          after         = simplify (drop index xs) eps

