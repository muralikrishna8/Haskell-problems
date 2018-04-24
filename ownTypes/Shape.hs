-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving Show
data Point = Point Float Float deriving Show
data Shape = Circle Point Float | Rectangle Point Point deriving Show

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRectangle :: Float -> Float -> Shape
baseRectangle width height = Rectangle (Point 0 0) (Point width height)

--nudge (baseCircle 39) 3 4 = Circle (Point 3.0 4.0) 39.0
--nudge (baseRectangle 3 4) 4 5 = Rectangle (Point 4.0 5.0) (Point 7.0 9.0)
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

-- surface $ Circle (Point 1 2) 1 = 3.1415927
-- surface $ Rectangle (Point 1 2) (Point 3 4) = 4.0
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)