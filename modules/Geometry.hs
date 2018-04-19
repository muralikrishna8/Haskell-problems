module Geometry (
    circleArea,
    squareArea
) where

circleArea :: Float -> Float
circleArea radius = pi * radius ^ 2

squareArea :: Float -> Float
squareArea a = a ^ 2
