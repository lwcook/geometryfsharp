module Parsing exposing (..)

import Shapes
import Pixels
import Circle2d
import Point2d
import Shapes exposing (Shape)

circleLineToShape : List String -> Maybe (Shapes.Shape Pixels.Pixels coordinates)
circleLineToShape args =
    case args of 
        [_,rs,xs,ys] -> 
            let
                r = Maybe.withDefault 100.0 (String.toFloat rs)
                x = Maybe.withDefault 100.0 (String.toFloat xs)
                y = Maybe.withDefault 100.0 (String.toFloat ys)
            in
                Just <| Shapes.Circle (Circle2d.withRadius (Pixels.float r) (Point2d.fromTuple Pixels.float (x, y)))
        _ -> Nothing

lineToShape : String -> Maybe (Shapes.Shape Pixels.Pixels coordinates)
lineToShape text = 
    let 
        args = String.split " " text 
        first = List.head args
    in
    case first of 
        Nothing -> Nothing
        Just elem -> 
            case elem of
                "Circle" -> circleLineToShape args
                _ -> Nothing

shapesFromText : String -> List (Shapes.Shape Pixels.Pixels coordinates)
shapesFromText txt = 
    let
        lines = String.split "\n" txt
        listify : Maybe (Shapes.Shape units coordinates) -> List (Shapes.Shape units coordinates)
        listify maybeShape =
            case maybeShape of
               Nothing -> []
               Just shape -> [shape]
            
    in List.concatMap (listify << lineToShape) lines 