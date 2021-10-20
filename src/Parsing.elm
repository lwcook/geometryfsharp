module Parsing exposing (..)

import Shapes
import Pixels
import Circle2d
import Rectangle2d
import Point2d
import Shapes exposing (Shape(..))

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

rectLineToShape : List String -> Maybe (Shapes.Shape Pixels.Pixels coordinates)
rectLineToShape args =
    case args of 
        [_, x1s, y1s, x2s, y2s] ->
            let 
                x1 = Maybe.withDefault 100.0 (String.toFloat x1s)
                y1 = Maybe.withDefault 100.0 (String.toFloat y1s)
                x2 = Maybe.withDefault 100.0 (String.toFloat x2s)
                y2 = Maybe.withDefault 100.0 (String.toFloat y2s)
                record = {
                    x1 = Pixels.pixels x1,
                    x2 = Pixels.pixels x2,
                    y1 = Pixels.pixels y1,
                    y2 = Pixels.pixels y2
                    }
            in
                Just <| Shapes.Rectangle (Rectangle2d.with record)
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
                "Rectangle" -> rectLineToShape args
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