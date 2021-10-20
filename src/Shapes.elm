module Shapes exposing (..)
import Point2d exposing (Point2d)
import Pixels
import Circle2d exposing (Circle2d)
import Rectangle2d exposing (Rectangle2d)
import Pixels exposing (Pixels)
import Svg exposing (Svg)
import Svg.Attributes
import Geometry.Svg as Svg

type Coordinates = Coordinates

-- Is using Coordinates here the right thing to do
-- The type variable coordinates has to be concrete here. 
-- I guess it's up to me to decide what this is
-- It's a phantom type, so it doesn't matter, as long as I use this type everywhere? 
type Shape units coordinates
    = Circle (Circle2d units coordinates)
    | Rectangle (Rectangle2d units coordinates)


toCircleSvg : Circle2d Pixels.Pixels coordinates -> Svg msg
toCircleSvg =
    Svg.circle2d
        [ Svg.Attributes.fill "transparent"
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "1"
        ]

toRectangleSvg : Rectangle2d Pixels.Pixels coordinates -> Svg msg
toRectangleSvg =
    Svg.rectangle2d
        [ Svg.Attributes.fill "transparent"
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "1"
        ] 


fromShape : Shape Pixels.Pixels coordinates -> Svg msg
fromShape shape =
    case shape of
        Circle circle ->
            toCircleSvg circle

        Rectangle rect ->
            toRectangleSvg rect
