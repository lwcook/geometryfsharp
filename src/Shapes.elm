module Shapes exposing (..)
import Point2d
import Pixels
import Quantity
import Circle2d
import Rectangle2d exposing (Rectangle2d)
import Pixels exposing (Pixels)
import Svg exposing (Svg)
import Svg.Attributes
import Geometry.Svg as Svg
import Point2d exposing (Point2d)
import Svg exposing (circle)
import Circle2d exposing (Circle2d)
import Dict exposing (diff)

type Coordinates = Coordinates

-- Is using Coordinates here the right thing to do
-- The type variable coordinates has to be concrete here. 
-- I guess it's up to me to decide what this is
-- It's a phantom type, so it doesn't matter, as long as I use this type everywhere? 
type Shape units coordinates
    = Circle (Circle2d.Circle2d units coordinates)
    | Rectangle (Rectangle2d units coordinates)


type alias SelectedShape units coordinates = 
    { shape : Shape units coordinates
    , selected : Bool
    }

onCircleBoundary : Circle2d.Circle2d units coordinates -> Point2d.Point2d units coordinates -> Bool
onCircleBoundary circle point =
    let 
        distFromCenter = Point2d.distanceFrom point (Circle2d.centerPoint circle)
        tolerance = Quantity.multiplyBy 0.08 (Circle2d.radius circle)
        difference = Quantity.abs <| Quantity.difference distFromCenter (Circle2d.radius circle) 
    in Quantity.lessThan tolerance difference

onRectBoundary : Rectangle2d.Rectangle2d units coordinates -> Point2d.Point2d units coordinates -> Bool
onRectBoundary rect point = False

onShapeBoundary : Shape units coordinates -> Point2d.Point2d units coordinates -> Bool
onShapeBoundary shape point =
   case shape of
       Circle circle -> onCircleBoundary circle point
       Rectangle rect -> onRectBoundary rect point


toCircleSvg : Circle2d.Circle2d Pixels.Pixels coordinates -> Svg msg
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
