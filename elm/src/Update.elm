module Update exposing (..)

import Shapes
import Parsing
import Pixels exposing (Pixels)
import Circle2d exposing (Circle2d)
import Rectangle2d exposing (Rectangle2d)
import Point2d exposing (Point2d)

type Msg
    = NumberChange String
    | UpdateCode String


type alias Model =
    { numberString : String
    , code : String
    , shapes : List (Shapes.Shape Pixels.Pixels Shapes.Coordinates)
    , canvasHeight : Int
    , canvasWidth : Int
    }

init : Model
init =
    { numberString = "3"
    , code = "Circle 20 100 100\nRectangle 100 100 150 150"
    , shapes = [myCircleShape, myRectangleShape]
    , canvasHeight = 200
    , canvasWidth = 200
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NumberChange number ->
            { model | numberString = number }
        UpdateCode text ->
            { model | code = text, shapes = Parsing.shapesFromText text}



myCircleShape : Shapes.Shape Pixels.Pixels coordinates
myCircleShape =
    let 
        myCircle = Circle2d.withRadius (Pixels.float 10) (Point2d.fromTuple Pixels.float ( 150, 150 ))
    in
    Shapes.Circle myCircle

myRectangleShape : Shapes.Shape Pixels.Pixels coordinates
myRectangleShape =
    let
        record = {
            x1 = Pixels.pixels 150.0,
            x2 = Pixels.pixels 200.0,
            y1 = Pixels.pixels 150.0,
            y2 = Pixels.pixels 200.0
            }
    in
    Shapes.Rectangle <| Rectangle2d.with record

