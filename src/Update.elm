module Update exposing (..)

import Shapes
import Parsing
import Pixels exposing (Pixels)
import Circle2d exposing (Circle2d)
import Rectangle2d exposing (Rectangle2d)
import Point2d exposing (Point2d)
import Random
import Stats
import Real
import Vector

type Msg
    = NumberChange String
    | UpdateCode String
    | Generated (List (List Float))
    | Generate


type alias Model =
    { numberString : String
    , code : String
    , shapes : List (Shapes.Shape Pixels.Pixels Shapes.Coordinates)
    , canvasHeight : Int
    , canvasWidth : Int
    , distribution : Stats.TruncatedGaussianDistribution
    , currentSample : List (List Float)
    }

startingCode : String
startingCode = "Circle 20 100 100\nRectangle 100 100 150 150\n" 

init : Model
init =
    { numberString = "3"
    , code = startingCode
    , shapes = Parsing.shapesFromText startingCode
    , canvasHeight = 200
    , canvasWidth = 200
    , distribution = Stats.normal2d
    , currentSample = []
    }

updateSample : Model -> List (List Float) -> Model
updateSample model listOfSamples =
    let
        singleSample sample = Stats.rejectionSampleGaussianTruncated model.distribution (Vector.Vector <| List.map (Real.Real) sample)
        allSamples = List.map singleSample listOfSamples
        extractFromReal real = let (Real.Real num) = real in num
        extractFromVector vec = let (Vector.Vector theList) = vec in List.map extractFromReal theList 
    in
    {model | currentSample = List.map extractFromVector allSamples}
    

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate -> 
            (model, Random.generate Generated (Stats.generatorFromGaussian model.distribution 100))
        Generated listOfSamples -> 
            (updateSample model listOfSamples, Cmd.none)
        NumberChange number ->
            ({ model | numberString = number }, Cmd.none)
        UpdateCode text ->
            ({ model | code = text, shapes = Parsing.shapesFromText text}, Cmd.none)



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

