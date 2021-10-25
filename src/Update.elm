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
import Task
import Browser.Dom
import Point2d exposing (coordinates)

type Msg
    = NumberChange String
    | UpdateCode String
    | Generated (List (List Float))
    | Generate
    | MouseMove Float Float
    | GetElement (Task.Task Browser.Dom.Error Browser.Dom.Element)
    | GotElement Browser.Dom.Element
    | StayTheSame


type alias Model =
    { numberString : String
    , code : String
    , shapes : List (Shapes.SelectedShape Pixels.Pixels Shapes.Coordinates)
    , canvasHeight : Int
    , canvasWidth : Int
    , distribution : Stats.TruncatedGaussianDistribution
    , currentSample : List (List Float)
    , absMousePosition : (Float, Float)
    , elementPosition : (Float, Float)
    , relativePosition : (Float, Float)
    }

startingCode : String
startingCode = "Circle 20 100 100\nRectangle 100 100 150 150\n" 

startingSelected : List Bool
startingSelected = [False, False]

createSelectedShapes : List Bool -> List (Shapes.Shape units coordinates) -> List (Shapes.SelectedShape units coordinates)
createSelectedShapes selected shapes = 
    List.map2 (\isSelected shape -> Shapes.SelectedShape shape isSelected) selected shapes

init : Model
init =
    { numberString = "3"
    , code = startingCode
    , shapes = createSelectedShapes startingSelected (Parsing.shapesFromText startingCode)
    , canvasHeight = 200
    , canvasWidth = 200
    , distribution = Stats.normal2d
    , currentSample = []
    , absMousePosition = (0, 0)
    , elementPosition = (0, 0)
    , relativePosition = (0, 0)
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
    

cmdFromGetElement : Task.Task Browser.Dom.Error Browser.Dom.Element -> Cmd Msg
cmdFromGetElement task = 
    let 
        resToMsg res = 
            case res of 
                Ok elem -> GotElement elem
                Err _ -> StayTheSame

    in Task.attempt resToMsg task

updateModelWithElement : Browser.Dom.Element -> Model -> Model
updateModelWithElement elem model = 
    let (absX, absY) = model.absMousePosition
    in 
    { model 
    | elementPosition = (elem.element.x, elem.element.y) 
    , relativePosition = (absX - elem.element.x, absY - elem.element.y)
    }

updateOnMouseMove : Float -> Float -> Model -> Model
updateOnMouseMove x y model = 
    let 
        (elemX, elemY) = model.elementPosition
        relPos = (x - elemX, y - elemY)
        first = 
            { model 
            | absMousePosition = (x, y)
            , relativePosition = relPos
            }
    in 
    selectShapes relPos first

selectShapes : (Float, Float) -> Model -> Model
selectShapes (relX, relY) model =
    let 
        select : Shapes.SelectedShape Pixels.Pixels coordinates -> Bool
        select selectedShape = 
            let 
                {shape, selected} = selectedShape
            in 
                case shape of 
                    Shapes.Rectangle rect -> Shapes.onRectBoundary rect (Point2d.fromPixels {x=relX, y=relY})
                    Shapes.Circle circle -> Shapes.onCircleBoundary circle (Point2d.fromPixels {x=relX, y=relY})

        nextSelcted = List.map select model.shapes
    in
    { model | shapes = createSelectedShapes nextSelcted model.shapes }

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
        MouseMove x y -> (updateOnMouseMove x y model, Cmd.none)
        GetElement task -> (model, cmdFromGetElement task)
        GotElement elem -> (updateModelWithElement elem model, Cmd.none)
        StayTheSame -> (model, Cmd.none)



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

