module App exposing (..)

-- import Frame3d
-- import Point3d
-- import Polygon2d
-- import Quantity

import Browser
import Geometry.Svg as Svg
import Html
import Html.Attributes
import Html.Events
import Stats
import Vector
import Real
import Plots
import Svg
import Browser.Events
import Json.Decode
import Browser.Dom
import Task
import Pixels
import Random
import Point2d
import Circle2d
import Rectangle2d
import Quantity
import Svg.Attributes
import Rectangle2d exposing (Rectangle2d)
import Frame2d


-- SHAPES

type Coordinates = Coordinates

-- Is using Coordinates here the right thing to do
-- The type variable coordinates has to be concrete here. 
-- I guess it's up to me to decide what this is
-- It's a phantom type, so it doesn't matter, as long as I use this type everywhere? 
type Shape units coordinates
    = Circle (Circle2d.Circle2d units coordinates)
    | Rectangle (Rectangle2d.Rectangle2d units coordinates)


type alias SelectedShape units coordinates = 
    { shape : Shape units coordinates
    , selected : Bool
    , codeLine : Int
    }


extractShapes : List (SelectedShape units coordinates) -> List (Shape units coordinates)
extractShapes selectedShapes =
    List.map (\s -> s.shape) selectedShapes

onRectBoundary : Rectangle2d.Rectangle2d units coordinates -> Point2d.Point2d units coordinates -> Bool
onRectBoundary rect point = 
    let
        frame = Rectangle2d.axes rect
        xLength = Quantity.multiplyBy 0.5 <| Tuple.first <| Rectangle2d.dimensions rect
        yLength = Quantity.multiplyBy 0.5 <| Tuple.second <| Rectangle2d.dimensions rect
        pointInFrame = Point2d.relativeTo frame point
        tolerance = Quantity.multiplyBy 0.08 xLength
        inXBounds = Quantity.lessThan (Quantity.multiplyBy 1.08 xLength) (Quantity.abs <| Point2d.xCoordinate pointInFrame)
        inYBounds = Quantity.lessThan (Quantity.multiplyBy 1.08 yLength) (Quantity.abs <| Point2d.yCoordinate pointInFrame)

        axisDistance axisCoord axisLength = 
            let
                xDist1 = Quantity.abs <| Quantity.minus axisCoord axisLength
                xDist2 = Quantity.abs <| Quantity.minus axisCoord (Quantity.negate axisLength)
            in 
            Quantity.min xDist1 xDist2
        
        xDist = axisDistance (Point2d.xCoordinate pointInFrame) xLength
        yDist = axisDistance (Point2d.yCoordinate pointInFrame) yLength

    in
        if Quantity.lessThan tolerance xDist && inYBounds
        then True
        else if
            Quantity.lessThan tolerance yDist && inXBounds
            then True
            else False

onCircleBoundary : Circle2d.Circle2d units coordinates -> Point2d.Point2d units coordinates -> Bool
onCircleBoundary circle point =
    let 
        distFromCenter = Point2d.distanceFrom point (Circle2d.centerPoint circle)
        tolerance = Quantity.multiplyBy 0.08 (Circle2d.radius circle)
        difference = Quantity.abs <| Quantity.difference distFromCenter (Circle2d.radius circle) 
    in Quantity.lessThan tolerance difference

onShapeBoundary : Shape units coordinates -> Point2d.Point2d units coordinates -> Bool
onShapeBoundary shape point =
   case shape of
       Circle circle -> onCircleBoundary circle point
       Rectangle rect -> onRectBoundary rect point


toCircleSvg : List (Svg.Attribute msg) -> Circle2d.Circle2d Pixels.Pixels coordinates -> Svg.Svg msg
toCircleSvg attributes =
    Svg.circle2d <|
        [ Svg.Attributes.fill "transparent"
        , Svg.Attributes.strokeWidth "1"
        ] 
        ++ attributes

toRectangleSvg : List (Svg.Attribute msg) -> Rectangle2d.Rectangle2d Pixels.Pixels coordinates -> Svg.Svg msg
toRectangleSvg attributes =
    Svg.rectangle2d <|
        [ Svg.Attributes.fill "transparent"
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "1"
        ] 
        ++ attributes


fromShape : SelectedShape Pixels.Pixels coordinates -> Svg.Svg msg
fromShape selectedShape =
    let 
        {shape, selected} = selectedShape
        stroke = case selected of 
            True -> "Red" 
            False -> "Black"
        attributes = [ Svg.Attributes.stroke stroke ]
    in
    case shape of
        Circle circle ->
            toCircleSvg attributes circle

        Rectangle rect ->
            toRectangleSvg attributes rect

-- PARSING

circleLineToShape : List String -> Maybe (Shape Pixels.Pixels coordinates)
circleLineToShape args =
    case args of 
        [_,rs,xs,ys] ->
            let 
                r = Maybe.withDefault 100.0 (String.toFloat rs)
                x = Maybe.withDefault 100.0 (String.toFloat xs)
                y = Maybe.withDefault 100.0 (String.toFloat ys)
            in
                Just <| Circle (Circle2d.withRadius (Pixels.float r) (Point2d.fromTuple Pixels.float (x, y)))
        _ -> Nothing

rectLineToShape : List String -> Maybe (Shape Pixels.Pixels coordinates)
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
                Just <| Rectangle (Rectangle2d.with record)
        _ -> Nothing

lineToShape : String -> Maybe (Shape Pixels.Pixels coordinates)
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

shapesFromText : String -> List (SelectedShape Pixels.Pixels coordinates)
shapesFromText txt = 
    let
        lines = String.split "\n" txt
        enumeratedLines = List.indexedMap (\index line -> (index, line)) lines
        listify : (Int, Maybe (Shape units coordinates)) -> List (SelectedShape units coordinates)
        listify (index, maybeShape) =
            case maybeShape of
               Nothing -> []
               Just shape -> [(SelectedShape shape False index)]

    in 
    List.concatMap (listify << (\(index, line) -> (index, lineToShape line))) enumeratedLines  


-- MESSAGES AND MODELS

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
    , shapes : List (SelectedShape Pixels.Pixels Coordinates)
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

updateSelectedShapes : List Bool -> List (SelectedShape units coordinates) -> List (SelectedShape units coordinates)
updateSelectedShapes toSelect shapes = 
    List.map2 (\isSelected ({shape, selected, codeLine}) -> SelectedShape shape isSelected codeLine) toSelect shapes

init : Model
init =
    { numberString = "3"
    , code = startingCode
    , shapes = shapesFromText startingCode
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
        point = (Point2d.fromPixels {x=relX, y=relY})
        select shape = onShapeBoundary shape point
        nextSelected = List.map select <| extractShapes model.shapes
    in
    { model | shapes = updateSelectedShapes nextSelected model.shapes }

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
            ({ model | code = text, shapes = shapesFromText text}, Cmd.none)
        MouseMove x y -> (updateOnMouseMove x y model, Cmd.none)
        GetElement task -> (model, cmdFromGetElement task)
        GotElement elem -> (updateModelWithElement elem model, Cmd.none)
        StayTheSame -> (model, Cmd.none)


-- INITIALISATION AND VIEWING

myCircleShape : Shape Pixels.Pixels coordinates
myCircleShape =
    let 
        myCircle = Circle2d.withRadius (Pixels.float 10) (Point2d.fromTuple Pixels.float ( 150, 150 ))
    in
    Circle myCircle

myRectangleShape : Shape Pixels.Pixels coordinates
myRectangleShape =
    let
        record = {
            x1 = Pixels.pixels 150.0,
            x2 = Pixels.pixels 200.0,
            y1 = Pixels.pixels 150.0,
            y2 = Pixels.pixels 200.0
            }
    in
    Rectangle <| Rectangle2d.with record


canvas : Model -> Html.Html msg
canvas model =
    let
        -- topLeftFrame = Frame2d.atPoint (Point2d.pixels 100 100)
        elements = Svg.g [ ] (List.map fromShape <| model.shapes)

        -- scene = Svg.relativeTo topLeftFrame elements
        scene = elements
    in
    Svg.svg []
        [ scene ]


toPlottableSample : List (List Float) -> List (Float, Float)
toPlottableSample sampled =
    let 
        toSingle : List Float -> (Float, Float)
        toSingle sample =
            case sample of
                [x, y] -> (x, y)
                [x, y, _] -> (x, y)
                [x] -> (x, 0)
                _  -> (0, 0)
    in
    List.map toSingle sampled

htmlCoords : (Float, Float) -> Html.Html msg
htmlCoords (x, y) = 
        Html.div [] 
            [ Html.text <| String.fromFloat <| x
            , Html.text <| ", "
            , Html.text <| String.fromFloat <| y
            ]

debugView : a -> Html.Html msg
debugView thing =
        Html.div [] [Html.text <| Debug.toString thing]

view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div [ Html.Events.onMouseOver (MouseMove 100 100 )]
            [ Html.textarea
                [ Html.Attributes.value model.code
                , Html.Attributes.rows 15
                , Html.Attributes.id "my-thing"
                , Html.Attributes.cols 79
                , Html.Events.onInput (\txt -> UpdateCode txt)
                , Html.Events.onMouseOver (GetElement <| Browser.Dom.getElement "my-thing")
                ]
                []
            ]
        , Html.div 
            [ Html.Attributes.id "other" 
            , Html.Events.onMouseOver (GetElement <| Browser.Dom.getElement "other")
            ] [Html.button [Html.Events.onClick Generate] [Html.text "Generate"]]
        , debugView model.shapes
        , htmlCoords model.absMousePosition
        , htmlCoords model.elementPosition
        , htmlCoords model.relativePosition
        , Html.div 
            [ Html.Attributes.id "canvas"
            , Html.Events.onMouseOver (GetElement <| Browser.Dom.getElement "canvas")
            ]
            [ Svg.svg
                [ Html.Attributes.height model.canvasHeight
                , Html.Attributes.width model.canvasWidth
                ]
                [ canvas model ]
            ]
        , Html.div []
            [ Plots.viewScatter <| toPlottableSample model.currentSample]
        , Html.div []
            [ Html.text <| "Total number of shapes: " ++ String.fromInt (List.length model.shapes)
            ]
        , Html.div [] [ Plots.viewPlot timeSeries]
        ]

subscriptions : Model -> Sub Msg
subscriptions _ =
    let 
        first = (Json.Decode.field "pageX" Json.Decode.float)
        second = (Json.Decode.field "pageY" Json.Decode.float)
        decoder = Json.Decode.map2 MouseMove first second
    in 
    Sub.batch [Browser.Events.onMouseMove decoder]

main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , update = \message model -> update message model
        , view = view
        , subscriptions = subscriptions
        }

timeSeries : List ( Float, Float )
timeSeries =
    let 
        vectorify x = Vector.Vector <| [Real.Real x, Real.Real 0.0]
        f = \x -> (x, (Stats.gaussianPDF Stats.normal2d (vectorify x)))
        scale = 0.5
    in List.map (toFloat >> ((*) scale >> f )) (List.range -10 10)

