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
import Angle
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
import Rectangle2d

swap : (a -> b -> c) -> b -> a -> c
swap f x y = f y x

-- SHAPES

-- Use one type singleton variable for all coordinates. 
-- Not doing any global / local shenanigans yet
type Coordinates = Coordinates

type Shape units coordinates
    = Circle (Circle2d.Circle2d units coordinates)
    | Rectangle (Rectangle2d.Rectangle2d units coordinates)


type alias ShapeModel units coordinates = 
    { shape : Shape units coordinates
    , hovered : Bool
    , codeLine : Int
    , code : String
    , uncertainty : List (Shape units coordinates)
    , selected : Bool
    }


extractShapes : List (ShapeModel units coordinates) -> List (Shape units coordinates)
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


toCircleSvg : List (Svg.Attribute msg) -> Circle2d.Circle2d units coordinates -> Svg.Svg msg
toCircleSvg attributes =
    Svg.circle2d <|
        [ Svg.Attributes.fill "transparent"
        , Svg.Attributes.strokeWidth "1"
        ] 
        ++ attributes

toRectangleSvg : List (Svg.Attribute msg) -> Rectangle2d.Rectangle2d units coordinates -> Svg.Svg msg
toRectangleSvg attributes =
    Svg.rectangle2d <|
        [ Svg.Attributes.fill "transparent"
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "1"
        ] 
        ++ attributes

fromIndividualShape : List (Svg.Attribute msg) -> Shape units coordinates -> Svg.Svg msg
fromIndividualShape attributes shape =
    case shape of
        Circle circle ->
            toCircleSvg attributes circle

        Rectangle rect ->
            toRectangleSvg attributes rect
    

svgFromShapeData : ShapeModel units coordinates -> List (Svg.Svg msg)
svgFromShapeData selectedShape =
    let 
        {shape, hovered} = selectedShape
        stroke = if hovered then "Red" else "Black"
        attributes = [ Svg.Attributes.stroke stroke ]
        uncertainAttributes = [ Svg.Attributes.stroke "Grey"]
        -- TODO: Make the uncertain shapes visible on screen
    in
    [fromIndividualShape attributes shape] ++ (List.map (fromIndividualShape uncertainAttributes) selectedShape.uncertainty)

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

lineToBaseShape : String -> Int -> Maybe (ShapeModel Pixels.Pixels coordinates)
lineToBaseShape text lineIndex = 
    let 
        args = String.split " " text 
        first = List.head args
        toSelected shape = ShapeModel shape False lineIndex text [] False
    in
    case first of 
        Nothing -> Nothing
        Just elem -> 
            case elem of
                "Circle" -> Maybe.map toSelected (circleLineToShape args)
                "Rectangle" -> Maybe.map toSelected (rectLineToShape args)
                _ -> Nothing

uncertainRectangle : Rectangle2d.Rectangle2d units coordinates -> List (Rectangle2d.Rectangle2d units coordinates)
uncertainRectangle rect =
    let
         (l0, h0) = Rectangle2d.dimensions rect 
         angle = Angle.degrees 0
         newD dims = Rectangle2d.withDimensions dims angle (Rectangle2d.centerPoint rect)
    in
    List.map newD 
        [ (Quantity.multiplyBy 0.9 l0, h0)
        , (Quantity.multiplyBy 1.1 l0, h0)
        , (l0, Quantity.multiplyBy 0.9 h0)
        , (l0, Quantity.multiplyBy 1.1 h0)
        ]

uncertainCircle : Circle2d.Circle2d units coordinates -> List (Circle2d.Circle2d units coordinates)
uncertainCircle circ =
    let
        r0 = Circle2d.radius circ
        -- TODO: Sample some distribution for these, don't hard code
        newR = [Quantity.multiplyBy 1.1 r0, Quantity.multiplyBy 0.9 r0]
    in 
    List.map (\r -> Circle2d.withRadius r (Circle2d.centerPoint circ)) newR
    

updateShapeWithUncertainty : ShapeModel units coordinates -> Maybe (ShapeModel units coordinates)
updateShapeWithUncertainty start = 
    let
        new = case start.shape of 
                Rectangle rect -> { start | uncertainty = List.map Rectangle (uncertainRectangle rect)}
                Circle circ -> { start | uncertainty = List.map Circle (uncertainCircle circ) }
    in
    Just new
    

nextStep : ShapeModel units coordinates -> Maybe (ShapeModel units coordinates)
nextStep shape = Just shape

lineToShape : String -> Int -> Maybe (ShapeModel Pixels.Pixels coordinates)
lineToShape line lineIndex = 
    lineToBaseShape line lineIndex
    |> Maybe.andThen updateShapeWithUncertainty
    |> Maybe.andThen nextStep


shapesFromText : String -> List (ShapeModel Pixels.Pixels coordinates)
shapesFromText txt = 
    let
        lines = String.split "\n" txt
        enumeratedLines = List.indexedMap (\index line -> (index, line)) lines
        listify : Maybe (ShapeModel units coordinates) -> List (ShapeModel units coordinates)
        listify maybeShape =
            case maybeShape of
               Nothing -> []
               Just shape -> [shape]

    in 
    List.concatMap (listify << (\(index, line) -> lineToShape line index)) enumeratedLines  
    |> combineShapes


-- Representing some next step
combineShapes : List (ShapeModel units coordinates) -> List (ShapeModel units coordinates)
combineShapes shapes = shapes


-- MESSAGES AND MODELS

type Msg
    = NumberChange String
    | UpdateCode String
    | Generated (List (List Float))
    | Generate
    | MouseMove Float Float
    | MouseDown Float Float
    | MouseUp Float Float
    | GetElement (Task.Task Browser.Dom.Error Browser.Dom.Element, String)
    | GotElement Browser.Dom.Element
    | StayTheSame

type alias Model =
    { numberString : String
    , code : String
    , shapes : List (ShapeModel Pixels.Pixels Coordinates)
    , canvasHeight : Int
    , canvasWidth : Int
    , distribution : Stats.TruncatedGaussianDistribution
    , currentSample : List (List Float)
    , absMousePosition : (Float, Float)
    , elementPosition : (Float, Float)
    , relativePosition : (Float, Float)
    , currentElementName : String
    }

startingCode : String
startingCode = "Circle 20 100 100\nRectangle 100 100 150 150\n" 

updateHoveredShapes : List Bool -> List (ShapeModel units coordinates) -> List (ShapeModel units coordinates)
updateHoveredShapes toSelect shapes = 
    let updateHovered isSelected current = { current | hovered = isSelected}
    in
    List.map2 updateHovered toSelect shapes

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
    , currentElementName = ""
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
    in 
    { model 
    | absMousePosition = (x, y)
    , relativePosition = relPos
    }

updateOnMouseDown : Model -> Model
updateOnMouseDown model =  -- Mouse position kept track of separately
    model

updateOnMouseUp : Model -> Model
updateOnMouseUp model = 
    model


updateTextWithSelected : Model -> UpdateOrCommand Model
updateTextWithSelected model =
    let
        currentText = model.code
        lines = String.split "\n" currentText
        selectedLines = List.filterMap (\s -> if s.hovered then Just s.codeLine else Nothing) model.shapes 
        updateLine i l =
            if List.member i selectedLines 
            then 
                if String.endsWith "<-" l then l else (l ++ " <-") 
            else 
                if String.endsWith " <-" l then String.dropRight 3 l else l
        updatedLines = List.indexedMap (\index line -> updateLine index line) lines
        newText = String.join "\n" updatedLines
    in
    JustModel { model | code = newText }
    
shapesWithMouseOnBoundary : (Float, Float) -> Model -> List Bool
shapesWithMouseOnBoundary (relX, relY) model =
    let 
        point = (Point2d.fromPixels {x=relX, y=relY})
        select shape = onShapeBoundary shape point
    in
    List.map select <| extractShapes model.shapes

hoveredShapesDirect : (Float, Float) -> Model -> Model
hoveredShapesDirect (relX, relY) model =
    let 
        nextSelected = shapesWithMouseOnBoundary (relX, relY) model
    in
    { model | shapes = updateHoveredShapes nextSelected model.shapes }

hoveredShapes : Model -> UpdateOrCommand Model
hoveredShapes model =
     JustModel <| hoveredShapesDirect model.relativePosition model
    

-- We want to chain things that either continue with updating functions
-- , or run a command then go back to the begining
type UpdateOrCommand m =
    JustModel m
    | WithCommand (Cmd Msg) m

bind : (m -> UpdateOrCommand m) -> UpdateOrCommand m -> UpdateOrCommand m
bind callback value =
    case value of
        JustModel model -> callback model
        WithCommand command model -> WithCommand command model -- ignore later callbacks

finish : UpdateOrCommand m -> (m, Cmd Msg)
finish value = 
    case value of
        JustModel model -> (model, Cmd.none)
        WithCommand command model -> (model, command)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let 
        firstUpdate : Model -> UpdateOrCommand Model
        firstUpdate = case msg of
            Generate -> 
                WithCommand <| Random.generate Generated (Stats.generatorFromGaussian model.distribution 100)
            Generated listOfSamples -> 
                JustModel << (swap updateSample) listOfSamples
            NumberChange number ->
                JustModel << \m -> { m | numberString = number }
            UpdateCode text ->
                JustModel << \m -> { m | code = text, shapes = shapesFromText text}
            MouseMove x y -> JustModel << updateOnMouseMove x y 
            MouseDown _ _ -> JustModel << updateOnMouseDown
            MouseUp _ _ -> JustModel << updateOnMouseUp
            GetElement (task, name) -> WithCommand (cmdFromGetElement task) << (\m -> {m | currentElementName = name})
            GotElement elemAndName -> JustModel << updateModelWithElement elemAndName
            StayTheSame -> JustModel
        
    in 
    JustModel model
    |> bind firstUpdate  -- After this is things we always want to do, they may have commands too
    |> bind hoveredShapes
    |> bind updateTextWithSelected
    |> finish


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
        elements = Svg.g [ ] (List.concatMap svgFromShapeData <| model.shapes)

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
    let
        getElementByName name = (Browser.Dom.getElement name, name)
    in
    Html.div []
        [ Html.div [ Html.Events.onMouseOver (MouseMove 100 100 )]
            [ Html.textarea
                [ Html.Attributes.value model.code
                , Html.Attributes.rows 15
                , Html.Attributes.id "my-thing"
                , Html.Attributes.cols 79
                , Html.Events.onInput (\txt -> UpdateCode txt)
                , Html.Events.onMouseOver (GetElement <| getElementByName "my-thing")
                ]
                []
            ]
        , Html.div 
            [ Html.Attributes.id "other" 
            , Html.Events.onMouseOver (GetElement <| getElementByName "other")
            ] [Html.button [Html.Events.onClick Generate] [Html.text "Generate"]]
        , debugView model.shapes
        , debugView model.currentElementName
        , htmlCoords model.absMousePosition
        , htmlCoords model.elementPosition
        , htmlCoords model.relativePosition
        , Html.div 
            [ Html.Attributes.id "canvas"
            , Html.Events.onMouseOver (GetElement <| getElementByName "canvas")
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
        x = (Json.Decode.field "pageX" Json.Decode.float)
        y = (Json.Decode.field "pageY" Json.Decode.float)
        decoder = Json.Decode.map2 MouseMove x y
    in 
    Sub.batch 
        [ Browser.Events.onMouseMove decoder
        , Browser.Events.onMouseDown decoder
        , Browser.Events.onMouseUp decoder
        ]

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

