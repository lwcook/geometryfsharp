module App exposing (..)

-- import Frame3d
-- import Point3d
-- import Polygon2d
-- import Quantity

import Browser
import Geometry.Svg as Svg
import Html
import Html.Attributes
import Html.Events as Events
import Shapes exposing (Shape(..))
import Update
import Stats
import Vector
import Real
import Plots
import Svg
import Browser.Events
import Json.Decode
import Browser.Dom
import Update exposing (Msg(..))

canvas : Update.Model -> Html.Html msg
canvas model =
    let
        -- topLeftFrame = Frame2d.atPoint (Point2d.pixels 100 100)

        elements = Svg.g [] (List.map Shapes.fromShape model.shapes)

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

view : Update.Model -> Html.Html Update.Msg
view model =
    Html.div []
        [ Html.div [ Events.onMouseOver (Update.MouseMove 100 100 )]
            [ Html.textarea
                [ Html.Attributes.value model.code
                , Html.Attributes.rows 15
                , Html.Attributes.id "my-thing"
                , Html.Attributes.cols 79
                , Events.onInput (\txt -> Update.UpdateCode txt)
                , Events.onMouseOver (Update.GetElement <| Browser.Dom.getElement "my-thing")
                ]
                []
            ]
        , Html.div 
            [ Html.Attributes.id "other" 
            , Events.onMouseOver (Update.GetElement <| Browser.Dom.getElement "other")
            ] [Html.button [Events.onClick Update.Generate] [Html.text "Generate"]]
        , debugView model.selectedShapes
        , debugView model.shapes
        , htmlCoords model.absMousePosition
        , htmlCoords model.elementPosition
        , htmlCoords model.relativePosition
        , Html.div 
            [ Html.Attributes.id "canvas"
            , Events.onMouseOver (Update.GetElement <| Browser.Dom.getElement "canvas")
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

subscriptions : Update.Model -> Sub Update.Msg
subscriptions model =
    let 
        first = (Json.Decode.field "pageX" Json.Decode.float)
        second = (Json.Decode.field "pageY" Json.Decode.float)
        decoder = Json.Decode.map2 Update.MouseMove first second
    in 
    Sub.batch [Browser.Events.onMouseMove decoder]

main : Program () Update.Model Update.Msg
main =
    Browser.element
        { init = always ( Update.init, Cmd.none )
        , update = \message model -> Update.update message model
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

