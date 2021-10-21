module App exposing (..)

-- import Frame3d
-- import Point3d
-- import Polygon2d
-- import Quantity

import Angle exposing (Angle)
import Browser
import Path
import Color
import Circle2d exposing (Circle2d)
import Frame2d
import Geometry.Svg as Svg
import Html
import Html.Attributes
import Html.Events as Events
import Pixels exposing (Pixels)
import Point2d exposing (Point2d, pixels)
import Quantity exposing (Quantity)
import Rectangle2d
import Shapes exposing (Shape(..))
import Svg exposing (Svg)
import Svg.Attributes
import Update
import Axis
import Time
import Scale
import Shape
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))
import Stats
import ColumnVector
import Vector
import Real

canvas : Update.Model -> Html.Html msg
canvas model =
    let
        topLeftFrame =
            Frame2d.atPoint (Point2d.pixels 100 100)

        elements =
            Svg.g [] (List.map Shapes.fromShape model.shapes)

        -- scene = Svg.relativeTo topLeftFrame elements
        scene =
            elements
    in
    Svg.svg []
        [ scene ]


view : Update.Model -> Html.Html Update.Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.textarea
                [ Html.Attributes.value model.code
                , Html.Attributes.rows 15
                , Html.Attributes.cols 79
                , Events.onInput (\txt -> Update.UpdateCode txt)
                ]
                []
            ]
        , Html.div []
            [ Svg.svg
                [ Html.Attributes.height model.canvasHeight
                , Html.Attributes.width model.canvasWidth
                ]
                [ canvas model ]
            ]
        , Html.div []
            [ Html.text <| "Total number of shapes: " ++ String.fromInt (List.length model.shapes)
            ]
        , Html.div []
            [ viewPlot timeSeries]
        ]


main : Program () Update.Model Update.Msg
main =
    Browser.element
        { init = always ( Update.init, Cmd.none )
        , update = \message model -> ( Update.update message model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        }

w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : Scale.ContinuousScale Float
xScale =
    Scale.linear ( 0, w - 2 * padding ) ( -3, 3)


yScale : Scale.ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 0.5 )


xAxis : List a -> Svg msg
xAxis model =
    Axis.bottom [ Axis.tickCount (List.length model) ] xScale


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale


transformToLineData : ( Float, Float ) -> Maybe ( Float, Float )
transformToLineData ( x, y ) =
    Just ( Scale.convert xScale x, Scale.convert yScale y )


tranfromToAreaData : ( Float, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
tranfromToAreaData ( x, y ) =
    Just
        ( ( Scale.convert xScale x, Tuple.first (Scale.rangeExtent yScale) )
        , ( Scale.convert xScale x, Scale.convert yScale y )
        )


line : List ( Float, Float ) -> Path.Path
line model =
    List.map transformToLineData model
        |> Shape.line Shape.monotoneInXCurve


area : List ( Float , Float ) -> Path.Path
area model =
    List.map tranfromToAreaData model
        |> Shape.area Shape.monotoneInXCurve


viewPlot : List ( Float, Float ) -> Svg msg
viewPlot model =
    Svg.svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            [ Path.element (area model) [ strokeWidth 3, fill <| Paint <| Color.rgba 1 0 0 0.54 ]
            , Path.element (line model) [ stroke <| Paint <| Color.rgb 1 0 0, strokeWidth 3, fill PaintNone ]
            ]
        ]



-- From here onwards this is simply example boilerplate.
-- In a real app you would load the data from a server and parse it, perhaps in
-- a separate module.


timeSeries : List ( Float, Float )
timeSeries =
    let 
        cify x = ColumnVector.ColumnVector <| Vector.Vector <| [Real.Real x, Real.Real 0.0]
        f = \x -> (x, (Stats.gaussianPDF Stats.normal2d (cify x)))
        scale = 0.5
    in List.map (toFloat >> ((*) scale >> f )) (List.range -10 10)

