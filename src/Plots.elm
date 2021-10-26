module Plots exposing (..)

import Scale
import Path
import Color
import Axis
import Axis
import Scale
import Shape
import TypedSvg.Core
import TypedSvg.Types exposing (AnchorAlignment(..), CoordinateSystem(..), Length(..), Opacity(..), Paint(..), Transform(..))
import TypedSvg exposing (circle, defs, g, linearGradient, stop)
import TypedSvg.Attributes exposing (class, fill, id, offset, opacity, stopColor, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth)
import Svg


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
    Scale.linear ( 0, w - 2 * padding ) (-10, 10)


yScale : Scale.ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) (0, 0.5)

yScaleScatter : Scale.ContinuousScale Float
yScaleScatter =
    Scale.linear ( h - 2 * padding, 0 ) (-10, 10)


xAxis : List a -> TypedSvg.Core.Svg msg
xAxis model =
    Axis.bottom [ Axis.tickCount (List.length model) ] xScale


yAxis : TypedSvg.Core.Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale

yAxisScatter : TypedSvg.Core.Svg msg
yAxisScatter =
    Axis.left [ Axis.tickCount 5 ] yScaleScatter

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


circles : List ( Float, Float ) -> List (TypedSvg.Core.Svg msg)
circles model =
    List.map pointCircle model


pointCircle : ( Float, Float ) -> TypedSvg.Core.Svg msg
pointCircle ( dataX, dataY ) =
    g [ class [ "data-point" ] ]
        [ circle
            [ cx (Scale.convert xScale dataX)
            , cy (Scale.convert yScaleScatter dataY)
            , r 3
            , fill <| Reference "linGradientRed"
            , strokeWidth 0
            , stroke <| PaintNone
            , opacity <| Opacity 0.85
            ]
            []
        ]

scatterDefs : List (TypedSvg.Core.Svg msg)
scatterDefs =
    [ linearGradient
        [ id "linGradientRed"
        , TypedSvg.Attributes.x1 <| Percent 0.0
        , TypedSvg.Attributes.y1 <| Percent 0.0
        , TypedSvg.Attributes.x2 <| Percent 0.0
        , TypedSvg.Attributes.y2 <| Percent 100.0
        ]
        [ stop [ offset "0%", stopColor "#e52d27" ] []
        , stop [ offset "100%", stopColor "#b31217" ] []
        ]
    , linearGradient
        [ id "linGradientGray"
        , TypedSvg.Attributes.x1 <| Percent 0.0
        , TypedSvg.Attributes.y1 <| Percent 0.0
        , TypedSvg.Attributes.x2 <| Percent 0.0
        , TypedSvg.Attributes.y2 <| Percent 100.0
        ]
        [ stop [ offset "0%", stopColor "#5b6467" ] []
        , stop [ offset "74%", stopColor "#8b939a" ] []
        ]
    ]

viewScatter : List ( Float, Float ) -> TypedSvg.Core.Svg msg
viewScatter model =
    Svg.svg [ viewBox 0 0 w h ]
        [ defs [] scatterDefs
        , g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxisScatter ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            (circles model)
        ]

viewPlot : List ( Float, Float ) -> TypedSvg.Core.Svg msg
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
