module App exposing (..)

-- import Frame3d
-- import Point3d
-- import Polygon2d
-- import Quantity

import Angle exposing (Angle)
import Browser
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
        ]


main : Program () Update.Model Update.Msg
main =
    Browser.element
        { init = always ( Update.init, Cmd.none )
        , update = \message model -> ( Update.update message model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        }
