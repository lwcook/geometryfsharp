module Main exposing (Model, Msg(..), init, inputField, main, update, view)

import Angle exposing (Angle)
import Browser
import Frame2d
import Frame3d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import Html.Events as Events
import Point2d
import Point3d
import Polygon2d
import Quantity
import Svg exposing (Svg)
import Svg.Attributes


type alias Model =
    { radius : String }


init : Model
init =
    { radius = "0.8" }


type Msg
    = Radius String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Radius radius ->
            { model | radius = radius }



inputField : String -> String -> (String -> Msg) -> Html Msg
inputField label value msg =
    Html.div []
        [ Html.label [] [ Html.text label ]
        , Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.value value
            , Events.onInput msg
            ]
            []
        ]

safeToFloat : String -> String
safeToFloat s =
    case String.toFloat s of 
        Just val -> String.fromFloat val
        Nothing -> "0"

drawing : Model -> Html Msg
drawing model = 
    Svg.svg [ Svg.Attributes.width "500", Svg.Attributes.height "500" ]
        [ Svg.circle [ 
            Svg.Attributes.cx "100", 
            Svg.Attributes.cy "100", 
            Svg.Attributes.r <| safeToFloat model.radius, 
            Svg.Attributes.fill "white",
            Svg.Attributes.stroke "black"
            ] [] 
        ]

view : Model -> Html Msg
view model =
    Html.div []
        [ Html.form []
            [ inputField "Height:" model.radius Radius
            ]
        , drawing model
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , update = \message model -> ( update message model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        }