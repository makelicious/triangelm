module Main exposing (..)
import Browser


import Canvas exposing (rect, circle, shapes, fill)
import Dict exposing (Dict)
import Color exposing (..)
import Json.Decode as Decode
import Time exposing (..)
import Html exposing (Html, text, div, h1, img)
import Html.Events exposing (keyCode, on)
import Html.Attributes exposing (src, tabindex, style)


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


onKeyPress : (Int -> msg) -> Html.Attribute msg
onKeyPress tagger =
    on "keydown" <| Decode.map tagger keyCode

keyTagger key =
    case key of
        65 -> Left
        68 -> Right
        83 -> Down
        87 -> Up
        _ -> NoOp

---- SUBSCRIPTIONS ----

-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--     Time.every 1000 Tick

---- MODEL ----

type alias Point2D =
    { x: Float
    , y: Float
    }

type alias Model =
    { movingItem: Point2D
    , mapSize: Point2D
    }


-- type Direction = Left | Right | Up | Down

initialPosition =
    { x = 125
    , y = 0
    }

mapSize =
    { x = 250
    , y = 500
    }


init : ( Model, Cmd Msg )
init =
    ( { movingItem = initialPosition, mapSize = mapSize}, Cmd.none )


move : Float -> Float -> Msg -> Float
move position mapDiameters direction =
    case direction of
        Left ->
            if position - 5 > 0 then
                position + 5
            else
                position
        Right ->
            if position + 5 < mapDiameters then
                position + 5
            else
                position
        Up ->
            position
        Down ->
            position
        NoOp ->
            position



---- UPDATE ----

type Msg
    = Left
    | Right
    | Up
    | Down
    | NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Left ->
            let
                movingItem = model.movingItem
                mapArea = model.mapSize

                newMovingItem = { movingItem | x = move movingItem.x mapArea.x Left }
            in
            ({ model | movingItem = { movingItem | x = (move movingItem.x movingItem.x Left)}}, Cmd.none)
        Right ->
            Debug.log "Right"
            (model, Cmd.none)
        Down ->
            Debug.log "down"
            (model, Cmd.none)
        Up ->
            Debug.log "down"
            (model, Cmd.none)
        NoOp ->
             Debug.log "noop"
            (model, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        movingItem = model.movingItem
    in
        div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "flex-end"
        , style "background-color" "#fafafa"
        ]
        [ Canvas.toHtml
            ( 250, 500 )
            [ tabindex 100
            , style "border" "10px solid rgba(0,0,0,0.1)"
            , style "background-color" "#FFFFFF"
            ,  onKeyPress keyTagger ]
            [
                shapes
                    [ fill (Color.rgb255 176 208 211) ]
                    [ rect (movingItem.x, movingItem.y) 100 50 ]
            ]
        ]
