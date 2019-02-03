module Main exposing (..)
import Browser
import Browser.Events exposing (onAnimationFrame)


import Canvas exposing (rect, circle, shapes, fill, Shape)
import Dict exposing (Dict)
import Color exposing (..)
import Json.Decode as Decode
import Time exposing (..)
import Html exposing (Html, text, div, h1, img)
import Html.Events exposing (keyCode, on)
import Html.Attributes exposing (src, tabindex, style)
import Html.Keyed as Keyed


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


onKeyPress : (Int -> msg) -> Html.Attribute msg
onKeyPress tagger =
    on "keydown" <| Decode.map tagger keyCode

keyTagger key =
    case key of
        65 -> Move Left
        68 -> Move Right
        83 -> Move Down
        87 -> Move Up
        _ -> NoOp

---- SUBSCRIPTIONS ----

subscriptions : Model -> Sub Msg
subscriptions model = onAnimationFrame Tick

---- MODEL ----

type alias Point2D =
    { x: Float
    , y: Float
    }

type alias Model =
        { movable: Point2D
        , mapSize: Point2D
        }


type Direction = Left | Right | Up | Down

-- Position from top-left
initialPosition =
    { x = 80
    , y = 0
    }

mapSize =
    { x = 250
    , y = 500
    }


init : ( Model, Cmd Msg )
init =
    ({ movable = initialPosition, mapSize = mapSize}, Cmd.none )

moveLeft : Point2D -> Point2D
moveLeft position =
    if position.x - 5 < 0 then
        { x = 0
        , y =
            position.x
                |> round
                |> remainderBy 5
                |> toFloat

        }
    else
        { x = position.x - 5
        , y = position.y
        }










-- move : Float -> Float -> Msg -> Float
-- move position mapDiameters direction =
--     case direction of
--         Left ->
--             if position - 5 > 0 then
--                 position + 5
--             else
--                 position
--         Right ->
--             if position + 5 < mapDiameters then
--                 position + 5
--             else
--                 position
--         Up ->
--             position
--         Down ->
--             position
--         NoOp ->
--             position



---- UPDATE ----

setPosition : Point2D -> Direction -> Point2D -> Point2D
setPosition mapArea direction position =
    case direction of
        Left ->
            moveLeft position
        Right ->
            position
        Down ->
            position
        Up ->
            position


ascendItem : Point2D -> Point2D -> Point2D
ascendItem position mapArea =
    if position.y + 1 < mapArea.y
        then
            { y = position.y + 1
            , x = position.x
            }
    else
        { y = 0
        , x = position.x
        }




type Msg
    = Move Direction
    | NoOp
    | Tick Posix

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move direction ->
            ({model | movable = setPosition model.mapSize direction model.movable}, Cmd.none)
        NoOp ->
            (model, Cmd.none)
        Tick posix ->
            ({model | movable = ascendItem model.movable model.mapSize}, Cmd.none)




---- VIEW ----


view : Model -> Html Msg
view model =
    let
        movable = model.movable
    in
        div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "flex-end"
        , style "background-color" "#fafafa"
        ]
        [ Canvas.toHtml
            ( mapSize.x, mapSize.y )
            [ tabindex 100
            , style "border" "10px solid rgba(0,0,0,0.1)"
            , style "background-color" "#FFFFFF"
            ,  onKeyPress keyTagger ]
            [
                clearScreen
                , shapes
                    [ fill (Color.rgb255 176 208 211) ]
                    [ drawPoint movable ]
            ]
        ]


clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) mapSize.x mapSize.y ]

drawPoint : Point2D -> Shape
drawPoint { x, y } =
    rect ( x, y ) 100 50
