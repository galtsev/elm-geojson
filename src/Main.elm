module Main exposing (Model, Msg(..), emptyModel, main, update, view)

import Browser
import Html exposing (Html, div, text)


type Msg
    = Nope


type alias Model =
    { name : String
    }


emptyModel : Model
emptyModel =
    { name = "Hello" }


view : Model -> Html Msg
view model =
    text "hello"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


init : () -> ( Model, Cmd Msg )
init =
    always ( emptyModel, Cmd.none )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
