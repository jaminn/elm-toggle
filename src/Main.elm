module Main exposing (..)

import Browser
import Css exposing (..)
import Css.Transitions as Transition exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Task
import Time
import UIElement exposing (toggle)



-- MAIN


main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { toggleList : List Bool }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ True, False, True ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | ToggleClicked Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleClicked id ->
            ( { model
                | toggleList =
                    List.indexedMap
                        (\i t ->
                            if i == id then
                                not t

                            else
                                t
                        )
                        model.toggleList
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


toggleListView toggleList =
    case List.indexedMap Tuple.pair toggleList of
        [] ->
            div [] []

        ( _, hd ) :: tl ->
            div []
                (List.concat
                    [ [ div [ onClick (ToggleClicked 0) ] [ toggle hd ] ]
                    , List.map
                        (\( i, t ) ->
                            div
                                [ onClick (ToggleClicked i), css [ marginTop (px 10) ] ]
                                [ toggle t ]
                        )
                        tl
                    ]
                )


view : Model -> Html Msg
view model =
    div
        [ css
            [ backgroundColor (hex "#ddd")
            , padding (px 10)
            , margin (px 20)
            , borderRadius (px 10)
            , boxShadow4 (px 1) (px 1) (px 5) (hex "#999")
            ]
        ]
        [ toggleListView model.toggleList
        ]
