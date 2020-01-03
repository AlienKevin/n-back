module Main exposing (main)

import Browser
import Element as E
import Element.Input as Input
import Element.Background as Background
import Element.Font as Font
import Element.Border as Border
import Html exposing (Html)
import Random
import Time
import Delay


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { letter :  Char
    , paused : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ({ letter = 'B'
    , paused = False
    }
    , Delay.after 500 Delay.Millisecond Pause
    )



-- UPDATE


type Msg
    = NextLetter Time.Posix
    | NewLetter Char
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextLetter _ ->
            ( { model |
                paused = False
            }
            , Random.generate NewLetter
                (Random.uniform 'B' [ 'C', 'D', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', 'X', 'Y', 'Z'])
            )

        Pause ->
            ( { model |
                paused = True
                }
            , Cmd.none
            )

        NewLetter newLetter ->
            ( { model |
                letter = newLetter
                }
            , Delay.after 500 Delay.Millisecond Pause
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 2500 NextLetter



-- VIEW


theme : { dark : E.Color, darker : E.Color, light : E.Color, text : E.Color }
theme =
    { light = E.rgb255 102 121 217
    , dark = E.rgb255 20 69 125
    , darker = E.rgb255 50 58 106
    , text = E.rgb255 255 255 255
    }

view : Model -> Html Msg
view model =
    E.layout
        [ Background.color theme.darker
        , Font.color theme.text
        , E.padding 10
        ] <|
        E.column
            [ E.spacing 10
            , E.centerX
            , E.centerY
            ]
            [ E.el
                [ Font.size 100
                , E.centerX
                ] <|
                E.text (
                    if model.paused then
                        " "
                    else
                        String.fromChar model.letter
                )
            , Input.button
                [ Background.color theme.light
                , E.mouseOver
                    [ Background.color theme.dark ]
                , E.padding 10
                , Border.rounded 10
                ]
                { label = E.text "Next Letter"
                , onPress = Nothing
                }
            ]
