module Main exposing (main)

import Browser
import Browser.Events
import Element as E
import Element.Input as Input
import Element.Background as Background
import Element.Font as Font
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes
import Random
import Time
import Delay
import Json.Decode as Decode

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
    , index : Int
    , history : List Char
    , corrects : Int
    , totalCorrects : Int
    , n : Int
    , bg : E.Color
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ({ letter = 'B'
    , paused = False
    , index = 1
    , corrects = 1
    , totalCorrects = 1
    , history = [ 'B' ]
    , n = 2
    , bg = theme.darker
    }
    , Delay.after 500 Delay.Millisecond Pause
    )



-- UPDATE


type Msg
    = NextLetter Time.Posix
    | NewLetter Char
    | Pause
    | ConfirmTarget
    | IgnoreMessage


letters : List Char
letters =
    ['B', 'C', 'D', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', 'X', 'Y', 'Z']


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextLetter _ ->
            nextLetter model

        Pause ->
            ( { model |
                paused = True
                }
            , Cmd.none
            )

        NewLetter newLetter ->
            let
                newIndex =
                    model.index + 1
                newHistory =
                    nextHistory model newLetter
                newTotalCorrects =
                    nextTotalCorrects model model.history
            in
            ( { model |
                letter = newLetter
                , index = newIndex
                , history = newHistory
                , totalCorrects = newTotalCorrects
                , bg = theme.darker
                }
            , Delay.after 500 Delay.Millisecond Pause
            )

        ConfirmTarget ->
            ( updateCorrects model
            , Cmd.none
            )

        IgnoreMessage ->
            ( model, Cmd.none )


nextLetter : Model -> (Model, Cmd Msg)
nextLetter model =
    let
        total =
            List.length letters
        nBackLetter =
            Maybe.withDefault 'C' <|
                List.head <|
                    Maybe.withDefault [] <|
                        List.tail model.history
        rests =
            List.filter (\letter -> letter /= nBackLetter) letters
    in
    ( { model |
        paused = False
    }
    , Random.generate NewLetter
        (Random.weighted
            (toFloat (total - 1) / 2, nBackLetter)
            (List.map (\letter -> (1, letter)) rests)
        )
    )


nextTotalCorrects : Model -> List Char -> Int
nextTotalCorrects model history =
    if isCorrect history model.n then
        model.totalCorrects + 1
    else
        model.totalCorrects


updateCorrects : Model -> Model
updateCorrects model =
    if isCorrect model.history model.n then
        { model |
            corrects = model.corrects + 1
            , bg = theme.green
        }
    else
        { model |
            corrects =
                if model.corrects > 0 && List.length model.history > model.n then
                    model.corrects - 1
                else
                    model.corrects
            , bg = theme.red
        }


isCorrect : List Char -> Int -> Bool
isCorrect history n =
    List.head history
        == (List.head <| List.reverse history)
    && List.length history == n + 1


nextHistory : Model -> Char -> List Char
nextHistory model newLetter =
    if List.length model.history <= model.n then
        model.history ++ [ newLetter ]
    else
        (Maybe.withDefault [] <| List.tail model.history) ++ [ newLetter ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 2500 NextLetter
        , Browser.Events.onKeyDown keyDecoder
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map keyToMessage <| Decode.field "key" Decode.string


keyToMessage : String -> Msg
keyToMessage string =
    case String.uncons string of
        Just ( char, "" ) ->
            case char of
                ' ' ->
                    ConfirmTarget
                _ ->
                    IgnoreMessage
        _ ->
            IgnoreMessage



-- VIEW


theme =
    { light = E.rgb255 102 121 217
    , grey = E.rgb255 196 196 196
    , dark = E.rgb255 20 69 125
    , darker = E.rgb255 50 58 106
    , green = E.rgb255 124 252 0
    , red = E.rgb255 255 72 0
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
            [ E.spacing 20
            , E.centerX
            , E.height E.fill
            ]
            [ E.el
                [ Font.size 100
                , Font.bold
                , E.centerX
                , E.centerY
                , E.width <| E.px 200
                , E.height <| E.px 200
                , Background.color model.bg
                ] <|
                    E.el
                    [ E.centerX
                    , E.centerY
                    ] <|
                    E.text (
                        if model.paused then
                            " "
                        else
                            String.fromChar model.letter
                    )
            , let
                disabled =
                    List.length model.history <= model.n
            in
            Input.button
                [ Background.color <|
                    if disabled then
                        theme.grey
                    else
                        theme.light
                , E.mouseOver
                    [ Background.color <|
                        if disabled then
                            theme.grey
                        else
                            theme.dark
                        ]
                , E.padding 10
                , Border.rounded 10
                , E.centerY
                , E.centerX
                ]
                { label = E.text "Is Target"
                , onPress =
                    if disabled then
                        Nothing
                    else
                        Just ConfirmTarget
                }
            , E.column
                [ E.centerX
                , E.alignBottom
                ]
                [ E.text <| "#" ++ String.fromInt model.index
                , E.text <| "✔" ++ (String.fromInt <| round (toFloat model.corrects / toFloat model.totalCorrects * 100)) ++ "%"
                ]
            ]
