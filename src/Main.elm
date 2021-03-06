module Main exposing (main)

import Browser
import Browser.Events
import Element as E
import Element.Input as Input
import Element.Background as Background
import Element.Font as Font
import Element.Border as Border
import Element.Events as Events
import Html exposing (Html)
import Html.Attributes
import Random
import Time
import Delay
import Json.Decode as Decode
import FeatherIcons

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

type Page
    = HomePage
    | TaskPage Int


type alias Model =
    { letter :  Char
    , paused : Bool
    , index : Int
    , history : List Char
    , corrects : Int
    , totalCorrects : Int
    , n : Int
    , bg : E.Color
    , page : Page
    , pageBg : E.Color
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
    , bg = theme.light
    , page = HomePage
    , pageBg = theme.black
    }
    , Delay.after 500 Delay.Millisecond Pause
    )



-- UPDATE


type Msg
    = NextLetter Time.Posix
    | NewLetter Char
    | Pause
    | ConfirmTarget
    | ChangePage Page
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
                , bg = theme.light
                }
            , Delay.after 500 Delay.Millisecond Pause
            )

        ConfirmTarget ->
            (updateCorrects model
            , Cmd.none
            )

        ChangePage page ->
            changePage page

        IgnoreMessage ->
            ( model, Cmd.none )


changePage : Page -> (Model, Cmd Msg)
changePage newPage =
    let
        (model, cmd) =
            init ()
    in
    ({ model |
        page = newPage
        , pageBg =
            case newPage of
                TaskPage n ->
                    case n of
                        1 ->
                            theme.darkPurple
                        2 ->
                            theme.darkerPurple
                        _ ->
                            theme.darkestPurple
                HomePage ->
                    theme.black
        , n =
            case newPage of
                TaskPage n ->
                    n
                HomePage ->
                    model.n
    }
    , cmd
    )


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
subscriptions model =
    if model.page == HomePage then
        Sub.none
    else
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
    , grey = E.rgb255 161 161 161
    , dark = E.rgb255 20 69 125
    , darkPurple = E.rgb255 50 58 106
    , darkerPurple = E.rgb255 36 42 79
    , darkestPurple = E.rgb255 29 34 64
    , green = E.rgb255 124 252 0
    , red = E.rgb255 255 72 0
    , white = E.rgb255 255 255 255
    , black = E.rgb255 74 74 74
    , text = E.rgb255 255 255 255
    }

view : Model -> Html Msg
view model =
    case model.page of
        HomePage ->
            homeView model
        TaskPage n ->
            taskView <| { model | n = n }


homeView : Model -> Html Msg
homeView model =
    E.layout
        [ Background.color model.pageBg
        , Font.color theme.text
        , E.padding 10
        ] <|
        E.column
            [ E.spacing 30
            , E.centerX
            , E.width E.fill
            , E.height E.fill
            ]
            [ E.el
                [ Font.bold
                , Font.size 50
                , E.centerX
                , E.centerY
                ] <|
                E.text "N-Back Task"
            , E.paragraph
                [ E.centerX
                , E.centerY
                , Font.center
                , E.htmlAttribute (Html.Attributes.style "white-space" "pre-line")
                ]
                [ E.html <| Html.text "Your task is to remember if the current letter matches n letters before.\n"
                , E.html <| Html.text "Press the \"Match\" button or the spacebar if you think it's a match."
                ]
            , E.row
                [ E.centerX
                , E.centerY
                , E.spacing 20
                , E.paddingXY 0 30
                ]
                [ button
                    theme.darkPurple
                    [ E.above <| E.el [ E.centerX, E.paddingXY 0 15, Font.color theme.text ] <| E.text "Easy"
                    ]
                    { onPress = Just (ChangePage <| TaskPage 1)
                    , label =
                        E.text "1-Back"
                    }
                , button
                    theme.darkerPurple
                    [ E.above <| E.el [ E.centerX, E.paddingXY 0 15, Font.color theme.text ] <| E.text "Medium"
                    ]
                    { onPress = Just (ChangePage <| TaskPage 2)
                    , label =
                        E.text "2-Back"
                    }
                , button
                    theme.darkestPurple
                    [ E.above <| E.el [ E.centerX, E.paddingXY 0 15, Font.color theme.text ] <| E.text "Hard"
                    ]
                    { onPress = Just (ChangePage <| TaskPage 3)
                    , label =
                        E.text "3-Back"
                    }
                ]
            ]


taskView : Model -> Html Msg
taskView model =
    E.layout
        [ Background.color model.pageBg
        , Font.color theme.text
        , E.padding 20
        ] <|
        E.column
            [ E.spacing 20
            , E.centerX
            , E.centerY
            ]
            [ E.el
                [ Font.size 100
                , Font.bold
                , E.centerX
                , E.centerY
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
            button
                (if disabled then
                    theme.grey
                else
                    model.bg
                )
                [ E.centerY
                , E.centerX
                , E.mouseOver
                    [ Background.color
                        ( if disabled then
                            theme.grey
                        else
                            model.bg
                        )
                    , Font.color
                        theme.text
                    ]
                ]
                { label = E.text "Match"
                , onPress =
                    if disabled then
                        Nothing
                    else
                        Just ConfirmTarget
                }
            , E.el [ E.centerX ] <|
                E.text <| String.fromInt model.n ++ "-back"
            , E.row
                [ E.centerX
                , E.spacing 10
                ]
                [ E.el
                    [ Events.onClick <| ChangePage HomePage
                    , E.pointer
                    , E.width <| E.px 40
                    ] <|
                    E.html (FeatherIcons.home |> FeatherIcons.toHtml [])
                , E.el [ E.width <| E.px 40  ] <| E.text <| "#" ++ String.fromInt model.index
                , E.el [ E.width <| E.px 40  ] <| E.text <| "✔" ++ (String.fromInt <| round (toFloat model.corrects / toFloat model.totalCorrects * 100)) ++ "%"
                ]
            ]


button :
    E.Color ->
    List (E.Attribute msg) ->
        { onPress : Maybe msg
        , label : E.Element msg
        } ->
    E.Element msg
button bg attributes properties =
    let
        lightShadowColor =
            "rgba(255, 255, 255, 0.3)"
        lightShadow =
            "-4px -4px 8px " ++ lightShadowColor
        darkShadowColor =
            "rgba(51, 51, 51, 0.5)"
        darkShadow =
            "4px 4px 8px 6px " ++ darkShadowColor
    in
    Input.button
        ( [ E.padding 10
            , Border.rounded 10
            , Background.color bg
            , E.mouseOver
                [ Background.color theme.white
                , Font.color theme.black
                ]
            , E.htmlAttribute <| Html.Attributes.style "box-shadow" <| lightShadow ++ ", " ++ darkShadow
            ]
        ++ attributes
        )
        properties
