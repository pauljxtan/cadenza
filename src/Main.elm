module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Chord
    exposing
        ( augmentedMajorSeventh
        , augmentedSeventh
        , diminishedSeventh
        , dominantSeventh
        , halfDiminishedSeventh
        , majorSeventh
        , minorMajorSeventh
        , minorSeventh
        )
import Dict exposing (Dict)
import Html
    exposing
        ( Html
        , a
        , div
        , footer
        , h1
        , h2
        , input
        , label
        , option
        , p
        , section
        , select
        , table
        , td
        , text
        , th
        , tr
        )
import Html.Attributes exposing (align, class, href, maxlength, placeholder, src, value)
import Html.Events exposing (onInput)
import Note exposing (Note, noteToStr, strToNote)



---- MODEL ----


type alias Model =
    { key : String
    , chords : Dict String (List String)
    , inversion : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { key = ""
      , chords = chordsEmpty
      , inversion = 0
      }
    , Cmd.none
    )


chordsEmpty : Dict String (List String)
chordsEmpty =
    Dict.fromList
        [ ( "Diminished", [ "", "", "", "" ] )
        , ( "Half-diminished", [ "", "", "", "" ] )
        , ( "Minor", [ "", "", "", "" ] )
        , ( "Minor-major", [ "", "", "", "" ] )
        , ( "Dominant", [ "", "", "", "" ] )
        , ( "Major", [ "", "", "", "" ] )
        , ( "Augmented", [ "", "", "", "" ] )
        , ( "Augmented-major", [ "", "", "", "" ] )
        ]



---- UPDATE ----


type Msg
    = ChangeKey String
    | ChangeInversion String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeKey newKey ->
            ( model |> updateKey newKey, Cmd.none )

        ChangeInversion newInversion ->
            ( model |> updateInversion newInversion, Cmd.none )


updateKey : String -> Model -> Model
updateKey newKey model =
    case strToNote newKey of
        Just note ->
            { model
                | key = newKey
                , chords = newChords note model.inversion
            }

        Nothing ->
            { model | key = newKey, chords = chordsEmpty }


newChords : Note -> Int -> Dict String (List String)
newChords note inversion =
    Dict.fromList
        [ ( "Diminished", diminishedSeventh inversion note |> List.map noteToStr )
        , ( "Half-diminished", halfDiminishedSeventh inversion note |> List.map noteToStr )
        , ( "Minor", minorSeventh inversion note |> List.map noteToStr )
        , ( "Minor-major", minorMajorSeventh inversion note |> List.map noteToStr )
        , ( "Dominant", dominantSeventh inversion note |> List.map noteToStr )
        , ( "Major", majorSeventh inversion note |> List.map noteToStr )
        , ( "Augmented", augmentedSeventh inversion note |> List.map noteToStr )
        , ( "Augmented-major", augmentedMajorSeventh inversion note |> List.map noteToStr )
        ]


updateInversion : String -> Model -> Model
updateInversion newInversion model =
    case String.toInt newInversion of
        Just inv ->
            case strToNote model.key of
                Just key ->
                    { model | inversion = inv, chords = newChords key inv }

                Nothing ->
                    model

        Nothing ->
            model



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ section [ class "hero is-small is-bold" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ h1 [ class "title" ] [ text "Cadenza" ]
                    , h2 [ class "subtitle" ] [ text "Chord analysis tool" ]
                    ]
                ]
            ]
        , section [ class "section" ]
            [ div [ class "content" ]
                [ div [ class "columns" ]
                    [ div [ class "column" ]
                        [ div [ class "field is-horizontal" ]
                            [ div [ class "field-label is-normal" ]
                                [ label [ class "label" ] [ text "Tonic" ] ]
                            , div [ class "field-body" ]
                                [ div [ class "field" ]
                                    [ p [ class "control" ]
                                        [ input
                                            [ class "input"
                                            , placeholder "Enter a note, e.g. F#"
                                            , maxlength 2
                                            , value model.key
                                            , onInput ChangeKey
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "column" ]
                        [ div [ class "field is-horizontal" ]
                            [ div [ class "field-label is-normal" ]
                                [ label [ class "label" ] [ text "Inversion" ]
                                ]
                            , div [ class "field-body" ]
                                [ div [ class "field" ]
                                    [ div [ class "select" ]
                                        [ select [ class "", onInput ChangeInversion ]
                                            [ option [ value "0" ] [ text "0" ]
                                            , option [ value "1" ] [ text "1" ]
                                            , option [ value "2" ] [ text "2" ]
                                            , option [ value "3" ] [ text "3" ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , div [ class "" ]
                    [ table [ class "table" ]
                        ([ tr []
                            ([ "Chord", "Root", "3rd", "5th", "7th" ]
                                |> List.map (\s -> th [] [ text s ])
                            )
                         ]
                            ++ (model.chords |> Dict.keys |> List.map (seventhChordRow model))
                        )
                    ]
                ]
            ]
        , footer [ class "footer" ]
            [ div [ class "content has-text-centered" ]
                [ p [] [ text "Built with Elm by ", a [ href "https://paultan.ca" ] [ text "Paul Tan" ] ]
                ]
            ]
        ]


seventhChordRow : Model -> String -> Html msg
seventhChordRow model key =
    tr []
        ([ td [] [ text key ] ]
            ++ (model.chords
                    |> Dict.get key
                    |> Maybe.withDefault [ "", "", "", "" ]
                    |> List.map unicodeAccidentals
                    |> List.map (\s -> td [] [ text s ])
               )
        )


unicodeAccidentals : String -> String
unicodeAccidentals str =
    str
        |> String.replace "bb" "𝄫"
        |> String.replace "b" "♭"
        |> String.replace "#" "♯"
        |> String.replace "x" "𝄪"



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
