module View exposing (view)

import Dict
import Html exposing (Html, a, div, footer, h1, input, label, option, p, section, select, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (align, class, colspan, href, id, maxlength, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import List
import Model exposing (Model, Msg(..), chordIntervals, chordNotesEmpty)


{-| The entire view.
-}
view : Model -> Html Msg
view model =
    div []
        [ titleSection
        , section [ class "section" ]
            [ div [ class "content" ]
                [ userInputContainer model
                , staffDiv
                , chordTable model
                , theFooter
                ]
            ]
        ]


{-| The title element.
-}
titleSection : Html Msg
titleSection =
    section [ class "hero is-small is-bold" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ] [ text "Chord calculator" ]
                ]
            ]
        ]


{-| A wrapper around the user input components.
-}
userInputContainer : Model -> Html Msg
userInputContainer model =
    div [ id "input-container", class "columns" ]
        [ div [ class "column" ] [ tonicField model ]
        , div [ class "column" ] [ inversionField model ]
        ]


{-| A text input for specifying a chord tonic.
-}
tonicField : Model -> Html Msg
tonicField model =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text "Tonic" ] ]
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


{-| A dropdown for selecting a chord inversion.
-}
inversionField : Model -> Html Msg
inversionField model =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text "Inversion" ] ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ div [ class "select" ]
                    [ select [ class "", onInput ChangeInversion ] <|
                        List.map (\i -> option [ value i ] [ text i ]) [ "0", "1", "2", "3" ]
                    ]
                ]
            ]
        ]


{-| An empty staff to be populated on the JS side.
-}
staffDiv : Html Msg
staffDiv =
    div [ id "staff", class "has-text-centered" ] []


{-| The table containing all chord information.
-}
chordTable : Model -> Html Msg
chordTable model =
    let
        header =
            tr []
                [ th [ class "has-text-centered" ] [ text "Chord" ]
                , th [ colspan 3, class "has-text-centered" ] [ text "Intervals (root position)" ]
                , th [ colspan 4, class "has-text-centered" ] [ text "Notes" ]
                ]

        rows =
            Model.chordTypes |> List.map (chordRow model)
    in
    table [ id "chord-table", class "table is-bordered is-hoverable" ]
        [ thead [] [ header ], tbody [] rows ]


{-| A single row in the table for a given chord.
-}
chordRow : Model -> String -> Html Msg
chordRow model chordType =
    let
        intervals =
            chordIntervals chordType |> List.map intervalCell

        notes =
            model.chords
                |> Dict.get chordType
                |> Maybe.withDefault chordNotesEmpty
                |> List.map unicodeAccidentals
                |> List.map (\s -> td [ class "chord-note-cell has-text-centered" ] [ text s ])
    in
    tr [] ([ td [ class "chord-name-cell has-text-centered" ] [ text chordType ] ] ++ intervals ++ notes)


{-| A single coloured cell in the intervals section of the table.
-}
intervalCell : String -> Html Msg
intervalCell interval =
    let
        colourClass =
            case interval |> String.left 1 of
                "d" ->
                    "interval-dim"

                "m" ->
                    "interval-min"

                "M" ->
                    "interval-maj"

                "P" ->
                    "interval-per"

                "A" ->
                    "interval-aug"

                _ ->
                    ""
    in
    td [ class <| "chord-interval-cell has-text-centered " ++ colourClass ] [ text interval ]


{-| Replaces accidentals wtih nicer-looking unicode equivalents.
-}
unicodeAccidentals : String -> String
unicodeAccidentals str =
    str
        |> String.replace "bb" " 𝄫"
        |> String.replace "b" " ♭"
        |> String.replace "##" " 𝄪"
        |> String.replace "#" " ♯"


theFooter : Html Msg
theFooter =
    let
        line =
            p []
                [ text "Built with "
                , a [ href "https://elm-lang.org/" ] [ text "Elm" ]
                , text " and "
                , a [ href "http://www.vexflow.com/" ] [ text "VexFlow" ]
                , text "."
                ]
    in
    footer [ class "footer" ] [ div [ class "content has-text-centered" ] [ line ] ]
