module View exposing (view)

import Dict
import Html exposing (Html, div, h1, input, label, option, p, section, select, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (align, class, colspan, href, id, maxlength, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Model exposing (Model, Msg(..), chordInfoEmpty, chordIntervals)


view : Model -> Html Msg
view model =
    div []
        [ titleSection
        , section [ class "section" ]
            [ div [ class "content" ]
                [ inputDiv model
                , staffDiv
                , chordTable model
                ]
            ]
        ]


titleSection : Html Msg
titleSection =
    section [ class "hero is-small is-bold" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ] [ text "Chord reference" ]
                ]
            ]
        ]


inputDiv : Model -> Html Msg
inputDiv model =
    div [ class "columns" ]
        [ div [ class "column" ] [ tonicField model ]
        , div [ class "column" ] [ inversionField model ]
        ]


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


inversionField : Model -> Html Msg
inversionField model =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ] [ label [ class "label" ] [ text "Inversion" ] ]
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


staffDiv : Html Msg
staffDiv =
    -- Populated on JS side
    div [ id "staff", class "has-text-centered" ] []


chordTable : Model -> Html Msg
chordTable model =
    table [ id "chord-table", class "table is-bordered is-hoverable" ]
        [ thead [] [ chordTableHeader ]
        , tbody []
            (Model.chordNames |> List.map (chordRow model))
        ]


chordTableHeader : Html Msg
chordTableHeader =
    tr []
        [ th [ class "has-text-centered" ] [ text "Chord" ]
        , th [ colspan 3, class "has-text-centered" ] [ text "Intervals (root position)" ]
        , th [ colspan 4, class "has-text-centered" ] [ text "Notes" ]
        ]


chordRow : Model -> String -> Html Msg
chordRow model chordName =
    let
        intervals =
            chordIntervals chordName |> List.map intervalCell

        notes =
            model.chords
                |> Dict.get chordName
                |> Maybe.withDefault (chordInfoEmpty chordName)
                |> List.map unicodeAccidentals
                |> List.map (\s -> td [ class "chord-note-cell" ] [ text s ])
    in
    tr [] ([ td [ class "chord-name-cell" ] [ text chordName ] ] ++ intervals ++ notes)


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
    td [ class ("chord-interval-cell " ++ colourClass) ] [ text interval ]


unicodeAccidentals : String -> String
unicodeAccidentals str =
    str
        |> String.replace "bb" " 𝄫"
        |> String.replace "b" " ♭"
        |> String.replace "##" " 𝄪"
        |> String.replace "#" " ♯"
