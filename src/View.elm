module View exposing (view)

import Dict
import Html exposing (Html, a, div, footer, form, h1, input, label, option, p, section, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (align, class, colspan, for, href, id, maxlength, src, value)
import Html.Events exposing (onClick, onInput)
import List
import Model exposing (Model, Msg(..), chordIntervals, chordNotesEmpty)


{-| The entire view.
-}
view : Model -> Html Msg
view model =
    section [ class "ph3 mw7 center" ]
        [ title
        , div []
            [ userInputContainer model
            , staffDiv
            , chordTable model
            , theFooter
            ]
        ]


{-| The title element.
-}
title : Html Msg
title =
    h1 [] [ text "Chord calculator" ]


{-| A wrapper around the user input components.
-}
userInputContainer : Model -> Html Msg
userInputContainer model =
    div [ class "w-100 center" ] [ tonicField model, inversionField model ]


{-| A text input for specifying a chord tonic.
-}
tonicField : Model -> Html Msg
tonicField model =
    form [ class "fl mr3" ]
        [ div [ class "measure" ]
            [ label [ class "b db mb2" ]
                [ text "Tonic "
                , span [ class "normal gray" ] [ text "(e.g. D, F#, Ab)" ]
                ]
            , input
                [ class "db"
                , maxlength 2
                , value model.key
                , onInput ChangeKey
                ]
                []
            ]
        ]


{-| A dropdown for selecting a chord inversion.
-}
inversionField : Model -> Html Msg
inversionField model =
    form [ class "fl" ]
        [ div [ class "measure" ] [ label [ class "b db mb2" ] [ text "Inversion" ] ]
        , select [ class "db", onInput ChangeInversion ] <|
            List.map (\i -> option [ value i ] [ text i ]) [ "0", "1", "2", "3" ]
        ]


{-| An empty staff to be populated on the JS side.
-}
staffDiv : Html Msg
staffDiv =
    div [ id "staff", class "center tc" ] [ text "The staff should be rendered here; if not, try refreshing." ]


{-| The table containing all chord information.
-}
chordTable : Model -> Html Msg
chordTable model =
    let
        header =
            tr []
                [ th [ class "tc" ] [ text "Chord" ]
                , th [ colspan 3, class "tc" ] [ text "Intervals (root)" ]
                , th [ colspan 4, class "tc" ] [ text "Notes" ]
                ]

        rows =
            Model.chordTypes |> List.map (chordRow model)
    in
    table [ class "pv2 w-100 center" ]
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
                |> List.map (\s -> td [ class "chord-note-cell tc w-10" ] [ text s ])
    in
    tr [] ([ td [ class "chord-name-cell tl w-30" ] [ text chordType ] ] ++ intervals ++ notes)


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
    td [ class <| "chord-interval-cell tc w-10 " ++ colourClass ] [ text interval ]


{-| Replaces accidentals wtih nicer-looking unicode equivalents.
-}
unicodeAccidentals : String -> String
unicodeAccidentals str =
    str
        |> String.replace "bb" "𝄫"
        |> String.replace "b" "♭"
        |> String.replace "##" "𝄪"
        |> String.replace "#" "♯"


{-| Some boring footer text.
-}
theFooter : Html Msg
theFooter =
    let
        line =
            p []
                [ text "Built with "
                , a [ class "link gray dim", href "https://elm-lang.org/" ] [ text "Elm" ]
                , text " and "
                , a [ class "link gray dim", href "http://www.vexflow.com/" ] [ text "VexFlow" ]
                , text "."
                ]
    in
    footer [ class "footer" ] [ div [ class "tc" ] [ line ] ]
