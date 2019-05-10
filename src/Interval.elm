module Interval exposing (Interval(..), Quality(..), applyInterval, strToInterval)

import List
import Note exposing (Accidental(..), Note, Pitch(..), enharmonics, raiseNoteBySemitones, raisePitch)


type Interval
    = Per1
    | Aug1
    | Dim2
    | Min2
    | Maj2
    | Aug2
    | Dim3
    | Min3
    | Maj3
    | Aug3
    | Dim4
    | Per4
    | Aug4
    | Dim5
    | Per5
    | Aug5
    | Dim6
    | Min6
    | Maj6
    | Aug6
    | Dim7
    | Min7
    | Maj7
    | Aug7
    | Dim8
    | Per8


type Quality
    = Diminished
    | Minor
    | Major
    | Perfect
    | Augmented



-- String conversion


strToInterval : String -> Maybe Interval
strToInterval str =
    case str of
        "P1" ->
            Just Per1

        "A1" ->
            Just Aug1

        "d2" ->
            Just Dim2

        "m2" ->
            Just Min2

        "M2" ->
            Just Maj2

        "A2" ->
            Just Aug2

        "d3" ->
            Just Dim2

        "m3" ->
            Just Min2

        "M3" ->
            Just Maj2

        "A3" ->
            Just Aug2

        "d4" ->
            Just Dim4

        "P4" ->
            Just Per4

        "A4" ->
            Just Aug4

        "d5" ->
            Just Dim5

        "P5" ->
            Just Per5

        "A5" ->
            Just Aug5

        "d6" ->
            Just Dim2

        "m6" ->
            Just Min2

        "M6" ->
            Just Maj2

        "A6" ->
            Just Aug2

        "d7" ->
            Just Dim2

        "m7" ->
            Just Min2

        "M7" ->
            Just Maj2

        "A7" ->
            Just Aug2

        "d8" ->
            Just Dim8

        "P8" ->
            Just Per8

        _ ->
            Nothing



-- Operations


applyInterval : Note -> Interval -> Note
applyInterval note interval =
    case interval of
        Per1 ->
            note

        Aug1 ->
            note |> applyIntervalBySemitones 1 1

        Dim2 ->
            note |> applyIntervalBySemitones 0 2

        Min2 ->
            note |> applyIntervalBySemitones 1 2

        Maj2 ->
            note |> applyIntervalBySemitones 2 2

        Aug2 ->
            note |> applyIntervalBySemitones 3 2

        Dim3 ->
            note |> applyIntervalBySemitones 2 3

        Min3 ->
            note |> applyIntervalBySemitones 3 3

        Maj3 ->
            note |> applyIntervalBySemitones 4 3

        Aug3 ->
            note |> applyIntervalBySemitones 5 3

        Dim4 ->
            note |> applyIntervalBySemitones 4 4

        Per4 ->
            note |> applyIntervalBySemitones 5 4

        Aug4 ->
            note |> applyIntervalBySemitones 6 4

        Dim5 ->
            note |> applyIntervalBySemitones 6 5

        Per5 ->
            note |> applyIntervalBySemitones 7 5

        Aug5 ->
            note |> applyIntervalBySemitones 8 5

        Dim6 ->
            note |> applyIntervalBySemitones 7 6

        Min6 ->
            note |> applyIntervalBySemitones 8 6

        Maj6 ->
            note |> applyIntervalBySemitones 9 6

        Aug6 ->
            note |> applyIntervalBySemitones 10 6

        Dim7 ->
            note |> applyIntervalBySemitones 9 7

        Min7 ->
            note |> applyIntervalBySemitones 10 7

        Maj7 ->
            note |> applyIntervalBySemitones 11 7

        Aug7 ->
            note |> applyIntervalBySemitones 12 7

        Dim8 ->
            note |> applyIntervalBySemitones 11 8

        Per8 ->
            note


applyIntervalBySemitones : Int -> Int -> Note -> Note
applyIntervalBySemitones nSemitones diaNum note =
    let
        raised =
            note |> raiseNoteBySemitones nSemitones

        candidates =
            [ raised ] ++ enharmonics raised
    in
    candidates |> filterByDiaNum note diaNum


filterByDiaNum : Note -> Int -> List Note -> Note
filterByDiaNum note diaNum notes =
    notes
        |> List.filter (\n -> n.pitch == raisePitch (diaNum - 1) note.pitch)
        |> List.head
        -- This is hacky, but can't think of a better way for now
        |> Maybe.withDefault { pitch = A, accidental = DoubleFlat }
