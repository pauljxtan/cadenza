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
            applyIntervalBySemitones note 1 1

        Dim2 ->
            applyIntervalBySemitones note 0 2

        Min2 ->
            applyIntervalBySemitones note 1 2

        Maj2 ->
            applyIntervalBySemitones note 2 2

        Aug2 ->
            applyIntervalBySemitones note 3 2

        Dim3 ->
            applyIntervalBySemitones note 2 3

        Min3 ->
            applyIntervalBySemitones note 3 3

        Maj3 ->
            applyIntervalBySemitones note 4 3

        Aug3 ->
            applyIntervalBySemitones note 5 3

        Dim4 ->
            applyIntervalBySemitones note 4 4

        Per4 ->
            applyIntervalBySemitones note 5 4

        Aug4 ->
            applyIntervalBySemitones note 6 4

        Dim5 ->
            applyIntervalBySemitones note 6 5

        Per5 ->
            applyIntervalBySemitones note 7 5

        Aug5 ->
            applyIntervalBySemitones note 8 5

        Dim6 ->
            applyIntervalBySemitones note 7 6

        Min6 ->
            applyIntervalBySemitones note 8 6

        Maj6 ->
            applyIntervalBySemitones note 9 6

        Aug6 ->
            applyIntervalBySemitones note 10 6

        Dim7 ->
            applyIntervalBySemitones note 9 7

        Min7 ->
            applyIntervalBySemitones note 10 7

        Maj7 ->
            applyIntervalBySemitones note 11 7

        Aug7 ->
            applyIntervalBySemitones note 12 7

        Dim8 ->
            applyIntervalBySemitones note 11 8

        Per8 ->
            note


applyIntervalBySemitones : Note -> Int -> Int -> Note
applyIntervalBySemitones note nSemitones diaNum =
    let
        raised =
            raiseNoteBySemitones nSemitones note
    in
    filterByDiaNum note ([ raised ] ++ enharmonics raised) diaNum


filterByDiaNum : Note -> List Note -> Int -> Note
filterByDiaNum note notes diaNum =
    notes
        |> List.filter (\n -> n.pitch == raisePitch (diaNum - 1) note.pitch)
        |> List.head
        -- This is hacky, but can't think of a better way for now
        |> Maybe.withDefault { pitch = A, accidental = DoubleFlat }
