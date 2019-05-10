module Chord exposing
    ( augmentedMajorSeventh
    , augmentedSeventh
    , augmentedTriad
    , diminishedSeventh
    , diminishedTriad
    , dominantSeventh
    , halfDiminishedSeventh
    , invert
    , majorSeventh
    , majorTriad
    , minorMajorSeventh
    , minorSeventh
    , minorTriad
    )

import Interval exposing (Interval(..), Quality(..), applyInterval)
import List
import Note exposing (Note, noteToStr, strToNote)



-- Triads


diminishedTriad : Int -> Note -> List Note
diminishedTriad inversion root =
    [ root, applyInterval root Min3, applyInterval root Dim5 ] |> invert inversion


minorTriad : Int -> Note -> List Note
minorTriad inversion root =
    [ root, applyInterval root Min3, applyInterval root Per5 ] |> invert inversion


majorTriad : Int -> Note -> List Note
majorTriad inversion root =
    [ root, applyInterval root Maj3, applyInterval root Per5 ] |> invert inversion


augmentedTriad : Int -> Note -> List Note
augmentedTriad inversion root =
    [ root, applyInterval root Maj3, applyInterval root Aug5 ] |> invert inversion



-- Seventh chords


diminishedSeventh : Int -> Note -> List Note
diminishedSeventh inversion root =
    diminishedTriad 0 root ++ [ applyInterval root Dim7 ] |> invert inversion


halfDiminishedSeventh : Int -> Note -> List Note
halfDiminishedSeventh inversion root =
    diminishedTriad 0 root ++ [ applyInterval root Min7 ] |> invert inversion


minorSeventh : Int -> Note -> List Note
minorSeventh inversion root =
    minorTriad 0 root ++ [ applyInterval root Min7 ] |> invert inversion


minorMajorSeventh : Int -> Note -> List Note
minorMajorSeventh inversion root =
    minorTriad 0 root ++ [ applyInterval root Maj7 ] |> invert inversion


dominantSeventh : Int -> Note -> List Note
dominantSeventh inversion root =
    majorTriad 0 root ++ [ applyInterval root Min7 ] |> invert inversion


majorSeventh : Int -> Note -> List Note
majorSeventh inversion root =
    majorTriad 0 root ++ [ applyInterval root Maj7 ] |> invert inversion


augmentedSeventh : Int -> Note -> List Note
augmentedSeventh inversion root =
    augmentedTriad 0 root ++ [ applyInterval root Min7 ] |> invert inversion


augmentedMajorSeventh : Int -> Note -> List Note
augmentedMajorSeventh inversion root =
    augmentedTriad 0 root ++ [ applyInterval root Maj7 ] |> invert inversion



-- Operations


invert : Int -> List Note -> List Note
invert n chord =
    case n of
        0 ->
            chord

        _ ->
            chord |> invertOnce |> invert (n - 1)


invertOnce : List Note -> List Note
invertOnce chord =
    case chord of
        h :: t ->
            t ++ [ h ]

        [] ->
            []



-- String conversion


strToChord : String -> List (Maybe Note)
strToChord str =
    str |> String.split " " |> List.map strToNote
