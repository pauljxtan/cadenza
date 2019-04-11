module Tests exposing (all)

import Chord
    exposing
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
import Expect
import Interval exposing (Interval(..), Quality(..), applyInterval, strToInterval)
import List
import Note
    exposing
        ( Accidental(..)
        , Note
        , Pitch(..)
        , enharmonics
        , raiseNoteBySemitones
        , strToNote
        )
import Test exposing (..)


all : Test
all =
    describe "The Cadenza library" [ noteTests, intervalTests, chordTests ]


noteTests : Test
noteTests =
    describe "The Note module"
        [ test "Converts string to note" <|
            \_ ->
                Expect.equal ([ "Abb", "Bb", "C", "D#", "Ex" ] |> List.map strToNote)
                    [ Just { pitch = A, accidental = DoubleFlat }
                    , Just { pitch = B, accidental = Flat }
                    , Just { pitch = C, accidental = Natural }
                    , Just { pitch = D, accidental = Sharp }
                    , Just { pitch = E, accidental = DoubleSharp }
                    ]
        , test "Gets all enharmonics of note" <|
            \_ ->
                Expect.equal (c "Abb Bb C D# Ex Fbb Gb" |> List.map enharmonics)
                    [ c "Fx G"
                    , c "A# Cbb"
                    , c "B# Dbb"
                    , c "Eb Fbb"
                    , c "F# Gb"
                    , c "D# Eb"
                    , c "Ex F#"
                    ]
        , test "Raises note by semitones" <|
            \_ ->
                Expect.equal
                    [ raiseNoteBySemitones 1 (n "Abb")
                    , raiseNoteBySemitones 2 (n "Bb")
                    , raiseNoteBySemitones 4 (n "C")
                    , raiseNoteBySemitones 8 (n "D#")
                    , raiseNoteBySemitones 16 (n "Ex")
                    ]
                    (c "Ab C E B Bb")
        ]


intervalTests : Test
intervalTests =
    describe "The Interval module"
        [ test "Unisons" <|
            \_ ->
                Expect.equal
                    [ applyInterval (n "Abb") Per1
                    , applyInterval (n "Bb") Aug1
                    ]
                    (c "Abb B")
        , test "Seconds" <|
            \_ ->
                Expect.equal
                    [ applyInterval (n "C") Dim2
                    , applyInterval (n "C") Min2
                    , applyInterval (n "C#") Maj2
                    , applyInterval (n "C#") Aug2
                    ]
                    (c "Dbb Db D# Dx")
        , test "Thirds" <|
            \_ ->
                Expect.equal
                    [ applyInterval (n "Db") Dim3
                    , applyInterval (n "Db") Min3
                    , applyInterval (n "D#") Maj3
                    , applyInterval (n "D") Aug3
                    ]
                    (c "Fbb Fb Fx Fx")
        , test "Fourths" <|
            \_ ->
                Expect.equal
                    [ applyInterval (n "Eb") Dim4
                    , applyInterval (n "E#") Per4
                    , applyInterval (n "E#") Aug4
                    ]
                    (c "Abb A# Ax")
        , test "Fifths" <|
            \_ ->
                Expect.equal
                    [ applyInterval (n "Fb") Dim5
                    , applyInterval (n "Fb") Per5
                    , applyInterval (n "F#") Aug5
                    ]
                    (c "Cbb Cb Cx")
        , test "Sixths" <|
            \_ ->
                Expect.equal
                    [ applyInterval (n "G") Dim6
                    , applyInterval (n "Gb") Min6
                    , applyInterval (n "G#") Maj6
                    , applyInterval (n "G#") Aug6
                    ]
                    (c "Ebb Ebb E# Ex")
        , test "Sevenths" <|
            \_ ->
                Expect.equal
                    [ applyInterval (n "Ab") Dim7
                    , applyInterval (n "Ab") Min7
                    , applyInterval (n "A#") Maj7
                    , applyInterval (n "A") Aug7
                    ]
                    (c "Gbb Gb Gx Gx")
        , test "Octaves" <|
            \_ ->
                Expect.equal
                    [ applyInterval (n "Bb") Dim8
                    , applyInterval (n "Bb") Per8
                    ]
                    (c "Bbb Bb")
        ]


chordTests : Test
chordTests =
    describe "The Chord module"
        [ test "Triads" <|
            \_ ->
                Expect.equal
                    [ diminishedTriad 0 (n "C")
                    , minorTriad 0 (n "C")
                    , majorTriad 0 (n "C")
                    , augmentedTriad 0 (n "C")
                    ]
                    [ c "C Eb Gb"
                    , c "C Eb G"
                    , c "C E G"
                    , c "C E G#"
                    ]
        , test "Sevenths" <|
            \_ ->
                Expect.equal
                    [ diminishedSeventh 0 (n "C")
                    , halfDiminishedSeventh 0 (n "C")
                    , minorSeventh 0 (n "C")
                    , minorMajorSeventh 0 (n "C")
                    , dominantSeventh 0 (n "C")
                    , majorSeventh 0 (n "C")
                    , augmentedSeventh 0 (n "C")
                    , augmentedMajorSeventh 0 (n "C")
                    ]
                    [ c "C Eb Gb Bbb"
                    , c "C Eb Gb Bb"
                    , c "C Eb G Bb"
                    , c "C Eb G B"
                    , c "C E G Bb"
                    , c "C E G B"
                    , c "C E G# Bb"
                    , c "C E G# B"
                    ]
        , test "Inversions" <|
            \_ ->
                Expect.equal
                    [ invert 0 (c "C E G B")
                    , invert 1 (c "C E G B")
                    , invert 2 (c "C E G B")
                    , invert 3 (c "C E G B")
                    ]
                    [ c "C E G B"
                    , c "E G B C"
                    , c "G B C E"
                    , c "B C E G"
                    ]
        ]



-- Convenience functions to reduce verbosity


n : String -> Note
n str =
    strToNote str |> Maybe.withDefault { pitch = A, accidental = DoubleFlat }


c : String -> List Note
c str =
    str |> String.split " " |> List.map n
