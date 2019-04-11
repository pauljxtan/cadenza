module Note exposing
    ( Accidental(..)
    , Note
    , Pitch(..)
    , enharmonics
    , raiseNoteBySemitones
    , raisePitch
    , strToNote
    , noteToStr
    )

import List
import String


type alias Note =
    { pitch : Pitch
    , accidental : Accidental
    }


type Pitch
    = A
    | B
    | C
    | D
    | E
    | F
    | G


type Accidental
    = DoubleFlat
    | Flat
    | Natural
    | Sharp
    | DoubleSharp


enharmonics : Note -> List Note
enharmonics note =
    let
        candidatePitches =
            [ lowerPitch 2 note.pitch
            , lowerPitch 1 note.pitch
            , note.pitch
            , raisePitch 1 note.pitch
            , raisePitch 2 note.pitch
            ]

        candidateNotes =
            candidatePitches
                |> List.concatMap
                    (\p ->
                        List.map (\a -> { pitch = p, accidental = a })
                            [ DoubleFlat, Flat, Natural, Sharp, DoubleSharp ]
                    )

        noteInt =
            noteToInt note
    in
    candidateNotes
        |> List.filter
            (\n ->
                List.member (noteToInt n) [ noteInt - 12, noteInt, noteInt + 12 ] && n /= note
            )



-- Note operations


raiseNoteBySemitones : Int -> Note -> Note
raiseNoteBySemitones n note =
    case n of
        0 ->
            note

        _ ->
            raiseNoteBySemitones (n - 1) (raiseNoteBySemitone note)


raiseNoteBySemitone : Note -> Note
raiseNoteBySemitone note =
    note |> noteToInt |> (+) 1 |> intToNote



-- Pitch operations


lowerPitch : Int -> Pitch -> Pitch
lowerPitch n pitch =
    case n of
        0 ->
            pitch

        _ ->
            lowerPitch (n - 1) (lowerPitchOnce pitch)


raisePitch : Int -> Pitch -> Pitch
raisePitch n pitch =
    case n of
        0 ->
            pitch

        _ ->
            raisePitch (n - 1) (raisePitchOnce pitch)


lowerPitchOnce : Pitch -> Pitch
lowerPitchOnce pitch =
    case pitch of
        A ->
            G

        B ->
            A

        C ->
            B

        D ->
            C

        E ->
            D

        F ->
            E

        G ->
            F


raisePitchOnce : Pitch -> Pitch
raisePitchOnce pitch =
    case pitch of
        A ->
            B

        B ->
            C

        C ->
            D

        D ->
            E

        E ->
            F

        F ->
            G

        G ->
            A



-- String conversion


strToNote : String -> Maybe Note
strToNote str =
    let
        pitch =
            str |> String.left 1 |> String.toUpper |> strToPitch

        accidental =
            str |> String.dropLeft 1 |> strToAccidental
    in
    case pitch of
        Just p ->
            case accidental of
                Just a ->
                    Just { pitch = p, accidental = a }

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


strToPitch : String -> Maybe Pitch
strToPitch str =
    case str of
        "A" ->
            Just A

        "B" ->
            Just B

        "C" ->
            Just C

        "D" ->
            Just D

        "E" ->
            Just E

        "F" ->
            Just F

        "G" ->
            Just G

        _ ->
            Nothing


strToAccidental : String -> Maybe Accidental
strToAccidental str =
    case str of
        "bb" ->
            Just DoubleFlat

        "b" ->
            Just Flat

        "" ->
            Just Natural

        "#" ->
            Just Sharp

        "##" ->
            Just DoubleSharp

        "x" ->
            Just DoubleSharp

        _ ->
            Nothing


noteToStr : Note -> String
noteToStr { pitch, accidental } =
    pitchToStr pitch ++ accidentalToStr accidental


pitchToStr : Pitch -> String
pitchToStr pitch =
    case pitch of
        A ->
            "A"

        B ->
            "B"

        C ->
            "C"

        D ->
            "D"

        E ->
            "E"

        F ->
            "F"

        G ->
            "G"


accidentalToStr : Accidental -> String
accidentalToStr accidental =
    case accidental of
        DoubleFlat ->
            "bb"

        Flat ->
            "b"

        Natural ->
            ""

        Sharp ->
            "#"

        DoubleSharp ->
            "x"



-- Integer conversion


intToNote : Int -> Note
intToNote int =
    case int of
        0 ->
            { pitch = A, accidental = Natural }

        1 ->
            { pitch = B, accidental = Flat }

        2 ->
            { pitch = B, accidental = Natural }

        3 ->
            { pitch = C, accidental = Natural }

        4 ->
            { pitch = D, accidental = Flat }

        5 ->
            { pitch = D, accidental = Natural }

        6 ->
            { pitch = E, accidental = Flat }

        7 ->
            { pitch = E, accidental = Natural }

        8 ->
            { pitch = F, accidental = Natural }

        9 ->
            { pitch = G, accidental = Flat }

        10 ->
            { pitch = G, accidental = Natural }

        11 ->
            { pitch = A, accidental = Flat }

        _ ->
            if int < 0 then
                intToNote (int + 12)

            else
                intToNote (int - 12)


noteToInt : Note -> Int
noteToInt { pitch, accidental } =
    case accidental of
        DoubleFlat ->
            pitchToInt pitch - 2

        Flat ->
            pitchToInt pitch - 1

        Natural ->
            pitchToInt pitch

        Sharp ->
            pitchToInt pitch + 1

        DoubleSharp ->
            pitchToInt pitch + 2


pitchToInt : Pitch -> Int
pitchToInt pitch =
    case pitch of
        A ->
            0

        B ->
            2

        C ->
            3

        D ->
            5

        E ->
            7

        F ->
            8

        G ->
            10
