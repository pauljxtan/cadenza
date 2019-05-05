module Model exposing (Model, Msg(..), chordInfoEmpty, chordIntervals, chordNames, init, update)

import Chord
import Dict exposing (Dict)
import Json.Encode
import Note
import ToJs exposing (toJs)


type alias Model =
    { key : String
    , chords : Dict String (List String)
    , inversion : Int
    , message : String
    }



-- MODEL


init : ( Model, Cmd Msg )
init =
    ( { key = ""
      , chords = chordsEmpty
      , inversion = 0
      , message = ""
      }
    , Cmd.none
    )


chordNames : List String
chordNames =
    [ "Diminished"
    , "Half-diminished"
    , "Minor"
    , "Minor-major"
    , "Dominant"
    , "Major"
    , "Augmented"
    , "Augmented-major"
    ]


chordsEmpty : Dict String (List String)
chordsEmpty =
    chordNames
        |> List.map (\chordName -> ( chordName, chordInfoEmpty chordName ))
        |> Dict.fromList


chordInfoEmpty : String -> List String
chordInfoEmpty chordName =
    [ "", "", "", "" ]


chordIntervals : String -> List String
chordIntervals chordName =
    case chordName of
        "Diminished" ->
            [ "m3", "d5", "d7" ]

        "Half-diminished" ->
            [ "m3", "d5", "m7" ]

        "Minor" ->
            [ "m3", "P5", "m7" ]

        "Minor-major" ->
            [ "m3", "P5", "M7" ]

        "Dominant" ->
            [ "M3", "P5", "m7" ]

        "Major" ->
            [ "M3", "P5", "M7" ]

        "Augmented" ->
            [ "M3", "A5", "m7" ]

        "Augmented-major" ->
            [ "M3", "A5", "M7" ]

        _ ->
            [ "", "", "" ]



-- UPDATE


type Msg
    = ChangeKey String
    | ChangeInversion String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeKey newKey ->
            let
                newModel =
                    model |> updateKey newKey
            in
            ( newModel, encodeChords newModel |> toJs )

        ChangeInversion newInversion ->
            let
                newModel =
                    model |> updateInversion newInversion
            in
            ( newModel, encodeChords newModel |> toJs )


encodeChords : Model -> String
encodeChords model =
    Json.Encode.encode 0
        (chordNames
            |> List.map
                (\key ->
                    ( key
                    , model.chords
                        |> Dict.get key
                        |> Maybe.withDefault [ "", "", "", "" ]
                        |> encodeNotesAndAccidentals model
                        |> String.join ","
                        |> Json.Encode.string
                    )
                )
            |> Json.Encode.object
        )


encodeNotesAndAccidentals : Model -> List String -> List String
encodeNotesAndAccidentals model notes =
    -- Split accidental from each note
    case notes of
        [ "", "", "", "" ] ->
            [ "", "", "", "", "", "", "", "" ]

        _ ->
            let keyCapitalized = String.toUpper (String.left 1 model.key) ++ (String.dropLeft 1 model.key) 
            in
            [keyCapitalized] ++ ([ notes |> List.map (\x -> x |> String.left 1)
            , notes |> List.map (\x -> x |> String.dropLeft 1)
            ]
                |> List.concat)


updateKey : String -> Model -> Model
updateKey newKey model =
    case Note.strToNote newKey of
        Just note ->
            { model | key = newKey, chords = newChords note model.inversion }

        Nothing ->
            { model | key = newKey, chords = chordsEmpty }


updateInversion : String -> Model -> Model
updateInversion newInversion model =
    case String.toInt newInversion of
        Just inversion ->
            case Note.strToNote model.key of
                Just key ->
                    { model | inversion = inversion, chords = newChords key inversion }

                Nothing ->
                    { model | inversion = inversion }

        Nothing ->
            model


chordNotes : String -> Int -> Note.Note -> List String
chordNotes chordName inversion key =
    case chordName of
        "Diminished" ->
            Chord.diminishedSeventh inversion key |> List.map Note.noteToStr

        "Half-diminished" ->
            Chord.halfDiminishedSeventh inversion key |> List.map Note.noteToStr

        "Minor" ->
            Chord.minorSeventh inversion key |> List.map Note.noteToStr

        "Minor-major" ->
            Chord.minorMajorSeventh inversion key |> List.map Note.noteToStr

        "Dominant" ->
            Chord.dominantSeventh inversion key |> List.map Note.noteToStr

        "Major" ->
            Chord.majorSeventh inversion key |> List.map Note.noteToStr

        "Augmented" ->
            Chord.augmentedSeventh inversion key |> List.map Note.noteToStr

        "Augmented-major" ->
            Chord.augmentedMajorSeventh inversion key |> List.map Note.noteToStr

        _ ->
            [ "", "", "", "" ]


newChords key inversion =
    chordNames
        |> List.map (\chordName -> ( chordName, chordNotes chordName inversion key ))
        |> Dict.fromList
