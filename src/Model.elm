module Model exposing (Model, Msg(..), chordNotesEmpty, chordIntervals, chordTypes, init, update)

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


{-| The initial (empty) model.
-}
init : ( Model, Cmd Msg )
init =
    ( { key = ""
      , chords = chordsEmpty
      , inversion = 0
      , message = ""
      }
    , Cmd.none
    )


{-| A list of all implemented chord types.
-}
chordTypes : List String
chordTypes =
    [ "Diminished"
    , "Half-diminished"
    , "Minor"
    , "Minor-major"
    , "Dominant"
    , "Major"
    , "Augmented"
    , "Augmented-major"
    ]


{-| A placeholder for no chords.
-}
chordsEmpty : Dict String (List String)
chordsEmpty =
    chordTypes
        |> List.map (\chordType -> ( chordType, chordNotesEmpty ))
        |> Dict.fromList


{-| A placeholder for an empty chord.
-}
chordNotesEmpty : List String
chordNotesEmpty = [ "", "", "", "" ]


{-| Returns the intervals for a given chord.
-}
chordIntervals : String -> List String
chordIntervals chordType =
    case chordType of
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
            []



-- UPDATE


type Msg
    = ChangeKey String
    | ChangeInversion String


{-| Updates the model and sends a relevant command.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeKey newKey ->
            let
                newModel =
                    updateKey newKey model
            in
            ( newModel, encodeData newModel |> toJs )

        ChangeInversion newInversion ->
            let
                newModel =
                    updateInversion newInversion model
            in
            ( newModel, encodeData newModel |> toJs )


{-| Updates the current key.
-}
updateKey : String -> Model -> Model
updateKey newKey model =
    case Note.strToNote newKey of
        Just note ->
            { model | key = newKey, chords = newChords note model.inversion }

        Nothing ->
            { model | key = newKey, chords = chordsEmpty }


{-| Updates the current chord inversion.
-}
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


{-| A dictionary of chords for a given key and inversion.
-}
newChords key inversion =
    chordTypes
        |> List.map (\chordType -> ( chordType, chordNotes chordType inversion key ))
        |> Dict.fromList


{-| A list of stringified notes for a given chord, inversion, and key.
-}
chordNotes : String -> Int -> Note.Note -> List String
chordNotes chordType inversion key =
    case chordType of
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
            []



-- JSON


{-| Encodes relevant data for JS-side processing.
-}
encodeData : Model -> String
encodeData model =
    let
        jsonObject =
            [ ( "key", capitalizeKey model.key |> Json.Encode.string )
            , ( "chords", encodeChords model )
            ]
                |> Json.Encode.object
    in
    Json.Encode.encode 0 jsonObject


{-| Capitalizes a key, e.g. c# -> C#, bb -> B.
-}
capitalizeKey : String -> String
capitalizeKey key =
    String.toUpper (String.left 1 key) ++ String.dropLeft 1 key


{-| Encodes chords into a JSON object with string-encoded chords keyed by chord name.
-}
encodeChords : Model -> Json.Encode.Value
encodeChords model =
    chordTypes
        |> List.map
            (\chordType ->
                ( chordType
                , model.chords
                    |> Dict.get chordType
                    |> Maybe.withDefault [ "", "", "", "" ]
                    |> encodeNotesAndAccidentals model.key
                    |> String.join ","
                    |> Json.Encode.string
                )
            )
        |> Json.Encode.object


{-| Splits accidentals into separate strings for JS-side processing.
-}
encodeNotesAndAccidentals : String -> List String -> List String
encodeNotesAndAccidentals key notes =
    -- Elems 1-4 : Notes (w/out accidentals)
    -- Elems 5-8 : Accidentals
    case notes of
        ["", "", "", ""] ->
            []

        _ ->
            List.concat
                [ notes |> List.map (String.left 1)
                , notes |> List.map (String.dropLeft 1)
                ]
