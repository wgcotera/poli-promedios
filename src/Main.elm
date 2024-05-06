module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, bool, float, nullable, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Result exposing (Result)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Grade
    = NotEntered
    | Entered Float
    | Error String


type alias GradeResult =
    { pass : Maybe Bool
    , missing : Maybe Float
    , grade : Maybe Float
    }


type alias Model =
    { primerParcial : Grade
    , segundoParcial : Grade
    , practico : Grade
    , mejoramiento : Grade
    , porcentajePractico : Grade
    , result : GradeResult
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { primerParcial = NotEntered
      , segundoParcial = NotEntered
      , practico = NotEntered
      , mejoramiento = NotEntered
      , porcentajePractico = NotEntered
      , result = { pass = Nothing, missing = Nothing, grade = Nothing }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdatePrimerParcial String
    | UpdateSegundoParcial String
    | UpdatePractico String
    | UpdateMejoramiento String
    | UpdatePorcentajePractico String
    | CalculateGrade
    | UpdateResult (Result Http.Error GradeResult)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePrimerParcial primer_parcial ->
            ( { model | primerParcial = parseGrade primer_parcial }, Cmd.none )

        UpdateSegundoParcial segundo_parcial ->
            ( { model | segundoParcial = parseGrade segundo_parcial }, Cmd.none )

        UpdatePractico practico ->
            ( { model | practico = parseGrade practico }, Cmd.none )

        UpdateMejoramiento mejoramiento ->
            ( { model | mejoramiento = parseGrade mejoramiento }, Cmd.none )

        UpdatePorcentajePractico porcentaje_practico ->
            ( { model | porcentajePractico = parseGrade porcentaje_practico }, Cmd.none )

        CalculateGrade ->
            ( model, getResult model )

        UpdateResult (Ok result) ->
            ( { model | result = result }, Cmd.none )

        UpdateResult (Err _) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "h-screen flex flex-col-reverse lg:flex-row justify-center items-center gap-12" ]
        [ div [ class "flex flex-col items-center xl:w-1/3 lg:w-1/2 sm:w-2/3" ]
            [ div [ class "flex flex-col sm:flex-row gap-4 sm:gap-6 justify-center" ]
                [ div [ class "w-full flex flex-col gap-4" ]
                    [ inputComponent "Primer Parcial" UpdatePrimerParcial
                    , inputComponent "Segundo Parcial" UpdateSegundoParcial
                    , inputComponent "Mejoramiento" UpdateMejoramiento
                    ]
                , div [ class "w-full flex flex-col gap-4" ]
                    [ inputComponent "Practico" UpdatePractico
                    , inputComponent "Porcentaje Practico" UpdatePorcentajePractico
                    , button [ class "border border-gray-400 rounded-lg bg-blue-900 text-white uppercase p-2 w-full", onClick CalculateGrade ] [ text "Calcular" ]
                    ]
                ]
            ]
        , viewResult model.result
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- COMPONENTS


inputComponent : String -> (String -> msg) -> Html msg
inputComponent placeholderMsg msg =
    input [ class "border border-gray-400 rounded-lg p-2 text-right w-full", onInput msg, placeholder placeholderMsg ] []


viewGrade : Maybe Float -> Html msg
viewGrade maybeGrade =
    case maybeGrade of
        Just grade ->
            div []
                [ text <| String.fromFloat grade
                ]

        Nothing ->
            div [] []


viewPassMsg : Maybe Bool -> Html msg
viewPassMsg maybePass =
    case maybePass of
        Just pass ->
            div []
                [ text <|
                    if pass then
                        "Great, you pass"

                    else
                        "Sad, you didn't pass :("
                ]

        Nothing ->
            div [] []


viewResult : GradeResult -> Html msg
viewResult result =
    div [ class "flex flex-col items-center" ]
        [ viewPassMsg result.pass
        , viewGrade result.grade
        , viewGrade result.missing
        ]



-- HELPERS


parseGrade : String -> Grade
parseGrade str =
    case String.toFloat str of
        Just value ->
            Entered value

        Nothing ->
            NotEntered


gradeToFloat : Grade -> Float
gradeToFloat grade =
    case grade of
        Entered value ->
            value

        _ ->
            0


payloadEncoder : Model -> Encode.Value
payloadEncoder model =
    Encode.object
        [ ( "primer_parcial", Encode.float <| gradeToFloat model.primerParcial )
        , ( "segundo_parcial", Encode.float <| gradeToFloat model.segundoParcial )
        , ( "practico", Encode.float <| gradeToFloat model.practico )
        , ( "mejoramiento", Encode.float <| gradeToFloat model.mejoramiento )
        , ( "porcentaje_practico", Encode.float <| gradeToFloat model.porcentajePractico )
        ]



-- API


resultDecoder : Decoder GradeResult
resultDecoder =
    succeed GradeResult
        |> required "pass" (nullable bool)
        |> required "missing" (nullable float)
        |> required "grade" (nullable float)


getResult : Model -> Cmd Msg
getResult model =
    Http.post
        { url = "http://localhost:3000/promedios"
        , body = Http.jsonBody <| payloadEncoder model
        , expect = Http.expectJson UpdateResult resultDecoder
        }
