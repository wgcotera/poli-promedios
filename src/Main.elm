module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, bool, float, succeed)
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
    { pass : Bool
    , missing : Float
    , grade : Float
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
      , result = GradeResult False 0 0
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
    div [ class "h-screen flex justify-center items-center gap-6" ]
        [ div [ class "flex flex-col items-center" ]
            [ inputComponent "Primer Parcial" UpdatePrimerParcial
            , inputComponent "Segundo Parcial" UpdateSegundoParcial
            , inputComponent "Practico" UpdatePractico
            , inputComponent "Mejoramiento" UpdateMejoramiento
            , inputComponent "Porcentaje Practico" UpdatePorcentajePractico
            , button [ class "border border-gray-400 rounded-lg p-2 m-2", onClick CalculateGrade ] [ text "Calcular Nota" ]
            ]
        , div [ class "flex flex-col items-center" ]
            [ div [] [ text <| "Nota Final: " ++ String.fromFloat model.result.grade ]
            , div []
                [ text <|
                    if model.result.pass then
                        "APROBASTE"

                    else
                        "KILL YOURSELF"
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- COMPONENTS


inputComponent : String -> (String -> msg) -> Html msg
inputComponent placeholderMsg msg =
    input [ class "border border-gray-400 rounded-lg p-2 m-2", onInput msg, placeholder placeholderMsg ] []



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
        |> required "pass" bool
        |> required "missing" float
        |> required "grade" float


getResult : Model -> Cmd Msg
getResult model =
    Http.post
        { url = "http://localhost:3000/promedios"
        , body = Http.jsonBody <| payloadEncoder model
        , expect = Http.expectJson UpdateResult resultDecoder
        }
