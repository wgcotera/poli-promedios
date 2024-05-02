module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Result exposing (Result)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { primerParcial : Grade
    , segundoParcial : Grade
    , practico : Grade
    , mejoramiento : Grade
    , porcentajePractico : Grade
    , grade : Grade
    }


type Grade
    = NotEntered
    | Entered Float


init : Model
init =
    { primerParcial = NotEntered
    , segundoParcial = NotEntered
    , practico = NotEntered
    , mejoramiento = NotEntered
    , porcentajePractico = NotEntered
    , grade = NotEntered
    }



-- UPDATE


type Msg
    = UpdatePrimerParcial String
    | UpdateSegundoParcial String
    | UpdatePractico String
    | UpdateMejoramiento String
    | UpdatePorcentajePractico String
    | CalculateGrade
    | CalculateGradeResult (Result Http.Error Float)


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdatePrimerParcial primer_parcial ->
            { model | primerParcial = toGrade primer_parcial }

        UpdateSegundoParcial segundo_parcial ->
            { model | segundoParcial = toGrade segundo_parcial }

        UpdatePractico practico ->
            { model | practico = toGrade practico }

        UpdateMejoramiento mejoramiento ->
            { model | mejoramiento = toGrade mejoramiento }

        UpdatePorcentajePractico porcentaje_practico ->
            { model | porcentajePractico = toGrade porcentaje_practico }

        CalculateGrade ->
            { model
                | grade = NotEntered
            }

        CalculateGradeResult (Ok grade) ->
            { model | grade = Entered grade }

        CalculateGradeResult (Err _) ->
            { model | grade = NotEntered }


toGrade : String -> Grade
toGrade str =
    case String.toFloat str of
        Just value ->
            Entered value

        Nothing ->
            NotEntered



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
        , case model.grade of
            Entered grade ->
                div [ class "text-5xl" ] [ text <| "* " ++ String.fromFloat grade ++ " *" ]

            NotEntered ->
                div [] []
        ]


inputComponent : String -> (String -> msg) -> Html msg
inputComponent placeholderMsg msg =
    input [ class "border border-gray-400 rounded-lg p-2 m-2", onInput msg, placeholder placeholderMsg ] []
