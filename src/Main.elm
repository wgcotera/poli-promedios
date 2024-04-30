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
    { primerParcial : Maybe Float
    , segundoParcial : Maybe Float
    , practico : Maybe Float
    , mejoramiento : Maybe Float
    , porcentajePractico : Maybe Float
    , grade : Maybe Float
    }


init : Model
init =
    { primerParcial = Nothing
    , segundoParcial = Nothing
    , practico = Nothing
    , mejoramiento = Nothing
    , porcentajePractico = Nothing
    , grade = Nothing
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
            { model | primerParcial = String.toFloat primer_parcial }

        UpdateSegundoParcial segundo_parcial ->
            { model | segundoParcial = String.toFloat segundo_parcial }

        UpdatePractico practico ->
            { model | practico = String.toFloat practico }

        UpdateMejoramiento mejoramiento ->
            { model | mejoramiento = String.toFloat mejoramiento }

        UpdatePorcentajePractico porcentaje_practico ->
            { model | porcentajePractico = String.toFloat porcentaje_practico }

        CalculateGrade ->
            { model | grade = model.primerParcial }

        CalculateGradeResult (Ok grade) ->
            { model | grade = Just grade }

        CalculateGradeResult (Err _) ->
            { model | grade = Nothing }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "h-screen flex flex-col justify-center items-center" ]
        [ div [ class "flex flex-col items-center" ]
            [ input [ class "border border-gray-400 rounded-lg p-2 m-2", onInput UpdatePrimerParcial, placeholder "Primer Parcial" ] []
            , input [ class "border border-gray-400 rounded-lg p-2 m-2", onInput UpdateSegundoParcial, placeholder "Segundo Parcial" ] []
            , input [ class "border border-gray-400 rounded-lg p-2 m-2", onInput UpdatePractico, placeholder "Practico" ] []
            , input [ class "border border-gray-400 rounded-lg p-2 m-2", onInput UpdateMejoramiento, placeholder "Mejoramiento" ] []
            , input [ class "border border-gray-400 rounded-lg p-2 m-2", onInput UpdatePorcentajePractico, placeholder "Porcentaje Practico" ] []
            , button [ class "border border-gray-400 rounded-lg p-2 m-2", onClick CalculateGrade ] [ text "Calcular Nota" ]
            ]
        , case model.grade of
            Just grade ->
                div [ class "text-2xl" ] [ text <| "Nota: " ++ String.fromFloat grade ]

            Nothing ->
                div [] []
        ]
