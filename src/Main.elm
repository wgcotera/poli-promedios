module Main exposing (..)

import Browser
import Html exposing (Html, button, div, img, input, label, span, text)
import Html.Attributes exposing (class, src)
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


initModel : Model
initModel =
    { primerParcial = NotEntered
    , segundoParcial = NotEntered
    , practico = NotEntered
    , mejoramiento = NotEntered
    , porcentajePractico = NotEntered
    , result = GradeResult Nothing Nothing Nothing
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( initModel
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateField Field String
    | CalculateGrade
    | UpdateResult (Result Http.Error GradeResult)


type Field
    = PrimerParcial
    | SegundoParcial
    | Practico
    | Mejoramiento
    | PorcentajePractico


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateField field value ->
            let
                updateModel =
                    { model | result = GradeResult Nothing Nothing Nothing }
                        |> (case field of
                                PrimerParcial ->
                                    updateGrade (\m g -> { m | primerParcial = g }) value

                                SegundoParcial ->
                                    updateGrade (\m g -> { m | segundoParcial = g }) value

                                Practico ->
                                    updateGrade (\m g -> { m | practico = g }) value

                                Mejoramiento ->
                                    updateGrade (\m g -> { m | mejoramiento = g }) value

                                PorcentajePractico ->
                                    updateGrade (\m g -> { m | porcentajePractico = g }) value
                           )
            in
            ( updateModel, Cmd.none )

        CalculateGrade ->
            ( model, getResult model )

        UpdateResult (Ok result) ->
            ( { model | result = result }, Cmd.none )

        UpdateResult (Err _) ->
            ( { model | result = GradeResult Nothing Nothing Nothing }, Cmd.none )


updateGrade : (Model -> Grade -> Model) -> String -> Model -> Model
updateGrade setGrade value model =
    let
        updatedValue =
            parseGrade value

        updatedModel =
            setGrade model updatedValue
    in
    updatedModel



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "h-screen flex flex-col-reverse lg:flex-row justify-center items-center gap-12 " ]
        [ div [ class "flex flex-col items-center xl:w-1/3 lg:w-1/2 sm:w-2/3 border-2 border-blue-950 rounded py-12 px-6" ]
            [ div [ class "flex flex-col sm:flex-row gap-4 sm:gap-6 justify-center" ]
                [ div [ class "w-full flex flex-col gap-4" ]
                    [ inputComponent "Primer Parcial:" (UpdateField PrimerParcial)
                    , inputComponent "Segundo Parcial:" (UpdateField SegundoParcial)
                    , inputComponent "Mejoramiento:" (UpdateField Mejoramiento)
                    ]
                , div [ class "w-full flex flex-col gap-4" ]
                    [ inputComponent "Practico:" (UpdateField Practico)
                    , inputComponent "Porcentaje Practico:" (UpdateField PorcentajePractico)
                    ]
                ]
            , button
                [ class "border border-gray-400 rounded-lg bg-blue-950 text-white uppercase p-2 mt-6 w-full md:w-2/3 lg:w-1/2 mx-auto self-end"
                , onClick CalculateGrade
                ]
                [ text "Calcular" ]
            ]
        , viewResult model.result
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- COMPONENTS


inputComponent : String -> (String -> msg) -> Html msg
inputComponent labelTxt msg =
    div []
        [ label [ class "text-blue-950" ] [ text labelTxt ]
        , input [ class "border border-gray-400 rounded-lg p-2 text-right w-full", onInput msg ] []
        ]


viewGrade : Maybe Float -> Html msg
viewGrade maybeGrade =
    case maybeGrade of
        Just grade ->
            div
                [ class <|
                    "text-2xl "
                        ++ (if grade < 60 then
                                "text-red-700"

                            else
                                "text-green-700"
                           )
                ]
                [ text <| "Tu nota es: " ++ String.fromFloat grade ]

        Nothing ->
            div [] []


viewMissing : Maybe Float -> Html msg
viewMissing maybeMissing =
    case maybeMissing of
        Just missing ->
            div [ class "text-2xl" ]
                [ text "Debes sacar: "
                , text <| String.fromFloat missing
                ]

        Nothing ->
            div [] []


viewPassMsg : Maybe Bool -> Html msg
viewPassMsg maybePass =
    case maybePass of
        Just pass ->
            div [ class <| "text-2xl mt-2" ]
                [ text <|
                    if pass then
                        "¡Buen trabajo, aprobaste!"

                    else
                        "Reprobaste"
                ]

        Nothing ->
            div [] []


viewResult : GradeResult -> Html msg
viewResult result =
    div [ class "flex flex-col items-center" ]
        [ div []
            [ img
                [ src "https://img.icons8.com/external-itim2101-fill-itim2101/64/000000/external-turtle-plastic-pollution-itim2101-fill-itim2101-2.png"
                , class "mb-4"
                ]
                []
            ]
        , viewGrade result.grade
        , viewPassMsg result.pass
        , case result.missing of
            Just missing ->
                viewMissing <| Just missing

            Nothing ->
                div [] []
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
