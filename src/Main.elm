port module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Bootstrap.Button as Button
import Bootstrap.Utilities.Size as Size
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Progress as Progress


type alias Flags = { }

type alias Model = {
    countF: Int,
    countJ: Int
    }


port cookieReceiver : (String -> msg) -> Sub msg
port fetchCookies : () -> Cmd msg
port setCookie : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    cookieReceiver GotCookie


emptyModel : Model
emptyModel =
    { countF = 0, countJ = 0 }


main =
    Browser.element 
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


type Msg
    = IncrementF
    | IncrementJ
    | DecrementF
    | DecrementJ
    | Reset
    | GotCookie String


init : Flags -> (Model, Cmd Msg)
init _ =
    (emptyModel, fetchCookies ())


deconstructCookie : String -> (Int, Int)
deconstructCookie content =
    -- Cookie is in the form "F=1; J=2"
    let
        parseInt = String.toInt >> Maybe.withDefault 0
        parseLine line =
            case (String.split "=" line) of
                [ "F", value ] ->
                    Just ("F", parseInt value)
                [ "J", value ] ->
                    Just ("J", parseInt value)
                _ ->
                    Nothing
    in
        case (content |> String.split "|") |> List.map parseLine of
            [ (Just ("F", f)), Just ("J", j) ] ->
                (f,j)
            [ (Just ("J", j)), Just ("F", f) ] ->
                (f,j)
            _ ->
                (0,0)


constructCookie : (Int, Int) -> String
constructCookie (f, j) =
    "F=" ++ (String.fromInt f) ++ "|J=" ++ (String.fromInt j)


modelToCookie : Model -> String
modelToCookie model =
    constructCookie (model.countF, model.countJ)


updateModelFromCookie : String -> Model -> Model
updateModelFromCookie content model =
    let
        (f, j) = deconstructCookie content
    in
        { model | countF = f, countJ = j }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        IncrementF ->
            let 
                updatedModel = { model | countF = model.countF + 1 }
            in
                (updatedModel, setCookie (updatedModel |> modelToCookie))
        IncrementJ ->
            let 
                updatedModel = { model | countJ = model.countJ + 1 }
            in
                (updatedModel, setCookie (updatedModel |> modelToCookie))

        DecrementF ->
            let 
                updatedModel = { model | countF = model.countF - 1 }
            in
                (updatedModel, setCookie (updatedModel |> modelToCookie))

        DecrementJ ->
            let 
                updatedModel = { model | countJ = model.countJ - 1 }
            in
                (updatedModel, setCookie (updatedModel |> modelToCookie))

        Reset ->
            (emptyModel, setCookie (emptyModel |> modelToCookie))

        GotCookie text ->
            let
                (f, j) = text |> deconstructCookie
            in
                ({ model | countF = f, countJ = j } , Cmd.none)


counterRow : String -> Int -> Int -> Msg -> Html Msg
counterRow name total count msg =
    let
        value = if count == 0 then 0 else toFloat(count) / toFloat(total) * 100.0
    in
        div [ Spacing.mt3 ]
            [ Grid.row [ ]
                [ Grid.col [ Col.xs3, Col.md2 ] [ Button.button [ Button.primary, Button.attrs [ onClick msg, class "w-100" ] ] [ text (name ++ "++") ] ]
                , Grid.col [ Col.xs1, Col.md1, Col.attrs [ Flex.block, Flex.alignItemsCenter, Flex.justifyCenter, class "font-weight-bold" ] ] [ text (String.fromInt count) ]
                , Grid.col [ Col.xs8, Col.md9 ] [ Progress.progress [ Progress.value value, Progress.attrs [ Size.h100 ] ] ]
                ]
            ]


view model =
    let total = model.countF + model.countJ
    in
        div []
            [ counterRow "F" total model.countF IncrementF
            , counterRow "J" total model.countJ IncrementJ
            , Grid.row []
                [ Grid.col [ Col.xs3, Col.md2, Col.attrs [ Spacing.mt4 ] ] [ Button.button [ Button.danger, Button.attrs [ onClick Reset, class "w-100" ] ] [ text "Reset" ] ]
                ]
            ]
