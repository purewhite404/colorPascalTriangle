module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import Svg exposing (Svg, svg)
import Svg.Attributes


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { multiple : Int
    , depth : Int
    }


init : Model
init =
    { multiple = 2
    , depth = 2
    }



-- VIEW


viewInput : String -> String -> (String -> Msg) -> Html Msg
viewInput prompt v toMsg =
    div []
        [ text prompt
        , input [ placeholder v, onInput toMsg ] []
        ]


viewPascal : Model -> Html msg
viewPascal model =
    div []
        [ svg
            [ Svg.Attributes.viewBox "0 0 800 800"
            , Svg.Attributes.width "800"
            , Svg.Attributes.height "800"
            ]
            (svgPascal model)
        ]


svgPascal : Model -> List (Svg msg)
svgPascal model =
    List.map (translateCoordinateIntoSvg model.multiple)
        (List.concat <| coordinatePascal <| createPascal model.depth)


translateCoordinateIntoSvg : Int -> ( ( Float, Int ), Int ) -> Svg msg
translateCoordinateIntoSvg m ( ( x, y ), value ) =
    let
        color =
            case modBy m value of
                0 ->
                    "red"

                _ ->
                    "black"
    in
    Svg.text_
        [ Svg.Attributes.x <| String.fromFloat (400 + x * 50)
        , Svg.Attributes.y <| String.fromInt (25 + y * 25)
        , Svg.Attributes.fill color
        ]
        [ text <| String.fromInt value ]


coordinatePascal : List (List Int) -> List (List ( ( Float, Int ), Int ))
coordinatePascal ascedPascal =
    let
        indexedPascal =
            List.indexedMap Tuple.pair ascedPascal
    in
    List.map coordinatePascalRow indexedPascal


coordinatePascalRow : ( Int, List Int ) -> List ( ( Float, Int ), Int )
coordinatePascalRow ( rownum, rowlist ) =
    let
        indexedRow =
            List.indexedMap Tuple.pair rowlist
    in
    List.map (coordinateCell rownum) indexedRow


coordinateCell : Int -> ( Int, Int ) -> ( ( Float, Int ), Int )
coordinateCell row ( index, value ) =
    ( ( toFloat -row / 2 + toFloat index, row ), value )


view : Model -> Html Msg
view model =
    div []
        [ viewInput "倍数" "2" MultipleSet
        , viewInput "深さ" "2" DepthSet
        , viewPascal model
        ]



-- UPDATE


type Msg
    = MultipleSet String
    | DepthSet String


update : Msg -> Model -> Model
update msg model =
    case msg of
        MultipleSet m ->
            { model | multiple = Maybe.withDefault 2 <| String.toInt m }

        DepthSet d ->
            { model | depth = Maybe.withDefault 2 <| String.toInt d }



-- Logic


createPascal : Int -> List (List Int)
createPascal depth =
    List.reverse <| createPascalHelp (depth - 2) [ [ 1, 1 ], [ 1 ] ]


createPascalHelp : Int -> List (List Int) -> List (List Int)
createPascalHelp d pascalList =
    if d <= 0 then
        pascalList

    else
        case pascalList of
            [] ->
                []

            preRow :: rest ->
                createPascalHelp (d - 1) <| (1 :: createPascalRow preRow) :: pascalList


createPascalRow : List Int -> List Int
createPascalRow preRow =
    case preRow of
        [] ->
            []

        x :: [] ->
            [ 1 ]

        x :: y :: xs ->
            (x + y) :: createPascalRow (y :: xs)
