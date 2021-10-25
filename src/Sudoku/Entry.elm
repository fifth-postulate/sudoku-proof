module Sudoku.Entry exposing (Model, Msg, empty, fromProblem, toProblem, update, view)

import Css exposing (..)
import Html.Events.Extra exposing (targetValueIntParse, targetValueMaybeInt)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Json.Decode as Decode
import Sudoku exposing (Cell, Domain, Problem, emptySudoku)


type alias Model =
    { m : Int, clues : List ( Cell, Domain ) }


empty : Int -> Model
empty m =
    { m = m, clues = [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 4 ), ( 4, 3 ) ] }


fromProblem : Int -> Problem -> Model
fromProblem size problem =
    let
        clues =
            problem
                |> Sudoku.toClues
    in
    { m = size, clues = clues }


toProblem : Model -> Problem
toProblem { m, clues } =
    clues
        |> List.map (uncurry Sudoku.fill)
        |> List.foldl Sudoku.execute (emptySudoku m)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


type Msg
    = SizeChanged Int
    | ClueAdded Cell Domain
    | ClueRemoved Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SizeChanged m ->
            ( { model | m = m, clues = [] }, Cmd.none )

        ClueAdded cell value ->
            ( { model | clues = ( cell, value ) :: model.clues }, Cmd.none )

        ClueRemoved cell ->
            ( { model | clues = List.filter (\( c, _ ) -> not (c == cell)) model.clues }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ viewSize model
        , viewClues model
        ]


viewSize : Model -> Html Msg
viewSize model =
    let
        decoder =
            targetValueIntParse
                |> Decode.map SizeChanged
    in
    Html.div []
        [ Html.select [ Event.on "change" decoder ] <| List.map (viewSizeOption model) [ 4, 9 ]
        ]


viewSizeOption : Model -> Int -> Html Msg
viewSizeOption { m } n =
    Html.option [ Attribute.value <| String.fromInt n, Attribute.selected <| m == n ] [ Html.text <| String.fromInt n ]


viewClues : Model -> Html Msg
viewClues ({ m } as model) =
    let
        clues =
            List.range 0 (m * m - 1)
                |> List.map (viewClue model)
    in
    Html.div
        [ Attribute.css
            [ grid
            , gridTemplateColumns <| List.repeat m "100px"
            , gridAutoRows "100px"
            ]
        ]
        clues


grid : Style
grid =
    property "display" "grid"


gridTemplateColumns : List String -> Style
gridTemplateColumns dimensions =
    property "grid-template-columns" <| String.join " " dimensions


gridAutoRows : String -> Style
gridAutoRows size =
    property "grid-auto-rows" size


viewClue : Model -> Int -> Html Msg
viewClue model index =
    let
        gray =
            rgb 192 192 192
    in
    Html.div
        [ Attribute.css
            [ borderColor gray
            , borderWidth <| px 1
            , borderStyle solid
            , displayFlex
            , justifyContent center
            , alignItems center
            , color (rgb 0 0 0)
            , fontSize medium
            , position relative
            ]
        ]
        [ Html.span
            [ Attribute.css
                [ fontSize xxSmall
                , position absolute
                , color gray
                , left (px 2)
                , top (px 2)
                ]
            ]
            [ Html.text <| String.fromInt index ]
        , viewClueOptions model index
        ]


viewClueOptions : Model -> Cell -> Html Msg
viewClueOptions { m, clues } cell =
    let
        id =
            cell
                |> String.fromInt
                |> String.padLeft 3 '0'
                |> (++) "cell-"

        current =
            clues
                |> List.filter (Tuple.first >> (==) cell)
                |> List.map Tuple.second
                |> List.head

        options =
            List.range 1 m
                |> List.map Just
                |> (::) Nothing
                |> List.map (viewOption cell current)

        decoder =
            targetValueMaybeInt
                |> Decode.map toMsg

        toMsg value =
            value
                |> Maybe.map (ClueAdded cell)
                |> Maybe.withDefault (ClueRemoved cell)
    in
    Html.select [ Attribute.id id, Event.on "change" decoder ] options


viewOption : Cell -> Maybe Domain -> Maybe Domain -> Html Msg
viewOption cell current option =
    case option of
        Just value ->
            let
                v =
                    String.fromInt value
            in
            Html.option [ Attribute.value v, Attribute.selected <| current == option ] [ Html.text v ]

        Nothing ->
            Html.option [ Attribute.value "", Attribute.selected <| current == Nothing ] [ Html.text "" ]
