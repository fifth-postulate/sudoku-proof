module Visualizer.Entry exposing (Model, Msg, fromProblem, toProblem, update, view)

import Css exposing (..)
import Html.Events.Extra exposing (targetValueIntParse, targetValueMaybeInt)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Json.Decode as Decode
import Set
import Sudoku exposing (Problem, emptySudoku)
import Sudoku.Cell exposing (Cell)
import Sudoku.Clue exposing (Clue)
import Sudoku.Domain exposing (Domain)


type alias Model =
    { m : Int, clues : List Clue }


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
    = Resetted
    | SizeChanged Int
    | ClueAdded Clue
    | ClueRemoved Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        remove target clues =
            clues
                |> List.filter (\( c, _ ) -> not (c == target))
    in
    case msg of
        Resetted ->
            ( { model | clues = [] }, Cmd.none )

        SizeChanged m ->
            ( { model | m = m, clues = [] }, Cmd.none )

        ClueAdded (( cell, _ ) as clue) ->
            ( { model | clues = clue :: remove cell model.clues }, Cmd.none )

        ClueRemoved cell ->
            ( { model | clues = remove cell model.clues }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ viewControls model
        , viewClues model
        , viewProgram model
        ]


viewControls : Model -> Html Msg
viewControls model =
    Html.div [ Attribute.css [ displayFlex ] ]
        [ viewReset
        , viewSize model
        ]


viewReset : Html Msg
viewReset =
    Html.button [ Event.onClick Resetted ] [ Html.text "🗑" ]


viewSize : Model -> Html Msg
viewSize model =
    let
        decoder =
            targetValueIntParse
                |> Decode.map SizeChanged
    in
    Html.select [ Event.on "change" decoder ] <| List.map (viewSizeOption model) [ 4, 9 ]


viewSizeOption : Model -> Int -> Html Msg
viewSizeOption { m } n =
    Html.option [ Attribute.value <| String.fromInt n, Attribute.selected <| m == n ] [ Html.text <| String.fromInt n ]


viewClues : Model -> Html Msg
viewClues ({ m } as model) =
    let
        problem =
            toProblem model

        clues =
            List.range 0 (m * m - 1)
                |> List.map (viewClue model problem)
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


viewClue : Model -> Problem -> Cell -> Html Msg
viewClue model problem cell =
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
            [ Html.text <| String.fromInt cell ]
        , viewClueOptions model cell
        , viewCandidates problem cell
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
                |> Maybe.map (Tuple.pair cell >> ClueAdded)
                |> Maybe.withDefault (ClueRemoved cell)
    in
    Html.select [ Attribute.id id, Event.on "change" decoder ] options


viewOption : Cell -> Maybe Domain -> Maybe Domain -> Html Msg
viewOption _ current option =
    case option of
        Just value ->
            let
                v =
                    String.fromInt value
            in
            Html.option [ Attribute.value v, Attribute.selected <| current == option ] [ Html.text v ]

        Nothing ->
            Html.option [ Attribute.value "", Attribute.selected <| current == Nothing ] [ Html.text "" ]


viewCandidates : Problem -> Cell -> Html Msg
viewCandidates problem cell =
    let
        gray =
            rgb 192 192 192

        content =
            Sudoku.candidatesAt cell problem
                |> Set.toList
                |> List.map String.fromInt
                |> String.join ","
    in
    Html.span
        [ Attribute.css
            [ Css.color gray
            , fontSize xxSmall
            , position absolute
            , bottom (px 2)
            , right (px 2)
            ]
        ]
        [ Html.text content ]


viewProgram : Model -> Html msg
viewProgram { m, clues } =
    let
        h =
            Html.text <| "    emptySudoku " ++ String.fromInt m ++ "\n"

        cs =
            clues
                |> List.map (Tuple.mapBoth String.fromInt String.fromInt)
                |> List.map (\( cell, d ) -> "        |> clue " ++ cell ++ " " ++ d ++ "\n")
                |> List.map Html.text
    in
    Html.div []
        [ Html.pre [] (h :: cs)
        ]
