module Sudoku exposing (Action, Info, Problem, clue, emptySudoku, execute, fill, isOverConstrained, isSolved, options, view, viewAction)

import Array exposing (Array)
import Array.Util as Util
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Set exposing (Set)
import Sudoku.Blocks as Blocks


type Problem
    = Problem { states : Array State, blocks : List Block }


type State
    = Determined Domain
    | Options (Set Domain)


type alias Domain =
    Int


type alias Block =
    Set Cell


type alias Cell =
    Int


isSolved : Problem -> Bool
isSolved (Problem { states }) =
    Util.all isDetermined states


isOverConstrained : Problem -> Bool
isOverConstrained (Problem { states }) =
    Util.any (\c -> (not <| isDetermined c) && (Set.isEmpty <| candidates c)) states


isDetermined : State -> Bool
isDetermined state =
    case state of
        Determined _ ->
            True

        _ ->
            False


candidates : State -> Set Domain
candidates state =
    case state of
        Determined _ ->
            Set.empty

        Options domain ->
            domain


options : Problem -> List ( Cell, Set Domain )
options (Problem { states }) =
    states
        |> Array.indexedMap (\cell state -> ( cell, candidates state ))
        |> Array.toList


emptySudoku : Int -> Problem
emptySudoku m =
    let
        n =
            m * m

        emptyState =
            m
                |> List.range 1
                |> Set.fromList
                |> Options

        states =
            emptyState
                |> Array.repeat n
    in
    Problem { states = states, blocks = Blocks.sudokuBlocks m }


clue : Cell -> Domain -> Problem -> Problem
clue cell d =
    execute (Fill cell d)


type Action
    = Fill Cell Domain


fill : Cell -> Domain -> Action
fill =
    Fill


execute : Action -> Problem -> Problem
execute (Fill cell d) (Problem problem) =
    let
        consequences =
            problem.blocks
                |> List.filter (Set.member cell)
                |> List.foldl Set.union Set.empty
                |> Set.remove cell
                |> Set.toList
                |> List.map (RemoveCandidateAt d)
                |> (::) (Determine d cell)

        states =
            List.foldl apply problem.states consequences
    in
    Problem { problem | states = states }


type Consequence
    = Determine Domain Cell
    | RemoveCandidateAt Domain Cell


apply : Consequence -> Array State -> Array State
apply consequence states =
    case consequence of
        Determine d cell ->
            states
                |> Array.set cell (Determined d)

        RemoveCandidateAt d cell ->
            let
                updateState : State -> State
                updateState state =
                    case state of
                        Determined v ->
                            Determined v

                        Options domain ->
                            domain
                                |> Set.remove d
                                |> Options

                updatedState =
                    states
                        |> Array.get cell
                        |> Maybe.map updateState
            in
            updatedState
                |> Maybe.map (\s -> Array.set cell s states)
                |> Maybe.withDefault states



-- VIEW


type alias Info =
    { m : Int }


view : Info -> Problem -> Html msg
view { m } ((Problem { states }) as problem) =
    states
        |> Array.indexedMap (viewCell m problem)
        |> Array.toList
        |> Html.div
            [ Attribute.css
                [ grid
                , gridTemplateColumns <| List.repeat m "100px"
                , gridAutoRows "100px"
                ]
            ]


grid : Style
grid =
    property "display" "grid"


gridTemplateColumns : List String -> Style
gridTemplateColumns dimensions =
    property "grid-template-columns" <| String.join " " dimensions


gridAutoRows : String -> Style
gridAutoRows size =
    property "grid-auto-rows" size


viewCell : Int -> Problem -> Int -> State -> Html msg
viewCell m problem index cell =
    let
        content =
            case cell of
                Determined v ->
                    String.fromInt v

                Options cs ->
                    cs
                        |> Set.toList
                        |> List.map String.fromInt
                        |> List.sort
                        |> String.join ","

        color =
            case cell of
                Determined _ ->
                    rgb 0 0 0

                Options _ ->
                    rgb 200 200 200

        size =
            case cell of
                Determined _ ->
                    medium

                Options _ ->
                    xxSmall
    in
    Html.div
        [ Attribute.css
            [ borderColor (rgb 192 192 192)
            , borderWidth <| px 1
            , borderStyle solid
            , displayFlex
            , justifyContent center
            , alignItems center
            , Css.color color
            , fontSize size
            ]
        ]
        [ Html.text content ]


viewAction : Action -> Html msg
viewAction (Fill cell domain) =
    let
        message =
            String.join " " [ "write", String.fromInt domain, "in cell", String.fromInt cell ]
    in
    Html.span [] [ Html.text message ]
