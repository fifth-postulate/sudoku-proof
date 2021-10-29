module Sudoku exposing (Action, Cell, Domain, Info, Problem, emptySudoku, execute, fill, isOverConstrained, isSolved, options, toClue, toClues, view, viewAction)

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


toClues : Problem -> List ( Cell, Domain )
toClues (Problem { states }) =
    let
        toValue state =
            case state of
                Determined v ->
                    v

                Options _ ->
                    -- This option is impossible
                    0

        lift : (State -> a) -> ( Cell, State ) -> ( Cell, a )
        lift f ( cell, state ) =
            ( cell, f state )
    in
    states
        |> Array.indexedMap (\cell state -> ( cell, state ))
        |> Array.filter (Tuple.second >> isDetermined)
        |> Array.map (lift toValue)
        |> Array.toList


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


toClue : Action -> ( Cell, Domain )
toClue (Fill cell domain) =
    ( cell, domain )


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


viewCell : Int -> Problem -> Cell -> State -> Html msg
viewCell m problem cell state =
    let
        content =
            case state of
                Determined v ->
                    String.fromInt v

                Options cs ->
                    cs
                        |> Set.toList
                        |> List.map String.fromInt
                        |> List.sort
                        |> String.join ","

        black =
            rgb 0 0 0

        gray =
            rgb 192 192 192

        color =
            case state of
                Determined _ ->
                    black

                Options _ ->
                    gray

        size =
            case state of
                Determined _ ->
                    medium

                Options _ ->
                    xxSmall
    in
    Html.div
        [ Attribute.css
            [ borderColor gray
            , borderWidth <| px 1
            , borderStyle solid
            , displayFlex
            , justifyContent center
            , alignItems center
            , Css.color color
            , fontSize size
            , position relative
            ]
        ]
        [ Html.span [ Attribute.css [ Css.color color, fontSize size ] ] [ Html.text content ]
        , Html.span
            [ Attribute.css
                [ Css.color gray
                , fontSize xxSmall
                , position absolute
                , top (px 2)
                , left (px 2)
                ]
            ]
            [ Html.text <| String.fromInt cell ]
        ]


viewAction : Action -> Html msg
viewAction (Fill cell domain) =
    let
        message =
            String.join " " [ "write", String.fromInt domain, "in cell", String.fromInt cell ]
    in
    Html.span [] [ Html.text message ]
