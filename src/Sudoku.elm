module Sudoku exposing (Fuel(..), Info, Msg(..), Problem, Strategy, Suggestion, clue, emptySudoku, execute, isSolved, shouldBe, solve, solveWithFuel, update, view)

import Array exposing (Array)
import Array.Util as Util exposing (member)
import Browser
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Set exposing (Set)
import Set.Util exposing (pick)
import Stream exposing (Stream)
import Sudoku.Blocks as Blocks


type Problem
    = Problem { states : Array State, blocks : List Block }


type State
    = Determined Domain
    | Candidates (Set Domain)


type alias Domain =
    Int


type alias Block =
    Set Cell


type alias Cell =
    Int


isSolved : Problem -> Bool
isSolved (Problem { states }) =
    Util.all isDetermined states


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

        Candidates domain ->
            domain


emptySudoku : Int -> Problem
emptySudoku m =
    let
        n =
            m * m

        emptyState =
            m
                |> List.range 1
                |> Set.fromList
                |> Candidates

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


execute : Action -> Problem -> Problem
execute (Fill cell d) (Problem problem) =
    let
        consequences =
            problem.blocks
                |> List.filter (Set.member cell)
                |> List.foldl Set.union Set.empty
                |> Set.remove cell
                |> Set.toList
                |> List.map (\c -> RemoveCandidateAt c d)
                |> (::) (Determine cell d)

        states =
            List.foldl apply problem.states consequences
    in
    Problem { problem | states = states }


type Consequence
    = Determine Cell Domain
    | RemoveCandidateAt Cell Domain


apply : Consequence -> Array State -> Array State
apply consequence states =
    case consequence of
        Determine cell d ->
            states
                |> Array.set cell (Determined d)

        RemoveCandidateAt cell d ->
            let
                updateState : State -> State
                updateState state =
                    case state of
                        Determined v ->
                            Determined v

                        Candidates domain ->
                            domain
                                |> Set.remove d
                                |> Candidates

                updatedState =
                    states
                        |> Array.get cell
                        |> Maybe.map updateState
            in
            updatedState
                |> Maybe.map (\s -> Array.set cell s states)
                |> Maybe.withDefault states


type alias Strategy =
    Problem -> Maybe Action


type Suggestion
    = ShouldBe Domain


shouldBe : Domain -> Suggestion
shouldBe =
    ShouldBe


actOn : Cell -> Suggestion -> Action
actOn cell (ShouldBe d) =
    Fill cell d


type Fuel
    = Finite Int
    | Infinite


consume : Fuel -> Maybe Fuel
consume fuel =
    case fuel of
        Finite 0 ->
            Nothing

        Finite n ->
            Just <| Finite <| n - 1

        Infinite ->
            Just <| Infinite


solve : Strategy
solve =
    solveWithFuel Infinite


solveWithFuel : Fuel -> Strategy
solveWithFuel fuel ((Problem { states }) as problem) =
    states
        |> toTreeStream
        |> firstSuggestionWith fuel problem


type Tree
    = Leaf Cell (Set Domain)
    | Node Cell (Set Domain) (Array ( Block, Array Tree ))


rootCell : Tree -> Cell
rootCell tree =
    case tree of
        Leaf cell _ ->
            cell

        Node cell _ _ ->
            cell


rootDomain : Tree -> Set Domain
rootDomain tree =
    case tree of
        Leaf _ domain ->
            domain

        Node _ domain _ ->
            domain


rootChildren : Tree -> Array ( Block, Array Tree )
rootChildren tree =
    case tree of
        Leaf _ _ ->
            Array.empty

        Node _ _ children ->
            children


effectiveCandidates : Tree -> Set Domain
effectiveCandidates tree =
    case tree of
        Leaf _ domain ->
            domain

        Node _ domain children ->
            let
                exclude =
                    children
                        |> Array.toList
                        |> List.concatMap (Tuple.second >> Array.toList)
                        |> List.map effectiveCandidates
                        |> List.foldl Set.union Set.empty
            in
            Set.diff domain exclude


toTreeStream : Array State -> Stream Tree
toTreeStream cells =
    let
        order ( leftCell, leftState ) ( rightCell, rightState ) =
            case compare (size leftState) (size rightState) of
                EQ ->
                    compare leftCell rightCell

                _ as o ->
                    o

        size state =
            Set.size <| candidates <| state
    in
    cells
        |> Array.indexedMap Tuple.pair
        |> Array.filter (Tuple.second >> isDetermined >> not)
        |> Array.toList
        |> List.sortWith order
        |> List.map (Tuple.mapSecond candidates)
        |> List.map (uncurry Leaf)
        |> Stream.fromList


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


firstSuggestionWith : Fuel -> Problem -> Stream Tree -> Maybe Action
firstSuggestionWith fuel problem stream =
    let
        consumedFuelSuggestion candidate =
            fuel
                |> consume
                |> Maybe.andThen (\remainingFuel -> suggestionFromTree remainingFuel problem candidate)
    in
    stream
        |> Stream.head
        |> Maybe.andThen consumedFuelSuggestion


suggestionFromTree : Fuel -> Problem -> ( Tree, Stream Tree ) -> Maybe Action
suggestionFromTree fuel problem ( tree, stream ) =
    let
        domain =
            effectiveCandidates tree
    in
    if Set.size domain == 1 then
        domain
            |> pick
            |> Maybe.map shouldBe
            |> Maybe.map (actOn <| rootCell tree)

    else
        stream
            |> Stream.afterwards (\_ -> sprout problem tree)
            |> firstSuggestionWith fuel problem


sprout : Problem -> Tree -> Stream Tree
sprout ((Problem { blocks }) as problem) tree =
    case tree of
        Leaf cell _ ->
            blocks
                |> List.filter (Set.member cell)
                |> List.map (addBlockToRoot problem tree)
                |> List.foldl Stream.afterwards Stream.empty

        Node cell domain ancestors ->
            let
                usedBlocks =
                    ancestors
                        |> Array.map Tuple.first

                rootStream =
                    blocks
                        |> List.filter (Set.member cell)
                        |> List.filter (\b -> not <| member b usedBlocks)
                        |> List.map (addBlockToRoot problem tree)
                        |> List.foldl Stream.afterwards Stream.empty

                ancestorStream : () -> Stream Tree
                ancestorStream =
                    \_ ->
                        ancestors
                            |> Array.indexedMap (blocksStream ancestors)
                            |> Array.foldl Stream.afterwards Stream.empty
                            |> Stream.map (Node cell domain)

                blocksStream : Array ( Block, Array Tree ) -> Int -> ( Block, Array Tree ) -> () -> Stream (Array ( Block, Array Tree ))
                blocksStream originalAncestors blockIndex block =
                    let
                        updateAncestor b =
                            Array.set blockIndex b originalAncestors
                    in
                    \_ ->
                        blockStream block
                            |> Stream.map updateAncestor

                blockStream : ( Block, Array Tree ) -> Stream ( Block, Array Tree )
                blockStream ( block, children ) =
                    children
                        |> Array.indexedMap (childrenStream children)
                        |> Array.foldl Stream.afterwards Stream.empty
                        |> Stream.map (Tuple.pair block)

                childrenStream : Array Tree -> Int -> Tree -> () -> Stream (Array Tree)
                childrenStream originalChildren childIndex child =
                    let
                        updateChild c =
                            Array.set childIndex c originalChildren
                    in
                    \_ ->
                        sprout problem child
                            |> Stream.map updateChild
            in
            rootStream
                |> Stream.afterwards ancestorStream


addBlockToRoot : Problem -> Tree -> Block -> () -> Stream Tree
addBlockToRoot (Problem { states }) tree block =
    let
        cell =
            rootCell tree

        domain =
            rootDomain tree

        children =
            rootChildren tree

        trees =
            block
                |> Set.remove cell
                |> Set.filter hasCandidates
                |> Set.toList
                |> List.map toLeaf
                |> Array.fromList

        hasCandidates candidate =
            states
                |> Array.get candidate
                |> Maybe.map (not << isDetermined)
                |> Maybe.withDefault False

        toLeaf c =
            let
                d =
                    states
                        |> Array.get c
                        |> Maybe.map candidates
                        |> Maybe.withDefault Set.empty
            in
            Leaf c d
    in
    \_ -> Stream.singleton <| Node cell domain <| Array.push ( block, trees ) children



-- VIEW


type alias Info =
    { m : Int }


type Msg
    = Advance


update : Msg -> Problem -> Problem
update msg problem =
    case msg of
        Advance ->
            problem
                |> solve
                |> Maybe.map (flip execute problem)
                |> Maybe.withDefault problem


flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b


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

                Candidates cs ->
                    cs
                        |> Set.toList
                        |> List.map String.fromInt
                        |> List.sort
                        |> String.join ","

        color =
            case cell of
                Determined _ ->
                    rgb 0 0 0

                Candidates _ ->
                    rgb 200 200 200

        size =
            case cell of
                Determined _ ->
                    medium

                Candidates _ ->
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
