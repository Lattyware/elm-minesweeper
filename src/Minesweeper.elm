module Minesweeper exposing (main)

import Browser
import Browser.Events as Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as Json
import Random
import Random.Set as Random
import Set exposing (Set)


type alias Flags =
    ()


boardSize : Int
boardSize =
    10


type alias Model =
    { game : Game
    , controlPressed : Bool
    }


type Game
    = Loading
    | Loaded { state : GameState, board : Board }


type GameState
    = Playing
    | Lost
    | Won


type alias Position =
    ( Int, Int )


type alias Cells cache =
    Dict Position (Cell cache)


type alias Board =
    { size : Int
    , mines : Int
    , cells : Cells Cache
    }


type CellState
    = Hidden { pushed : Bool }
    | Flagged
    | Revealed


type alias Cell cache =
    { cache
        | state : CellState
        , mined : Bool
    }


type alias Cache =
    { neighbouringMines : Int
    }


type Msg
    = NewGame
    | GenerateBoard { size : Int, mined : Set Position }
    | ToggleFlag Position
    | Reveal Position
    | SetControlPressed Bool
    | SetPushed { position : Position, pushed : Bool }
    | NoOp


main : Program Flags Model Msg
main =
    Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


newBoard : Cmd Msg
newBoard =
    let
        toMsg =
            \mined -> GenerateBoard { size = boardSize, mined = mined }
    in
    Random.generate toMsg (randomMinePositionGenerator boardSize boardSize)


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { game = Loading, controlPressed = False }, newBoard )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "Minesweeper"
    , body = [ viewGame model ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( { model | game = Loading }, newBoard )

        GenerateBoard { size, mined } ->
            let
                axis =
                    List.range 0 (size - 1)

                positions =
                    axis |> List.concatMap (\x -> axis |> List.map (\y -> ( x, y )))

                cells =
                    positions |> List.map (generateCell mined) |> Dict.fromList
            in
            ( { model | game = Loaded { state = Playing, board = Board size (Set.size mined) (computeCaches cells) } }
            , Cmd.none
            )

        ToggleFlag pos ->
            changeBoard model (atPosition pos toggleFlag)

        Reveal pos ->
            let
                op =
                    if model.controlPressed then
                        \b -> applyToSurrounding b pos reveal

                    else
                        \b -> Dict.get pos b.cells |> Maybe.map (reveal b pos) |> Maybe.withDefault b
            in
            changeBoard model op

        SetControlPressed controlPressed ->
            ( { model | controlPressed = controlPressed }, Cmd.none )

        SetPushed { position, pushed } ->
            let
                op =
                    if model.controlPressed then
                        \b -> applyToSurrounding b position (\board -> \p -> \_ -> atPosition p (trySetPushed pushed) board)

                    else
                        atPosition position (trySetPushed pushed)
            in
            changeBoard model op

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onKeyUp (decodeKeyPress False)
        , Events.onKeyDown (decodeKeyPress True)
        ]


viewGame : Model -> Html Msg
viewGame model =
    case model.game of
        Loading ->
            Html.text "Mining the field…"

        Loaded { state, board } ->
            let
                gameOver =
                    case state of
                        Playing ->
                            Nothing

                        Lost ->
                            Just "Oh no! You hit a mine!"

                        Won ->
                            Just "You cleared the field! Well done."
            in
            Html.div [ HtmlA.id "game" ]
                [ viewDetails board
                , viewBoard (gameOver /= Nothing) board
                , gameOver |> Maybe.map viewGameOver |> Maybe.withDefault (Html.text "")
                ]


viewDetails : Board -> Html msg
viewDetails board =
    Html.div [ HtmlA.id "details" ]
        [ Html.p [ HtmlA.class "mines-left", HtmlA.title "Mines Left" ]
            [ board.mines
                - (board.cells |> Dict.values |> List.filter (\c -> c.state == Flagged) |> List.length)
                |> String.fromInt
                |> String.padLeft 3 '0'
                |> Html.text
            ]
        ]


viewGameOver : String -> Html Msg
viewGameOver message =
    Html.div [ HtmlA.id "game-over" ]
        [ Html.div [ HtmlA.class "dialog" ]
            [ Html.p [] [ Html.text message ]
            , Html.button [ HtmlE.onClick NewGame ] [ Html.text "New Game" ]
            ]
        ]


viewBoard : Bool -> Board -> Html Msg
viewBoard gameOver board =
    Html.div [ HtmlA.id "board" ]
        [ Html.div [ HtmlA.class "aspect" ]
            [ board.cells
                |> Dict.toList
                |> List.map (viewCell gameOver)
                |> Html.div [ HtmlA.class "cells" ]
            ]
        ]


viewCell : Bool -> ( Position, Cell Cache ) -> Html Msg
viewCell gameOver ( position, { state, mined, neighbouringMines } ) =
    let
        ( x, y ) =
            position

        minesCountClassName =
            if state == Revealed then
                neighbouringMines |> minesCountClass

            else
                ""

        interaction =
            if gameOver then
                []

            else
                [ onRightClick (ToggleFlag position)
                , HtmlE.onClick (Reveal position)
                , HtmlE.onMouseDown (SetPushed { position = position, pushed = True })
                , HtmlE.onMouseLeave (SetPushed { position = position, pushed = False })
                ]
    in
    Html.button
        (List.concat
            [ interaction
            , [ HtmlA.classList
                    [ ( "cell", True )
                    , ( "flagged", state == Flagged )
                    , ( "revealed", state == Revealed )
                    , ( "mined", (state == Revealed || gameOver) && mined )
                    , ( "wrong", gameOver && not mined && state == Flagged )
                    , ( "pushed", state == Hidden { pushed = True } )
                    , ( minesCountClassName, state == Revealed && not mined )
                    ]
              , HtmlA.style "grid-area" (String.fromInt (y + 1) ++ " / " ++ String.fromInt (x + 1) ++ " / auto / auto")
              ]
            ]
        )
        []


minesCountClass : Int -> String
minesCountClass count =
    "m" ++ String.fromInt count


trySetPushed : Bool -> Cell a -> Cell a
trySetPushed pushed cell =
    case cell.state of
        Hidden _ ->
            { cell | state = Hidden { pushed = pushed } }

        _ ->
            cell


decodeKeyPress : Bool -> Json.Decoder Msg
decodeKeyPress changeTo =
    Json.field "key" Json.string
        |> Json.map
            (\k ->
                if k == "Control" then
                    SetControlPressed changeTo

                else
                    NoOp
            )


changeBoard : Model -> (Board -> Board) -> ( Model, Cmd msg )
changeBoard model change =
    case model.game of
        Loaded { board } ->
            let
                changed =
                    change board
            in
            ( { model | game = Loaded { state = changed |> boardState, board = changed } }, Cmd.none )

        _ ->
            ( model, Cmd.none )


boardState : Board -> GameState
boardState board =
    let
        values =
            board.cells |> Dict.values
    in
    if values |> List.any (\c -> c.mined && c.state == Revealed) then
        Lost

    else if values |> List.all (\c -> c.state == Revealed && not c.mined || c.state == Flagged && c.mined) then
        Won

    else
        Playing


minesCount : Cells a -> List Position -> Int
minesCount cells positions =
    positions
        |> List.filterMap (\pos -> Dict.get pos cells)
        |> List.map minesInCell
        |> List.sum


minesInCell : Cell a -> Int
minesInCell cell =
    if cell.mined then
        1

    else
        0


surrounding : Position -> List Position
surrounding ( x, y ) =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, -1 ), ( 0, 1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]
        |> List.map (\( x2, y2 ) -> ( x + x2, y + y2 ))


computeCaches : Cells {} -> Cells Cache
computeCaches cells =
    cells |> Dict.map (computeCache cells)


computeCache : Cells {} -> Position -> Cell {} -> Cell Cache
computeCache cells position { state, mined } =
    { state = state, mined = mined, neighbouringMines = position |> surrounding |> minesCount cells }


atPosition : Position -> (Cell Cache -> Cell Cache) -> Board -> Board
atPosition position change board =
    { board | cells = board.cells |> Dict.update position (Maybe.map change) }


reveal : Board -> Position -> Cell Cache -> Board
reveal board position cell =
    let
        ( newCell, cellChanged ) =
            tryReveal cell
    in
    if cellChanged then
        let
            changedBoard =
                { board | cells = Dict.insert position newCell board.cells }
        in
        if newCell.mined then
            changedBoard

        else if cell.neighbouringMines == 0 then
            applyToSurrounding changedBoard position reveal

        else
            changedBoard

    else
        board


applyToSurrounding : Board -> Position -> (Board -> Position -> Cell Cache -> Board) -> Board
applyToSurrounding board position f =
    surrounding position
        |> List.filterMap (\pos -> Dict.get pos board.cells |> Maybe.map (\c -> ( pos, c )))
        |> List.foldl (\( pos, c ) -> \b -> f b pos c) board


tryReveal : Cell a -> ( Cell a, Bool )
tryReveal cell =
    let
        state =
            case cell.state of
                Hidden _ ->
                    Revealed

                Flagged ->
                    Flagged

                Revealed ->
                    Revealed
    in
    ( { cell | state = state }, cell.state /= state )


toggleFlag : Cell a -> Cell a
toggleFlag cell =
    let
        state =
            case cell.state of
                Hidden _ ->
                    Flagged

                Flagged ->
                    Hidden { pushed = False }

                Revealed ->
                    Revealed
    in
    { cell | state = state }


randomMinePositionGenerator : Int -> Int -> Random.Generator (Set Position)
randomMinePositionGenerator size mines =
    randomPositionGenerator size |> Random.set mines


randomPositionGenerator : Int -> Random.Generator Position
randomPositionGenerator size =
    Random.map2 (\x -> \y -> ( x, y ))
        (Random.int 0 (size - 1))
        (Random.int 0 (size - 1))


generateCell : Set Position -> Position -> ( Position, Cell {} )
generateCell mined position =
    ( position
    , { state = Hidden { pushed = False }
      , mined = mined |> Set.member position
      }
    )


onRightClick : msg -> Html.Attribute msg
onRightClick msg =
    { message = msg
    , stopPropagation = True
    , preventDefault = True
    }
        |> Json.succeed
        |> HtmlE.custom "contextmenu"
