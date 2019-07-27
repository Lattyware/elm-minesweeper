# How To Build Minesweeper With Elm

This is a tutorial inspired by [the article "How To Build Minesweeper With JavaScript"][how-to-build-minesweeper]. I 
read the aforementioned article and thought it would make an interesting little project to compare implementation 
across the two languages. I won't seek to cover ground that article already does, and will be making comparisons, so 
you might want to have two tabs open to refer to the original article.

[how-to-build-minesweeper]: https://mitchum.blog/how-to-build-minesweeper-with-javascript/

You can [click here][play] to play this version.

[play]: https://lattyware.github.io/elm-minesweeper

I'm not aiming to go in-depth into how Elm works here. You might want to read [the official language guide][elm-guide]
first if you are not familiar with it. That said, I will try to explain anything too esoteric. If you want to try making 
changes or follow along, I suggest [`elm-live`][elm-live] for the easiest experience.

[elm-guide]: https://guide.elm-lang.org/
[elm-live]: https://github.com/wking-io/elm-live

## Data Structures

Elm is a pure functional language, so starting with the data structures works very well.

### Cell

    type CellState
        = Hidden { pushed : Bool }
        | Flagged
        | Revealed

    type alias Cache =
        { neighbouringMines : Int
        }
    
    type alias Cell cache =
        { cache
             | state : CellState
             , mined : Bool
        }
        
Already, this looks rather different. The first and most significant change here is changing from using three booleans
(`flagged`, `opened`, `mined`) to a custom type and a bool. This makes a lot of sense as using three booleans allows us
to have states that don't make sense—a player can't flag an opened cell, for example. By modeling our data like this we 
make it harder to make errors and make it easier to cover all potential cases of our data structure.

The `pushed` argument for the `Hidden` state is a small enhancement over the original version—when the player pushes
down on the mouse, we style the cell with an inset, letting them know what is about to be revealed. This is useful 
when using the <kbd>Control</kbd> key to see what cells will be affected.

You may also notice a lot of things are missing. Repeating the `row`/`column` information doesn't buy us a lot in a 
language without references, and introduces more potential for things getting out of sync. Where possible, keeping a 
single source of truth is preferable.

We also moved the `neighbourMineCount`. The simplest option would be not to have it at all—we can compute the value when
we need it from the cells we have. This is, however, inefficient given we can just compute the values once. 
We make the cell type take that cache type optionally. This allows us to have `Cell {}` when we don't have a cache 
computed. This ensures we don't accidentally use cells where we haven't initialised the cache in the wrong context.

This type might be a little unusual if you are new to Elm. Note that `Cache` and `cache` are two different things. The 
closest analogy here is that `cache` is a generic type, and here we are using it kinda like a base type.

To be clear: `Cell {}` is `{ state : CellState, mined : Bool }` while `Cell Cache` is 
`{ neighbouringMines : Int, state : CellState, mined : Bool }`. If you don't quite get how those types wire up, it isn't 
too important.

### Board

    type alias Position =
        ( Int, Int )
    
    
    type alias Cells cache =
        Dict Position (Cell cache)
    
    
    type alias Board =
        { size : Int
        , mines : Int
        , cells : Cells Cache
        }
    
We store the size of the board and the number of mines in it. Currently these values are fixed, but this would make it
easier to allow customisation of the game in the future. The cells in the board are stored as a dictionary of 
position to the cells themselves, and we specify that they must have a cache when we have them in the board. 

### Top Level

We need some top-level data structures for Elm:

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

The model here stores the currently active game and the current state of the control key. Unlike in JavaScript, we 
can't just ask for the state arbitrarily as we are in a pure environment, so we have to store the value in our model and
update it through subscriptions.

The game can be loading or loaded. Why the loading step? See the section about generating the board below. The loading 
is essentially instant from the user perspective, but we simply don't have a board at the start of the game, so we need 
it as we can't block the main thread on getting it.

With a loaded game, we have the board, but also a game state. This is again essentially cached data to avoid having to 
work out if the game is over every time we construct our view.

## Algorithm

### Randomly Assign Mines

Randomness is inherently hard in a pure system. Fortunately, [`elm/random`][elm-random] exists for this task. We make 
a `Random.Generator` for the data we want. Making a random position for a mine is easy enough: we combine two random 
integers to make the tuple we want:

    randomPositionGenerator : Int -> Random.Generator Position
    randomPositionGenerator size =
        Random.map2 (\x -> \y -> ( x, y ))
            (Random.int 0 (size - 1))
            (Random.int 0 (size - 1))

We want several unique values, which sounds like a `Set`—while `elm/random` can't help us with that, 
[`elm-community/random-extra's Random.Set`][random-extra] can. 

    import Random.Set as Random

    randomMinePositionGenerator : Int -> Int -> Random.Generator (Set Position)
    randomMinePositionGenerator size mines =
        randomPositionGenerator size |> Random.set mines
        
Note this also solves a potential hang if the size and number of mines become customisable. `Random.set` will give up 
if it can't generate a new value in ten tries, meaning the number of mines can be lower than requested. This 
realistically won't happen unless you ask for more mines than there are potential positions on the board, which would 
result in an infinite loop if you didn't check and tried to generate the requested number of unique values. 

Now, we need to actually use these to construct a board, which involves going from a `Random.Generator` to an actual 
`Set Position`.

    newBoard : Cmd Msg
    newBoard =
        let
            toMsg =
                \mined -> GenerateBoard { size = boardSize, mined = mined }
        in
        Random.generate toMsg (randomMinePositionGenerator boardSize boardSize)

We do this with a command, `Random.generate`. This will later deliver us a message with the computed value, and we 
ask for a `GenerateBoard` message here. We use this in `init` so a board is generated when the page loads and when 
handling the `NewGame` message.

Generating the board itself happens when we receive this message (hence the need for the loading state):

    generateCell : Set Position -> Position -> ( Position, Cell {} )
    generateCell mined position =
        ( position
        , { state = Hidden { pushed = False }
          , mined = mined |> Set.member position
          }
        )
    
    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            -- ...
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
            
We first create a list of all possible positions in our board. We then generate a new cell for each of those positions.
As we have a set of all the mined positions, we can simply check for membership to determine if the cell is mined.

What isn't explained here is `computeCaches`, as our next section is…

### Calculate Neighbour Mine Count

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

With the split of the types to have both uncached and cached our goal here becomes populating that cache, and hence 
going from `Cell {}` to `Cell Cache`. `computeCaches` does this over the dictionary of cells.

`surrounding` is very similar to `getNeighbours` in the JavaScript version, but we have tuples which removes some of the
string hassle. We also don't bother doing any validation to see if the cells are valid, which may seem odd. The reason 
for this can be found in `minesCount` which takes a list of positions and returns the number of mined cells. We use 
`List.filterMap` and `Dict.get`, as we can follow the simple logic that if the position isn't in the `Dict`, it isn't 
valid.

### Opening A Cell
        
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
        
    applyToSurrounding : Board -> Position -> (Board -> Position -> Cell Cache -> Board) -> Board
    applyToSurrounding board position f =
        surrounding position
            |> List.filterMap (\pos -> Dict.get pos board.cells |> Maybe.map (\c -> ( pos, c )))
            |> List.foldl (\( pos, c ) -> \b -> f b pos c) board

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

Revealing one cell is easy enough—if it is hidden we can reveal it, otherwise we can't. For efficiency we also take a 
flag telling us if we made a change here. If we do make a change, we can then update the board with the updated 
cell. If the cell is mined, the user has lost, so we don't need to do any more work. If the cell has no neighbouring 
mines, we then automatically reveal its neighbours recursively. 

`applyToSurrounding` does the legwork here using `surrounding` again, and extracting cells (again, using 
`List.filterMap` so we ignore positions outside the board) to apply the function we give it. In this case, a recursive 
call. 

We have skipped a little of the wiring here though. We really start from a message we fire when the user clicks:
    
    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            -- ...
            Reveal pos ->
                let
                    op =
                        if model.controlPressed then
                            \b -> applyToSurrounding b pos reveal
    
                        else
                            \b -> Dict.get pos b.cells |> Maybe.map (reveal b pos) |> Maybe.withDefault b
                in
                changeBoard model op
                
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

So here we can see that the "opening neighbouring cells" case is handled at the same time. We can use 
`applyToSurrounding` here too which is nice. We use `changeBoard` to handle some things that we will repeat with other 
messages—making sure we have a loaded game to work in, and updating that game with the new value when we are done, 
including updating the game state—checking if the user has won (has revealed all non-mined cells and flagged all mined 
ones) or lost (has revealed a mine) after those changes.

### Flagging a Cell

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            -- ...
            ToggleFlag pos ->
                changeBoard model (atPosition pos toggleFlag)

    atPosition : Position -> (Cell Cache -> Cell Cache) -> Board -> Board
    atPosition position change board =
        { board | cells = board.cells |> Dict.update position (Maybe.map change) }

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

Here we reuse `changeBoard` and introduce another helper—`atPosition` which simply applies the given function to the 
cell at the given position.

### Starting a new game

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            NewGame ->
                ( { model | game = Loading }, newBoard )
                
This becomes nicely trivial in the Elm version. We don't have any disparate state to worry about, we just use the 
`newBoard` command from earlier and clear out the game state.

## Rendering

The view is independent in Elm, and here it becomes very simple, mostly just setting classes:

### Board

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

    onRightClick : msg -> Html.Attribute msg
    onRightClick msg =
        { message = msg
        , stopPropagation = True
        , preventDefault = True
        }
            |> Json.succeed
            |> HtmlE.custom "contextmenu"

Note that we use CSS grid for layout (which allows us to have the grid scale to the available space). We set the 
`grid-area` here to get the right layout of the cells.

Rather than `getNumberColor`, we just set a CSS class using the number of mines (prefixed with `m` because CSS 
selectors can't start with a number). We offload picking the colour to the CSS, as it is style. (In fact, the CSS just
contains an SVG background image which is where the colours actually are in this implementation).

We also set up the mouse events here. The right click requiring a custom event. We disable these if the game is over. 

### Flag/Mine Count

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

We also render the number of flags/mines left. We do this calculation in the view—a slightly more optimised version 
could cache the count instead.

## Conclusion

Elm allows you to forget about a lot of issues you have to worry about with JavaScript, and the compiler can do a lot 
of work you otherwise have to do. It makes it hard to do the wrong thing, which I have increasingly become a big fan of.

[elm-random]: https://package.elm-lang.org/packages/elm/random/latest/
[random-extra]: https://package.elm-lang.org/packages/elm-community/random-extra/latest/Random-Set
