// Joseph Madden
module TicTac
open System

// states are 2D arrays of integers (3x3)
// 0 means not played
// 1 means played by max player (the human player) - displayed as an X
// -1 means played by min player (the computer) - displayed as an O

type State = int [,]

let start = Array2D.zeroCreate<int> 3 3  // all 0's


let sameState (s1:State) (s2:State) = 
    let mutable same = true
    
    for r in 0..2 do
        for c in 0..2 do
            if s1.[r,c] <> s2.[r,c] then
                same <- false
    same

let copyState (s:State) = 
    let newState = Array2D.zeroCreate<int> 3 3

    for r in 0..2 do 
        for c in 0..2 do 
            newState.[r,c] <- s.[r,c]
    newState

let action row col player state = 
    let newState = copyState state
    if player = "Max" then
        newState.[row,col] <- 1
    else
        newState.[row,col] <- -1
    newState

// "actions" - there's only one; choosing a row and column
let actions = [|action|]
let names = [|"action"|]

type MatrixGame = { Start : State;
                    CurrentState: State; 
                    IsOver : State -> bool; 
                    Actions : array<int -> int-> string -> State -> State>; 
                    Names : array<string>;
                    Utility : State -> string -> double; 
                    Rows : int;
                    Cols : int }

let makeMove row col player actionnum game =
    let newgame = { game with CurrentState = game.Actions.[actionnum] row col player game.CurrentState }
    newgame
    
// helper function used by gameOver
// returns whether the game is such that player has already won
let wonBy player (state:State) =  // use 1 for max player, -1 for min player
    let mutable isWon = false
    
    for r in 0..2 do
        if state.[r,0] = player && state.[r,1] = player && state.[r,2] = player then // check horizontal
             isWon <- true
    for c in 0..2 do
        if state.[0,c] = player && state.[1,c] = player && state.[2,c] = player then // check vertical
            isWon <- true
    if state.[0,0] = player && state.[1,1] = player && state.[2,2] = player then  // check diagonal 1
        isWon <- true
    if state.[2,0] = player && state.[1,1] = player && state.[0,2] = player then // check diagonal 2
        isWon <- true
    isWon

let gameOver (state:State) =
    if wonBy 1 state then
        true
    elif wonBy -1 state then
        true
    else
        let mutable spacesLeft = 0
        
        for r in 0..2 do
            for c in 0..2 do 
                if state.[r,c] = 0 then
                    spacesLeft <- spacesLeft + 1
     
        spacesLeft = 0

let utility state player =   // only use if gameOver is true

    if wonBy 1 state then 
        1.0
    elif wonBy -1 state then
        -1.0
    else 
        0.0

let game = {    Start = start;
                CurrentState = start;
                IsOver = gameOver;
                Actions = actions;
                Names = names;
                Utility = utility;
                Rows = 3;
                Cols = 3}


let expand (state:State) game player = [ for action in game.Actions do
                                            for row in 0..game.Rows-1 do
                                                for col in 0..game.Cols-1 do
                                                    if state.[row,col]=0 then
                                                        yield action row col player state ]
    
let rec minimaxValue game state player =
    let mutable value = 0.0
    if game.IsOver state then
        game.Utility state player
    elif player = "Max" then
        let children = expand state game "Max"
        [ for child in children -> minimaxValue game child "Min" ] |> List.max
    else
        let children = expand state game "Min"
        [ for child in children -> minimaxValue game child "Max" ] |> List.min


let minimaxDecision game (state:State) playerString =
    let mutable children = []
    if playerString = "Max" then
        for actNum in 0..game.Actions.Length-1 do
            for row in 0..game.Rows-1 do
                for col in 0..game.Cols-1 do
                    if state.[row,col] = 0 then
                        let child = game.Actions.[actNum] row col "Max" state        
                        children <- (actNum, row, col, minimaxValue game child "Min" ) :: children 
        children |> List.maxBy (fun (x,y,z,w) -> w)
    else
        for actNum in 0..game.Actions.Length-1 do
            for row in 0..game.Rows-1 do
                for col in 0..game.Cols-1 do
                    if state.[row,col] = 0 then
                        let child = game.Actions.[actNum] row col "Min" state        
                        children <- (actNum, row, col, minimaxValue game child "Max" ) :: children 
        children |> List.minBy (fun (x,y,z,w) -> w)

let chooseMove game = 
    let state = game.CurrentState
    let (move,row,col) = minimaxDecision game state "Min" |> (fun (x,y,z,w) -> (x,y,z))
    let newState = game.Actions.[move] row col "Min" state
    let newgame = { game with CurrentState = newState }
    newgame
