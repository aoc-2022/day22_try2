module day22_try2.Score

open Cube 
open Walker
open BaseTypes 

let private dirScore (dir:Direction) =
    match dir with
    | East -> 0
    | South -> 1
    | West -> 2
    | North -> 3

let private calculateScore(x:int,y:int,dir:Direction) =
    let x = x * 4
    let y = y * 1000
    let dir = dirScore dir
    x + y + dir 

let score(state:WalkState) : int =
    let size = state.Cube.SideLength
    let side = state.Side
    let pos = state.Pos
    printfn $"Score {size} {side} {pos}"
    let xOffset = (side.Pos |> fst) * size + 1
    let yOffset = (side.Pos |> snd) * size + 1
    let x = (fst pos) + xOffset
    let y = (snd pos) + yOffset
    let dir = state.Facing
    let score = calculateScore(x,y,dir)
    printfn $"Score x={x} y={y} dir={dir} dirScore{dirScore dir} score={score}"
    score

