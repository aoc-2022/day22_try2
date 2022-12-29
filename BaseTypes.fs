module day22_try2.BaseTypes

type Command =
    | TurnLeft
    | TurnRight
    | Steps of int

type Location =
    | Top
    | Bottom
    | Front
    | Back
    | Left
    | Right

type Direction =
    | East
    | West
    | North
    | South

let oppositeDirection (dir: Direction) =
    match dir with
    | North -> South
    | South -> North
    | East -> West
    | West -> East

let s2eRot (dir: Direction) =
    match dir with
    | South -> East
    | East -> North
    | North -> West
    | West -> South

let s2wRot (dir: Direction) =
    match dir with
    | South -> West
    | West -> North
    | North -> East
    | East -> South

type Offset = int * int
type RPos = int * int
type QPos = int * int
type Grid = Set<RPos>

type Quadrant(map: Map<QPos, Grid>, sideLength: int) =
    member this.Map = map
    member this.SideLength = sideLength

    override this.ToString() =
        let s = map.Keys |> Seq.map (fun s -> $"{s}") |> String.concat " "
        $"Quadrant({s} side:{sideLength})"
