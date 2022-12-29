module day22_try2.Walker

open day22_try2.Cube
open day22_try2.BaseTypes

exception NotImplemented of string

type WalkState(cube: Cube, commands: Command list, location: Location, pos: RPos, facing: Direction) =
    member this.Cube = cube
    member this.Commands = commands
    member this.Location = location
    member this.Facing = facing
    member this.Pos = pos
    member this.NE = cube.Sides[location].NE
    member this.Side = cube.Sides[location]

    override this.ToString() =
        let nextCommand = if commands.IsEmpty then "[]" else $"{commands.Head}"
        $"WalkState: at {location} {pos} facing: {facing} - nextCommand: {nextCommand}"

let turnRight (direction: Direction) =
    match direction with
    | East -> South
    | South -> West
    | West -> North
    | North -> East

let turnLeft (direction: Direction) =
    match direction with
    | East -> North
    | North -> West
    | West -> South
    | South -> East

let nextLocation (location: Location) (dir: Direction) : Location * Direction =
    match location, dir with
    | Front, North -> Top, South
    | Front, East -> Right, West
    | Front, West -> Left, East
    | Front, South -> Bottom, South
    | Top, South -> Front, North
    | Top, West -> Left, North
    | Top, East -> Right, North
    | Top, North -> Back, North
    | Bottom, North -> Back, South
    | Bottom, South -> Front, South
    | Bottom, West -> Left, South
    | Bottom, East -> Right, South
    | Back, North -> Top, North
    | Back, South -> Bottom, South
    | Back, West -> Left, East
    | Back, East -> Right, East
    | Left, North -> Top, West
    | Left, South -> Bottom, West
    | Left, West -> Front, West
    | Left, East -> Back, East
    | Right, North -> Top, East
    | Right, South -> Bottom, East
    | Right, West -> Front, East
    | Right, East -> Back, East

    | _ -> raise (NotImplemented $"{location},{dir}")
// rest is not implemented yet

let nextStepWithinBounds (state: WalkState) =
    let last = state.Cube.SideLength - 1

    match state.Pos with
    | 0, _ when state.Facing = West -> false
    | _, 0 when state.Facing = North -> false
    | (x, _) when x = last && state.Facing = East -> false
    | (_, y) when y = last && state.Facing = South -> false
    | _ -> true

let nextStep (state: WalkState) : RPos =
    match state.Pos, state.Facing with
    | (x, y), East -> (x + 1, y)
    | (x, y), West -> (x - 1, y)
    | (x, y), North -> (x, y - 1)
    | (x, y), South -> (x, y + 1)

let sideIsFlipped (ne: NorthEast) =
    match ne with
    | (North, West) -> true
    | (South, East) -> true
    | (East, North) -> true
    | (West, South) -> true
    | _ -> false

let getEdgePos (size: int) ((x, y): RPos) (dir: Direction) (ne: NorthEast) =
    printfn $"getEdgePos ({(x, y)} {dir} {ne} flipped={sideIsFlipped ne}"

    match dir with
    | North -> x
    | South -> x
    | East -> y
    | West -> y

let incomingPosDir (size: int) (edgePos: int) (inEdge: Direction) ((n, e): NorthEast) : Direction * RPos =
    let realEdge =
        match inEdge with
        | North -> n
        | South -> n |> oppositeDirection
        | East -> e
        | West -> e |> oppositeDirection

    let posDir =
        match inEdge with
        | North -> e
        | South -> e
        | East -> n
        | West -> n

    let pos =
        match realEdge, posDir with // this is likely all wrong
        | North, East -> (edgePos, 0)
        | North, West -> (size - 1 - edgePos, 0)
        | South, East -> (edgePos, size - 1)
        | South, West -> (size - 1 - edgePos, size - 1)
        | East, North -> (size - 1, size - 1 - edgePos)
        | East, South -> (size - 1, edgePos)
        | West, North -> (0, size - 1 - edgePos)
        | West, South -> (0, edgePos)

    (oppositeDirection realEdge), pos

let relativeFacing (facing: Direction) ((n, e): NorthEast) =
    match facing with
    | North -> n
    | South -> n |> oppositeDirection
    | East -> e
    | West -> e |> oppositeDirection

let shouldFlipEntryPoint (state: WalkState) (dest: Location) (entryEdge: Direction) =
    let fromNE = state.Side.NE
    let toNE = state.Cube.Sides[dest].NE
    let adjustedEntryEdge = 
        match entryEdge,toNE with
        | North,(n,_) -> n
        | South,(n,_) -> n |> oppositeDirection
        | East,(_,e) -> e
        | West,(_,e) -> e |> oppositeDirection
    let isDiagonalMove =
        match state.Facing,adjustedEntryEdge with
        | ns,ew when (ns = North || ns = South ) -> ew = East || ew = West
        | ew,ns when (ew = East || ew = West) -> ns = North || ns = South
    let flippedOut =
        match state.Facing,fromNE with
        | ns,(x,y) when (ns = North || ns = South) -> x = West || y = West
        | ew,(x,y) when (ew = East || ew = West) -> x = South || y = South
    let flippedIn =
        match entryEdge,fromNE with
        | ns,(x,y) when (ns = North || ns = South) -> x = West || y = West
        | ew,(x,y) when (ew = East || ew = West) -> x = South || y = South
    let flip = isDiagonalMove <> flippedOut <> flippedIn 
    printfn $"shouldFlipEntryPoint {fromNE}:{state.Facing}->{toNE}:{entryEdge}  {dest} :: flipped out={flippedOut} in={flippedIn} diag={isDiagonalMove} flip={flip} {adjustedEntryEdge}"
    flip

let private step (state: WalkState) =
    match state.Commands with
    | TurnRight :: rest ->
        printfn $"Turn right at {state.Location} {state.Pos}"
        WalkState(state.Cube, rest, state.Location, state.Pos, state.Facing |> turnRight)
    | TurnLeft :: rest ->
        printfn $"Turn left at {state.Location} {state.Pos}"
        WalkState(state.Cube, rest, state.Location, state.Pos, state.Facing |> turnLeft)
    | Steps 0 :: rest -> WalkState(state.Cube, rest, state.Location, state.Pos, state.Facing)
    | Steps n :: rest ->
        if nextStepWithinBounds state then
            let next: RPos = nextStep state
            // printfn $"safe: {state} step {n} -> {next}"

            if state.Cube.Sides[ state.Location ].Tiles.Contains next then
                printfn $"Move to {next} {state.Location}"
                WalkState(state.Cube, Steps(n - 1) :: rest, state.Location, next, state.Facing)
            else
                printfn $"blocked by {next} {state.Location}"
                WalkState(state.Cube, rest, state.Location, state.Pos, state.Facing)
        else
            let relFacing = relativeFacing state.Facing state.NE
            let newLoc, newEdge = nextLocation state.Location relFacing
            let edgePos = getEdgePos state.Cube.SideLength state.Pos state.Facing state.NE

            let flip = shouldFlipEntryPoint state newLoc newEdge

            let newDir, newPos =
                incomingPosDir state.Cube.SideLength edgePos newEdge (state.Cube.Sides[newLoc].NE)

            if state.Cube.Sides[ newLoc ].Tiles.Contains newPos then
                printfn $"Jumped to {newLoc} {newDir} {newPos}"
                WalkState(state.Cube, Steps(n - 1) :: rest, newLoc, newPos, newDir)
            else
                printfn $"Blocked when trying to enter {newLoc} from {state}"
                WalkState(state.Cube, rest, state.Location, state.Pos, state.Facing)

let walkAlongCube (cube: Cube) (commands: Command list) =
    let state = WalkState(cube, commands, Front, (0, 0), East)

    let rec nsteps (n: int) (state: WalkState) =
        if n = 0 then state else step state |> nsteps (n - 1)

    printfn $"State(1) {state}"
    let state = nsteps 38 state
    printfn $"State({8}) {state}"
    1
