module day22_try2.Walker

open day22_try2.Cube
open day22_try2.BaseTypes

type WalkState(cube: Cube, commands: Command list, location: Location, pos: RPos, facing: Direction) =
    member this.Cube = cube
    member this.Commands = commands
    member this.Location = location
    member this.Facing = facing
    member this.Pos = pos

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
    
let nextLocation (location:Location) (dir:Direction) : Location*Direction =
    match location,dir with
    | Front,North -> Top,South 
    | Front,East -> Right,West 
    | Front,West -> Left,East
    | Front,South -> Bottom,South
    | Top,South -> Front,North
    | Top,West -> Left,North
    | Top,East -> Right,North
    | Top,North -> Back,North
    // rest is not implemented yet 

let nextStepWithinBounds (state: WalkState) =
    let last = state.Cube.SideLength-1

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

let getEdgePos ((x,y):RPos) (dir:Direction) =
    match dir with
    | North -> x
    | South -> x
    | East -> y
    | West -> y

let incomingPos (size:int) (edgePos:int) (inDir:Direction) ((n,e):NorthEast) =
    let realDir =
        match inDir with
        | North -> n
        | South -> n |> oppositeDirection
        | East -> e
        | West -> e |> oppositeDirection
    let posDir =
        match inDir with
        | North -> e
        | South -> e
        | East -> n
        | West -> n
    match realDir,posDir with // this is all wrong
        | North,East -> (edgePos,0)
        | North,West -> (size - 1 - edgePos,0)
        | South,East -> (edgePos,size-1)
        | South,West -> (size - 1 - edgePos,size - 1)
        | East,North -> (0,size - 1 - edgePos)
        | East,South -> (0,edgePos)
        | West,North -> (size - 1,size - 1 - edgePos)
        
        
        
        

let nextRPos (pos:RPos) (fromDir:Direction) (toDir:Direction) (toNE:NorthEast) = 1
    

let private step (state: WalkState) =
    match state.Commands with
    | TurnRight :: rest -> WalkState(state.Cube, rest, state.Location, state.Pos, state.Facing |> turnRight)
    | TurnLeft :: rest -> WalkState(state.Cube, rest, state.Location, state.Pos, state.Facing |> turnLeft)
    | Steps 0 :: rest -> WalkState(state.Cube, rest, state.Location, state.Pos, state.Facing)
    | Steps n :: rest ->
        if nextStepWithinBounds state then
            let next : RPos = nextStep state
            printfn $"safe: {state} step {n} -> {next}"
            if state.Cube.Sides[state.Location].Tiles.Contains next then
                WalkState(state.Cube, Steps(n-1)::rest,state.Location,next,state.Facing)
            else
               printfn $"blocker at {state.Pos} throwing away Steps {n}"
               WalkState(state.Cube, rest, state.Location, state.Pos, state.Facing)  
        else
            let (newLoc,newDir) = nextLocation state.Location state.Facing
            printfn $"unsafe: {state} next={newLoc},{newDir}"
            state

let walkAlongCube (cube: Cube) (commands: Command list) =
    let state = WalkState(cube, commands, Front, (0, 0), East)
    let rec nsteps (n:int) (state:WalkState) =
        if n = 0 then state
        else
            step state |> nsteps (n - 1)
    printfn $"State(1) {state}"
    let state = nsteps 18 state 
    printfn $"State({8}) {state}"
    1
