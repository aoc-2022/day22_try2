module day22_try2.TestWalker

open day22_try2.BaseTypes
open day22_try2.Walker

let rec nsteps (state: WalkState) =
    if state.Commands.IsEmpty then
        state
    else
        step state |> nsteps


let testLocation (state: WalkState) (location: Location) =
    let commands = [ Steps 1; TurnLeft; TurnLeft; Steps 1; TurnLeft; TurnLeft ]
    let last = state.Cube.SideLength - 1

    let testState (pos: RPos) (pos2: RPos) (facing: Direction) =
        let ts1 = WalkState(state.Cube, commands, location, pos, facing)
        let ts2 = WalkState(state.Cube, commands, location, pos2, facing)
        if ts1.Side.Tiles.Contains ts1.Pos then ts1 else ts2

    let ts1 = testState (0, 2) (0, 3) West
    let ts2 = testState (last, 2) (last, 3) East
    let ts3 = testState (2, 0) (3, 0) North
    let ts4 = testState (2, last) (3, last) South
    let tsv = testState (1, 1) (2, 2) South

    let rs1 = nsteps ts1
    let rs2 = nsteps ts2
    let rs3 = nsteps ts3
    let rs4 = nsteps ts4
    let rsv = nsteps tsv

    let verifyPos (i: int) (res: WalkState) (input: WalkState) =
        if res.Pos <> input.Pos then
            let inOpen = input.Side.Tiles.Contains input.Pos
            printfn $"{location}:{i} res:{res.Location}:{res.Pos} <> input:{input.Location}{input.Pos} in={inOpen}"
        else
            printfn $"{location}:{i} OK"

    printfn "##########################################"
    verifyPos 1 rs1 ts1
    verifyPos 2 rs2 ts2
    verifyPos 3 rs3 ts3
    verifyPos 4 rs4 ts4
    verifyPos 5 rsv tsv
    printfn "##########################################"

let testEquator (state:WalkState) =
    let state = WalkState(state.Cube,[Steps (4*state.Cube.SideLength)],Front, (2,2),East)
    let final = nsteps state
    printfn $"- - - - -- testEquator from: {state.Location}:{state.Pos} to:{final.Location}:{final.Pos}"

let testNorth (state:WalkState) =
    let state = WalkState(state.Cube,[TurnLeft;Steps (4*state.Cube.SideLength)],Front, (2,2),East)
    let final = nsteps state
    printfn $"-- -- - -- testNorth from: {state.Location}{state.Pos} to:{final.Location}{final.Pos}"
 
let testSENorth (state:WalkState) =
    let state = WalkState(state.Cube,[Steps state.Cube.SideLength;TurnLeft;Steps (4*state.Cube.SideLength);TurnLeft;Steps state.Cube.SideLength],Front, (2,2),East)
    let final = nsteps state
    printfn $"i------ testSENorth from: {state.Location}{state.Pos} to:{final.Location}{final.Pos}"
 
 
let testCorner (state:WalkState) (location:Location) =
    let y1 = 5
    let x1 = 2
    let x2 = state.Cube.SideLength - 3
    let pos = (x2,y1)
    let stepN = Steps state.Cube.SideLength
    let commands = [stepN;TurnLeft;stepN;TurnLeft;stepN;TurnLeft]
    let state = WalkState(state.Cube,commands,location,pos,East)
    let state2 = nsteps state
    printfn $"testCorner(1): {state}"
    printfn $"testCorner(2): {state2}"
    
    
    
let testAll (state: WalkState) =
    let locations = state.Cube.Sides.Keys |> Seq.toList
    locations |> List.map (testLocation state)
    // testLocation state Top 
    // testEquator state
    // testNorth state
    // testSENorth state
    // testCorner state Front 