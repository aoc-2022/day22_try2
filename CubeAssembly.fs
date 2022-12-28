module day22_try2.CubeAssembly

open day22_try2.BaseTypes
open day22_try2.Cube

let private findStartSide(quadrant: Quadrant) : RPos =
    let isTopRow (x,y) = y = 0
    let pos = quadrant.Map.Keys |> Seq.filter isTopRow |> Seq.min
    pos 

type CubeMap = Map<Location, QPos*Map<Direction,QPos*Direction>>

let addSideIfPossible (cube:Cube) (quadrant:Quadrant) (location:Location) (pos:QPos) (ne:NorthEast) =
    let side = cube.Side location
    if side.IsNone && quadrant.Map.ContainsKey pos then
        let side = Side(pos,quadrant.Map[pos],location,Map.empty,ne)
        printfn $"Adding {side}"
        cube.AddSide (location,side) 
    else cube 
        
let adjustedLeft ((x,y):QPos) (nw:NorthEast) =
    match nw with
    | (North,East) -> (x+1,y)
    | (South,East) -> (x+1,y)
    | (North,West) -> (x+1,y)

let stepDir (pos:QPos) (dir:Direction) =
    match pos,dir with
    | (x,y), East -> x+1,y
    | (x,y), West -> x-1,y
    | (x,y), North -> x,y-1
    | (x,y), South -> x,y+1

let rec expand (cube:Cube) (quadrant:Quadrant) =
    let rec expand1 (cube:Cube) (locations:Location list) : Cube =
        match locations with
        | [] -> cube
        | Front::rest -> // front is always correctly aligned
            let front: Side = cube.Sides[Front]
            let (x,y) = front.Pos 
            let cube : Cube = addSideIfPossible cube quadrant Right (x+1,y) (North,East)
            let cube : Cube = addSideIfPossible cube quadrant Bottom (x,y+1) (South,East)
            expand1 cube rest
        | Top::rest ->
            let top: Side = cube.Sides[Top]
            let pos = top.Pos
            let left : QPos = snd top.NE |> oppositeDirection |> stepDir pos 
            let right : QPos = snd top.NE |> stepDir pos 
            let north : QPos = fst top.NE |> stepDir pos 
            let south : QPos = fst top.NE |> oppositeDirection |> stepDir pos
            printfn $"Top: {pos} {top.NE} left={left} right={right} north={north} south={south}"
            let cube = addSideIfPossible cube quadrant Back north (fst top.NE |> oppositeDirection,snd top.NE)
            let cube = addSideIfPossible cube quadrant Left left (snd top.NE,fst top.NE)
            let cube = addSideIfPossible cube quadrant Right right (snd top.NE |> oppositeDirection,fst top.NE)
            expand1 cube rest 
        | Bottom::rest ->
            let bottom: Side = cube.Sides[Bottom]
            let pos = bottom.Pos
            let left = snd bottom.NE |> oppositeDirection |> stepDir pos 
            let right = snd bottom.NE |> stepDir pos 
            let front = fst bottom.NE |> oppositeDirection |> stepDir pos 
            let back = fst bottom.NE |> stepDir pos
            printfn $"bottom: left={left} right={right} front={front} back={back}"
            let cube = addSideIfPossible cube quadrant Left left (snd bottom.NE |> oppositeDirection,fst bottom.NE)
            let cube = addSideIfPossible cube quadrant Right right (snd bottom.NE, fst bottom.NE)
            let cube = addSideIfPossible cube quadrant Back back bottom.NE 
            expand1 cube rest
        | Back::rest ->
            let back: Side = cube.Sides[Back]
            let pos = back.Pos
            let left = snd back.NE |> oppositeDirection |> stepDir pos 
            let right = snd back.NE |> stepDir pos 
            let north = fst back.NE |> stepDir pos 
            let south = fst back.NE |> oppositeDirection |> stepDir pos
            printfn $"back: {pos} {back.NE} left={left} right={right} north={north} south={south}"
            let cube = addSideIfPossible cube quadrant Right right (fst back.NE, snd back.NE |> oppositeDirection)
            let cube = addSideIfPossible cube quadrant Left left back.NE 
            let cube = addSideIfPossible cube quadrant Top north (fst back.NE |> oppositeDirection, snd back.NE |> oppositeDirection)
            let cube = addSideIfPossible cube quadrant Bottom south back.NE
            expand1 cube rest 
        | Left :: rest ->
            let left: Side = cube.Sides[Left]
            let pos = left.Pos
            let leftDir = snd left.NE |> oppositeDirection |> stepDir pos
            let right = snd left.NE |> stepDir pos
            let north = fst left.NE |> stepDir pos 
            let south = fst left.NE |> oppositeDirection |> stepDir pos
            printfn $"Left: {pos} {left.NE} left={leftDir} right={right} north={north} south={south}"
            let cube = addSideIfPossible cube quadrant Back leftDir left.NE
            let cube = addSideIfPossible cube quadrant Front right (fst left.NE, snd left.NE |> oppositeDirection)
            let cube = addSideIfPossible cube quadrant Top north (snd left.NE, fst left.NE)
            let cube = addSideIfPossible cube quadrant Bottom south (snd left.NE, fst left.NE |> oppositeDirection)
            expand1 cube rest
        | Right :: rest ->
            let right: Side = cube.Sides[Right]
            let pos = right.Pos
            let left = snd right.NE |> oppositeDirection |> stepDir pos
            let rightDir = snd right.NE |> stepDir pos
            let north = fst right.NE |> stepDir pos 
            let south = fst right.NE |> oppositeDirection |> stepDir pos
            let cube = addSideIfPossible cube quadrant Back rightDir (fst right.NE, snd right.NE |> oppositeDirection)
            let cube = addSideIfPossible cube quadrant Front left right.NE 
            let cube = addSideIfPossible cube quadrant Top north (snd right.NE,fst right.NE |> oppositeDirection)
            let cube = addSideIfPossible cube quadrant Bottom south (snd right.NE, fst right.NE)
            expand1 cube rest  
    expand1 cube (cube.Sides.Keys |> Seq.toList) 
   
let assembleCube (quadrant:Quadrant) =
    let frontPos = findStartSide quadrant
    let tiles: Grid = quadrant.Map[frontPos]
    let frontSide = Side(frontPos,tiles,Front,Map.empty,(North,East))
    let cube = Cube.init frontSide
    let rec expandCube (cube:Cube) =
        expand cube quadrant
    let cube = expandCube cube
    let cube = expandCube cube
    let cube = expandCube cube
    let cube = expandCube cube
    let cube = expandCube cube 
    cube 