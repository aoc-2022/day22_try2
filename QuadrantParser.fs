module day22_try2.QuadrantParser

open day22_try2.Utils 
open day22_try2.BaseTypes

let private findSideLength (lines:string list) : int = 
    let height = lines.Length
    let width = lines |> List.map String.length |> List.max
    let size : int = gcd width height
    let size = if (width/size) * (height/size) < 9 then size / 2 else size
    printfn $"parseCube.findSizeLength: width,height = {width},{height} {size}"
    size 

let mapToCoordinates (side: string list) : Option<Set<int*int>> =
    if side[0][0] = ' ' then None
    else
        let side = side |> List.map (fun s -> s.ToCharArray () |> Array.toList |> List.indexed) 
        let side = side |> List.indexed
        let side = side |> List.collect (fun (y,chars) -> chars |> List.map (fun (x,c) -> (x,y),c))
        let side = side |> List.filter (fun xyc -> snd xyc = '.') |> List.map fst 
        let side = side |> Set.ofList 
        printfn $"mapToCoordinates: side={side}"
        Some(side)

let private splitSides (size:int) (input:string list) =
    let rows = input |> List.chunkBySize size
    let rec split (row:string list) =
        if row[0].Length = 0 then []
        else
            let curr = row |> List.map (fun s -> s[0 .. size-1])
            let rest = row |> List.map (fun s -> s[size .. s.Length - 1])
            curr :: (split rest)
    let rows = rows |> List.map split
    rows 

let addQuadrant (sides: Option<Set<int*int>> list list) =
    let sides = sides |> List.indexed
    sides |> List.collect (fun (y,row) -> row |> List.indexed |> List.map (fun (x,pos) -> (x,y),pos))

let toQuadrantMap (sides: ((int*int)*Option<Set<int*int>>) list) : Map<int*int,Set<int*int>> =
    let sideExists (_,page) = page |> Option.isSome
    let deOption (pos,Some(x)) = (pos,x) 
    sides |> List.filter sideExists
          |> List.map deOption
          |> Map.ofList 

let parseQuadrant (lines: string list) =
    let size = findSideLength lines 
    let sides = splitSides size lines
    let sides = sides |> List.map (fun row -> row |> List.map mapToCoordinates)
    let sides = addQuadrant sides
    let map = toQuadrantMap sides 
    printfn $"parseQuadrant: {map}"
    Quadrant(map)

    