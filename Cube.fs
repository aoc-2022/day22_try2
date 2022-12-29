module day22_try2.Cube

open day22_try2.BaseTypes

type Edge = Direction * Direction * QPos

type NorthEast = Direction * Direction

type Side(pos: QPos, tiles: Grid, location: Location, borders: Map<Direction, Edge>, ne: NorthEast) =
    member this.Pos = pos
    member this.Tiles = tiles
    member this.Location = location
    member this.Borders = borders
    member this.NE = ne
    member this.TryBorder(direction: Direction) = borders.TryFind direction
    member this.Border(direction: Direction) = borders[direction]
    override this.ToString() = $"Side({pos} {location} NE={ne}"

//
//     ,c---,d     front=ab left=ac right=bd back=cd
//    a----b |     back top=cd bottom top=hg   (northEast for front = North, East (e->a, a->b)
//    |(h) | g
//    e----f´
//

type Cube(sides: Map<Location, Side>, sideLength: int) =
    member this.Sides = sides
    member this.Side(location: Location) = sides.TryFind location
    member this.SideLength = sideLength

    member this.AddSide(location: Location, side: Side) =
        Cube(sides.Add(location, side), sideLength)

    member this.HasAllSides = sides.Count = 6

    override this.ToString() =
        let s = sides.Keys |> Seq.map (fun s -> $"{s}") |> String.concat " "
        $"Cube({s})"

    member this.ToLongString() =
        let to_s (side: Side) =
            $"{side.Location}:{side.Pos} NE={side.NE}"

        let ss = sides.Values |> Seq.map to_s |> String.concat "\n"
        $"Cube:\n{ss}\n"

    static member init (front: Side) (sideLength: int) =
        Cube(Map.empty.Add(Front, front), sideLength)
