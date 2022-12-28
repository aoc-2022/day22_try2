open day22_try2
open day22_try2.QuadrantParser

let input = Input.readFile "/tmp/aoc/input.22.t"

printfn $"{input.Map} {input.Commands}"

let quadrant = QuadrantParser.parseQuadrant input.Map

CubeAssembly.assembleCube quadrant