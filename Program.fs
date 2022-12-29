open day22_try2
open day22_try2.QuadrantParser

let input = Input.readFile "/tmp/aoc/input.22.t"
// let input = Input.readFile "/tmp/aoc/input"
let quadrant = parseQuadrant input.Map

let cube = CubeAssembly.assembleCube quadrant
printfn $"cube: {cube.ToLongString()}"

let resultState = Walker.walkAlongCube cube input.Commands
printfn $"result: {resultState}"
let score = Score.score resultState

printfn $"Final score: {score}"