open day22_try2
open day22_try2.QuadrantParser

let input = Input.readFile "/tmp/aoc/input"
// let input = Input.readFile "/tmp/aoc/input.open"
// let input = Input.readFile "/tmp/aoc/input.open"
// let input = Input.readFile "/tmp/aoc/input"
let quadrant = parseQuadrant input.Map

let cube = CubeAssembly.assembleCube quadrant
printfn $"cube: {cube.ToLongString()}"

let runActual () =
    let resultState = Walker.walkAlongCube cube input.Commands
    printfn $"result: {resultState}"
    let score = Score.score resultState
    printfn $"Final score: {score}"

let runTest () =
    let state = Walker.walkAlongCube cube []
    TestWalker.testAll state

runActual ()
// runTest ()
