module day22_try2.Input

open System.IO
open day22_try2.BaseTypes

let parseCommands (commands: string) =
    let commands = commands.ToCharArray() |> Array.toList
    let toDigit (c: char) = c - '0' |> int

    let rec parse (curr: int) (commands: char list) =
        match commands with
        | [] -> if curr = 0 then [] else [Steps curr]
        | 'L' :: rest ->
            let tail = TurnLeft :: parse 0 rest
            if curr > 0 then Steps curr :: tail else tail
        | 'R' :: rest ->
            let tail = TurnRight :: parse 0 rest
            if curr > 0 then Steps curr :: tail else tail
        | c :: rest ->
            let curr = curr * 10 + (toDigit c)
            parse curr rest

    parse 0 commands

type Input(map: string list, commands: Command list) =
    member this.Map = map
    member this.Commands = commands


let readFile (fileName: string) : Input =
    let lines = File.ReadAllLines fileName |> Seq.toList
    let map = lines[0 .. lines.Length - 3]
    let commands = lines[lines.Length - 1]
    let commands = parseCommands commands
    Input(map, commands)