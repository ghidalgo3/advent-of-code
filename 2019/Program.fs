open System.IO
open Intcode

let rec fuelRequirement mass =
    let req = mass / 3 - 2
    if req > 0 then
        req + fuelRequirement req
    else 
        0

let day1 (input : string) : int =
    let input = [
        for line in File.ReadLines(input) ->
            line |> int |> fuelRequirement ]
    input |> List.sum

let day2 input =
    let input = [| for n in File.ReadAllText(input).Split(",") -> int n |]
    input.[1] <- 12
    input.[2] <- 2
    let program = { Memory = input ; PC = 0 }
    let finalState = run program
    finalState.Memory.[0]

let day2s input =
    let input = [| for n in File.ReadAllText(input).Split(",") -> int n |]
    let allPrograms = [
        for (noun, verb) in allPairs [ 0 .. 99] ->
            let mem = Array.copy input
            mem.[1] <- noun
            mem.[2] <- verb
            let program = { Memory = mem ; PC = 0 }
            // an anonymous record! like a C# anonymous type
            {| P = run program ; Noun = noun ; Verb = verb |}
            // printfn $"p({noun}, {verb}) = {f.Memory.[0]}"
    ]
    let finalProgram = 
        allPrograms
        |> List.filter (fun x -> x.P.Memory.[0] = 19690720)
        |> List.exactlyOne
    100 * finalProgram.Noun + finalProgram.Verb



let one : int = day1 "input/1"
printfn $"Day 1-2: {one}" 
let two : int = day2 "input/2"
printfn $"Day 2-1: {two}" 
let twos : int = day2s "input/2"
printfn $"Day 2-2: {twos}" 
