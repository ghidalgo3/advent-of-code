open System.IO
open Intcode
open Wires
open Numbers

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
    let program = { Memory = input ; PC = 0 ; Output = 0}
    let finalState = run program
    finalState.Memory.[0]

let day2s input =
    let input = [| for n in File.ReadAllText(input).Split(",") -> int n |]
    let allPrograms = [
        for (noun, verb) in allPairs [ 0 .. 99] ->
            let mem = Array.copy input
            mem.[1] <- noun
            mem.[2] <- verb
            let program = { Memory = mem ; PC = 0 ; Output = 0}
            // an anonymous record! like a C# anonymous type
            {| P = run program ; Noun = noun ; Verb = verb |}
            // printfn $"p({noun}, {verb}) = {f.Memory.[0]}"
    ]
    let finalProgram = 
        allPrograms
        |> List.filter (fun x -> x.P.Memory.[0] = 19690720)
        |> List.exactlyOne
    100 * finalProgram.Noun + finalProgram.Verb

let day3 input =
    let lines = File.ReadAllLines(input)
    let w1 = parseWire lines[0]
    let w2 = parseWire lines[1]
    [ for p in intersections w1 w2 -> manhattanLength p ]
    |> List.filter (fun x -> x > 0)
    |> List.min

let day3s input =
    let lines = File.ReadAllLines(input)
    let w1 = parseWire lines[0]
    let w2 = parseWire lines[1]
    intersections w1 w2
    |> List.filter (fun x -> (manhattanLength x) > 0)
    |> List.map (fun p ->
        // printfn $"lengthAlongWire of {p}"
        let first = lengthAlongWire w1 p
        // printfn $"First {first}"
        let second = lengthAlongWire w2 p
        // printfn $"Second {second}"
        let r = (first, second)
        r)
    |> List.map (fun (uno, dos) -> uno + dos)
    |> List.min

let day4 min max =
    let notAllUnique hist = Map.exists (fun d f -> f > 1) hist
    let requirements = [ isNonDecreasing ; strangeCriteria ; (digits >> freq >> notAllUnique) ]
    let applyRequirements booleanFunctions n =
        List.fold (fun accum f -> accum && f n) true booleanFunctions
    [ for n in min .. max -> applyRequirements requirements n ]
    |> List.filter (fun b -> b)
    |> List.length

let day5 input =
    let input = [| for n in File.ReadAllText("input/5").Split(",") -> int n |]
    let mutable program = { Memory = input ; PC = 0 ; Output = 0}
    let finalState = run program
    finalState.Output

let one : int = day1 "input/1"
printfn $"Day 1-2: {one}" 
let two : int = day2 "input/2"
printfn $"Day 2-1: {two}" 
let twos : int = day2s "input/2"
printfn $"Day 2-2: {twos}" 
let three : int = day3 "input/3"
printfn $"Day 3: {three}" 
let threes : int = day3s "input/3"
printfn $"Day 3-2: {threes}" 
printfn $"Day 4: {day4 138307 654504}"
let five : int = day5 "input/5"
printfn $"Day 5: {five}"
