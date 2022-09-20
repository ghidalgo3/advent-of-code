﻿open System.IO
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


let one : int = day1 "input/1"
let two : int = day2 "input/2"
printfn $"Day 1-2: {one}" 
printfn $"Day 2-1: {two}" 
