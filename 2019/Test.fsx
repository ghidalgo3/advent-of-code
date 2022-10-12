#load "Numbers.fs"
#load "Intcode.fs"
open Intcode
open System.IO

let programs = [
    {|Name= "Test equals 8"; Input = "3,9,8,9,10,9,4,9,99,-1,8" ; Test = (fun x -> if (x = 8) then 1 else 0)|}
    {|Name= "Test less than 8"; Input = "3,9,7,9,10,9,4,9,99,-1,8" ; Test = (fun x -> if (x < 8) then 1 else 0)|}
    {|Name= "Test equals 8"; Input = "3,3,1108,-1,8,3,4,3,99" ; Test = (fun x -> if (x = 8) then 1 else 0)|}
    {|Name= "Test less than 8"; Input = "3,3,1107,-1,8,3,4,3,99" ; Test = (fun x -> if (x < 8) then 1 else 0)|}
]
// for p in programs do
//     let pp = {
//         Memory = p.Input.Split(",") |> Array.map int
//         PC = 0
//         Output = 0
//         Input = 8 }
//     let terminalState = run pp
//     printfn $"{p.Name}, Output = {terminalState.Output}, should be {p.Test 8}"

let input = [| for n in File.ReadAllText("input/5-simple").Split(",") -> int n |]
let mutable program = { Memory = input ; PC = 0 ; Output = 0; Input = 9}
let isBranchTaken program =
    match decode program with
    | JumpIfTrue(argument, jumpAddress) -> $"{derefParameter program argument} != 0 ? GOTO: {derefParameter program jumpAddress}"
    | JumpIfFalse(argument, jumpAddress) -> $"{derefParameter program argument} == 0 ? GOTO: {derefParameter program jumpAddress}"
    | LessThan(arg1, arg2, arg3) -> $"{derefParameter program arg1} < {derefParameter program arg2} ? memory[{derefParameter program arg3}] <- 1"
    | Equals(arg1, arg2, arg3) -> $"{derefParameter program arg1} == {derefParameter program arg2} ? memory[{derefParameter program arg3}] <- 1"
    | _ -> ""
for i in 0 .. 20 do
    printfn $"{program.PC}: {decode program} {isBranchTaken program} Output: {program.Output}"
    program <- eval program