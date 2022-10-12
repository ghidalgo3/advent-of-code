module Intcode

open Numbers

type Program = {
    Memory : int array
    PC : int
    Output : int
    Input : int}

type Parameter = 
| Position of int
| Immediate of int

type Instruction =
| Add of Parameter * Parameter * Parameter
| Mult of Parameter * Parameter * Parameter
| Input of Parameter
| Output of Parameter
| JumpIfTrue of Parameter * Parameter
| JumpIfFalse of Parameter * Parameter
| LessThan of Parameter * Parameter * Parameter
| Equals of Parameter * Parameter * Parameter
| Halt 
| Unknown of int list

let decode program =
    let instruction = (digits >> padZeros 5) program.Memory.[program.PC]

    // printfn $"Decoding address {program.PC} as {instruction} {instruction[3]} {instruction[4]}"
    let parameter mode arg = 
        match mode with 
        | 0 -> Position(arg)
        | 1 -> Immediate(arg)

    match instruction with
    | [p3; p2; p1; 0; 1] ->
        // p1 // p2 then p3
        Add(
            parameter p1 program.Memory.[program.PC + 1],
            parameter p2 program.Memory.[program.PC + 2],
            parameter 1  program.Memory.[program.PC + 3])
    | [p3; p2; p1; 0; 2] ->
        Mult(
            parameter p1 program.Memory.[program.PC + 1],
            parameter p2 program.Memory.[program.PC + 2],
            parameter 1  program.Memory.[program.PC + 3])
    | [p3; p2; p1; 0; 3] ->
        Input(
            parameter 1 program.Memory[program.PC + 1])
    | [p3; p2; p1; 0; 4] ->
        Output(
            parameter p1 program.Memory[program.PC + 1])
    | [p3; p2; p1; 0; 5] ->
        JumpIfTrue(
            parameter p1 program.Memory[program.PC + 1],
            parameter p2 program.Memory[program.PC + 2])
    | [p3; p2; p1; 0; 6] ->
        JumpIfFalse(
            parameter p1 program.Memory[program.PC + 1],
            parameter p2 program.Memory[program.PC + 2])
    | [p3; p2; p1; 0; 7] ->
        LessThan(
            parameter p1 program.Memory.[program.PC + 1],
            parameter p2 program.Memory.[program.PC + 2],
            parameter 1  program.Memory.[program.PC + 3])
    | [p3; p2; p1; 0; 8] ->
        Equals(
            parameter p1 program.Memory.[program.PC + 1],
            parameter p2 program.Memory.[program.PC + 2],
            parameter 1  program.Memory.[program.PC + 3])
    | [p1; p2; p3; 9; 9] -> Halt
    | unknown -> Unknown(unknown)

let derefParameter program parameter = 
    match parameter with 
    | Immediate(n) -> n
    | Position(a) -> program.Memory[a]

let eval p =
    let program = { p with Memory = Array.copy p.Memory } 
    let deref parameter = derefParameter program parameter
    // printfn $"Begin decoding"
    // printfn $"{program.PC} : {decode program}"
    match decode program with
    | Add(p0, p1, p2) ->
        // printfn $"mem[{deref p2}] = {deref p0} + {deref p1}"
        program.Memory.[deref p2] <- deref p0 + deref p1
        { program with PC = program.PC + 4 }
    | Mult(p0, p1, p2) ->
        program.Memory.[deref p2] <- deref p0 * deref p1
        { program with PC = program.PC + 4 }
    | Input(addr) ->
        program.Memory.[deref addr] <- program.Input
        { program with PC = program.PC + 2 }
    | Output(addr) ->
        // printfn $"Outputing {addr} which is {deref addr}"
        // printfn $"{program}"
        { program with PC = program.PC + 2 ; Output = deref addr}
    | JumpIfTrue(argument, jumpAddress) ->
        if ((deref argument) <> 0) then
            { program with PC = deref jumpAddress }
        else 
            { program with PC = program.PC + 3}
    | JumpIfFalse(argument, jumpAddress) ->
        if ((deref argument) = 0) then
            { program with PC = deref jumpAddress }
        else 
            { program with PC = program.PC + 3}
    | LessThan(arg1, arg2, arg3) ->
        if ((deref arg1) < (deref arg2)) then
            program.Memory[deref arg3] <- 1
            // printfn $"mem[{deref arg3}] <- 1"
        else 
            program.Memory[deref arg3] <- 0
        { program with PC = program.PC + 4}
    | Equals(arg1, arg2, arg3) ->
        if ((deref arg1) = (deref arg2)) then
            program.Memory[deref arg3] <- 1
            // printfn $"mem[{deref arg3}] <- 1"
        else
            program.Memory[deref arg3] <- 0
        { program with PC = program.PC + 4}
    | _ -> program

let rec run program =
    match decode program with 
    | Halt -> program
    | _ -> (eval >> run) program

let allPairs xs =
    seq {
        for x in xs do
            for y in xs do
                yield (x, y)
    }