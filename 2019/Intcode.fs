module Intcode

type Program = {
    Memory : int array
    PC : int }

type Instruction =
| Add of int * int * int
| Mult of int * int * int
| Halt
| Unknown

let decode program =
    match program.Memory.[program.PC] with
    | 1 -> Add(program.Memory.[program.PC + 1], program.Memory.[program.PC + 2], program.Memory.[program.PC + 3])
    | 2 -> Mult(program.Memory.[program.PC + 1], program.Memory.[program.PC + 2], program.Memory.[program.PC + 3])
    | 99 -> Halt
    | _ -> Unknown

let eval p =
    let program = { p with Memory = Array.copy p.Memory } 
    match decode program with
    | Add(r0, r1, r2) ->
        let a = program.Memory.[r0]
        let b = program.Memory.[r1]
        program.Memory.[r2] <- a + b
        { program with PC = program.PC + 4 }
    | Mult(r0, r1, r2) ->
        let a = program.Memory.[r0]
        let b = program.Memory.[r1]
        program.Memory.[r2] <- a * b
        { program with PC = program.PC + 4 }
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