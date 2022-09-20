open System.IO

let fuelRequirement mass = mass / 3 - 2

let day1 (input : string) : int =
    let input = [ for line in File.ReadLines(input) -> line |> int |> fuelRequirement ]
    input |> List.sum

[<EntryPoint>]
let main args =
    let one : int = day1 args.[0]
    printfn $"{one}" 
    0
