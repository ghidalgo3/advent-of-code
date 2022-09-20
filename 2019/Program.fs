open System.IO

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

[<EntryPoint>]
let main args =
    let one : int = day1 args.[0]
    printfn $"{one}" 
    0
