module Numbers

type FrequencyDistribution = Map<int, int>
/// splits a number into digits
/// digits 1234 == [1; 2; 3; 4]
let digits n = 
    let rec _digits n =
        if n < 10 then
            [n]
        else
            [n % 10] @ _digits (n / 10)
    _digits n |> List.rev

/// Computes the frequency of digits in an integer
let freq (xs : 'T list) =
    List.fold (fun (acc: Map<'T, int>) n ->
            if acc.ContainsKey n then
                acc.Change (n, fun n -> Some(n.Value + 1))
            else
                acc.Add (n, 1)
        ) Map.empty xs

let isNonDecreasing n = 
    digits n
    |> List.windowed 2
    // you CAN destructure like this
    |> List.exists (function | [a; b] -> b < a) 
    |> not

let hasSameAdjacentDigits n =
    digits n
    |> List.windowed 2
    |> List.exists (function | [a; b] -> b = a) 

let strangeCriteria n =
    digits n
    |> List.windowed 2
    |> freq
    |> Map.exists (fun pair f -> pair[0] = pair[1] && f = 1)

let padZeros padding (ns : int list) : int list = 
    if (ns.Length >= padding) then
        ns
    else
        let zeros = List.init (padding - ns.Length) (fun n -> 0)
        zeros @ ns
