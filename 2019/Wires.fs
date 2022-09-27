module Wires

type Point = { X : int ; Y : int }
type WireSegment = { Start : Point ; End : Point }
type Wire = WireSegment list
type Direction = Up | Down | Left | Right

let sub p1 p2 = { X = p1.X - p2.X ; Y = p1.Y - p2.Y }
let mult p1 n = { X = p1.X * n ; Y = p1.Y * n }

let manhattanDistance p1 p2 =
    let d = sub p1 p2
    abs(d.X) + abs(d.Y)

let manhattanLength point = abs(point.X) + abs(point.Y)

let xContained x ws =
    ws.Start.X <= x && x <= ws.End.X
let yContained y ws =
    ws.Start.Y <= y && y <= ws.End.Y

// Line input comes in as U7, R6, D4, L4 of (direction, length) pairs
let parseWire (input: string) : Wire =
    let wireSegment from (offset: string) =
        let direction = offset.Substring(0, 1)
        let length = offset.Substring(1, offset.Length - 1) |> int
        match (direction, length) with
        | ("U", length) -> { Start = from; End = { from with Y = from.Y + length }}
        | ("D", length) -> { Start = from; End = { from with Y = from.Y - length }}
        | ("L", length) -> { Start = from; End = { from with X = from.X - length }}
        | ("R", length) -> { Start = from; End = { from with X = from.X + length }}
        | _ -> invalidArg (nameof offset) "Invalid direction"
    let origin = { X = 0 ; Y = 0 }
    // state       // collection                    // fold function
    (([], origin), input.Split(",")) ||> Array.fold (fun state offset ->
        let nextSegment = wireSegment (snd state) offset
        (nextSegment::(fst state), nextSegment.End))
    |> fst
    |> List.rev

let direction wireSegment = 
    let endDirection = sub wireSegment.End wireSegment.Start
    match (endDirection.X, endDirection.Y) with
    | (0, y) when y > 0 -> Up
    | (0, y) when y < 0 -> Down
    | (x, 0) when x > 0 -> Right
    | (x, 0) when x < 0 -> Left
    | _ -> invalidArg (nameof wireSegment) "Must be orthogonal"

let (|Parallel|_|) (segmentPair : WireSegment * WireSegment) =
    let (a, b) = segmentPair
    let (d1, d2) = (direction a, direction b)
    match (d1, d2) with
    | (Up, Up) | (Up, Down) | (Down, Down) | (Down, Up) -> Some Parallel
    | (Left, Left) | (Left, Right) | (Right, Right) | (Right, Left) -> Some Parallel
    | _ -> None

// UpDown, then LeftRight
let (|Perpendicular|_|) segmentPair =
    let (a, b) = segmentPair
    let (d1, d2) = (direction a, direction b)
    match (d1, d2) with
    | (Up, Left) | (Up, Right) | (Down, Left) | (Down, Right) -> Some (a, b)
    | (Left, Up) | (Right, Up) | (Left, Down) | (Right, Down) -> Some (b, a)
    | _ -> None

// we need to normalize directions to simplify the overlap check in the intersection function
let normalizeDirection ws =
    match direction ws with
    | Up | Right -> ws
    | Left| Down -> { Start = ws.End ; End = ws.Start } // invert them

let intersection ws1 ws2 : Point option = 
    match (ws1, ws2) with
    | Parallel -> None
    | Perpendicular(vertical, horizontal) ->
        let vNorm = normalizeDirection vertical
        let hNorm = normalizeDirection horizontal
        if (yContained hNorm.Start.Y vNorm) && (xContained vNorm.Start.X hNorm) then
            Some { X = hNorm.Start.Y ; Y = vNorm.Start.X }
        else
            None

let intersections (wire1 : Wire) (wire2 : Wire) =
        List.allPairs wire1 wire2
        |> List.choose (fun (ws1, ws2) -> intersection ws1 ws2)

// let w1 = parseWire "R75,D30,R83,U83,L12,D49,R71,U7,L72"
// let w2 = parseWire "U62,R66,U55,R34,D71,R55,D58,R83"
// let iii = intersections w1 w2
// find intersections
// parsing wires
// placing them on a grid