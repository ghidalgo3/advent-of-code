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
let xContained x ws = ws.Start.X <= x && x <= ws.End.X
let yContained y ws = ws.Start.Y <= y && y <= ws.End.Y
let wireSegmentLength ws = manhattanDistance ws.Start ws.End

let direction wireSegment = 
    let endDirection = sub wireSegment.End wireSegment.Start
    match (endDirection.X, endDirection.Y) with
    | (0, y) when y > 0 -> Up
    | (0, y) when y < 0 -> Down
    | (x, 0) when x > 0 -> Right
    | (x, 0) when x < 0 -> Left
    | _ -> invalidArg (nameof wireSegment) "Must be orthogonal"

// we need to normalize directions to simplify the overlap check in the intersection function
let normalizeDirection ws =
    match direction ws with
    | Up | Right -> ws
    | Left| Down -> { Start = ws.End ; End = ws.Start } // invert them

let pointOnWireSegment ws point = (xContained point.X (normalizeDirection ws)) && (yContained point.Y (normalizeDirection ws))

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


let intersection ws1 ws2 : Point option = 
    match (ws1, ws2) with
    | Parallel -> None
    | Perpendicular(vertical, horizontal) ->
        let vNorm = normalizeDirection vertical
        let hNorm = normalizeDirection horizontal
        if (yContained hNorm.Start.Y vNorm) && (xContained vNorm.Start.X hNorm) then
            Some { X = vNorm.Start.X ; Y = hNorm.Start.Y }
        else
            None

let intersections (wire1 : Wire) (wire2 : Wire) =
        List.allPairs wire1 wire2
        |> List.choose (fun (ws1, ws2) -> intersection ws1 ws2)

let lengthAlongWire wire point =
    let before = List.takeWhile (fun ws -> not (pointOnWireSegment ws point)) wire
    // printfn $"{point}"
    // printfn $"{wire.Length}"
    // printfn $"{before}"
    // printfn $"{before.Length}"
    let on = manhattanDistance point wire[before.Length].Start
    (List.sum <| [ for ws in before -> wireSegmentLength ws ]) + on
    // find the wire segment this point is on
    // find the wire segments before that wire segment, add their lengths
    // add the length along the wiresegment where the point is on