module Graphs

type DirectedGraph<'T when 'T : comparison> =
    { Vertices : 'T list
      Edges    : Map<'T, DirectedEdge<'T> list> }

    static member empty<'T> () = 
        { Edges = Map.empty<'T, DirectedEdge<'T> list>
          Vertices = [] }

and DirectedEdge<'T> =
    { From : 'T
      To   : 'T }

// what we need to do is to traverse the graph with a DFS, keep track of the depth and collect all
// of the direct and indirect orbits.
// Ok so we need to find the root which is made easy because there is a COM object.
// let root = graph.Edges |> List.find "COM"

// let countAllDirectOrbits graph = 

let edges graph = 
    graph.Edges.Values
    |> Seq.filter (fun es -> es.Length > 0)
    |> Seq.collect id 

let addEdge (vertex1 : 'T) (vertex2: 'T) (graph : DirectedGraph<'T>) = 
    // create the new vertices if they don't exist
    // let newEdge = { From = vertex1 ; To = vertex2 }
    let v1 = Map.containsKey vertex1 graph.Edges
    let v2 = Map.containsKey vertex2 graph.Edges
    let newEdge = {From = vertex1 ; To = vertex2}
    let appendVertex (v : (DirectedEdge<'T> list) option) : (DirectedEdge<'T> list) option =
        match v with 
        | Some(l) -> Some(newEdge :: l)
        | None -> Some([newEdge])
    match v1, v2  with
    | (false, false) ->
        { graph with 
            Vertices = vertex1 :: vertex2 :: graph.Vertices
            Edges = (Map.add vertex1 [newEdge] graph.Edges) |> (Map.add vertex2 []) }
    | (false, true) ->
        { graph with 
            Vertices = vertex1 :: graph.Vertices
            Edges = Map.add vertex1 [newEdge] graph.Edges }
    | (true, false) ->
        { graph with 
            Vertices = vertex2 :: graph.Vertices
            Edges = (Map.change vertex1 appendVertex graph.Edges) |> (Map.add vertex2 []) }
    | (true, true) when not (List.contains newEdge graph.Edges[vertex1]) ->
        // only add if edge doesn't already exist.
        { graph with 
            Edges = Map.change vertex1 appendVertex graph.Edges}
    | _ -> graph

let reverse (graph : DirectedGraph<'T>) =
    (DirectedGraph<'T>.empty (), edges graph)
    ||> Seq.fold (fun accum edge -> addEdge edge.To edge.From accum )

// // input is the multi-line string from the examples
// let parseGraph input = 
//     let graph : DirectedGraph<string> = { Vertices : [] ; Edges : []}
//     for line in input.Split("\n") do
//         let tokens = line.Split(")")
//         let central = tokens[0]
//         let satellite = tokens[1]
//         addEdge { From = satellite ; To = Central } graph
