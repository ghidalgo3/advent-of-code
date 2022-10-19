namespace Tests
// You must put tests into a namespace and not a module otherwise MSTest cannot
// instantiate your tests because obviously if they're in a module then they're 
// in some weird auto-generated static class.

open Graphs
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type GraphTests () = 

    [<TestMethod>]
    member this.SimpleGraph() = 
        let g = DirectedGraph<string>.empty ()
        Assert.AreEqual(0, g.Edges.Keys |> Seq.length)
        Assert.AreEqual(0, g.Vertices.Length)

    [<TestMethod>]
    member this.AddOneEdge() = 
        // A -> B
        let g =  addEdge "A" "B" (DirectedGraph<string>.empty ())
        Assert.AreEqual(2, g.Edges.Keys |> Seq.length)
        Assert.AreEqual(2, g.Vertices.Length)
        Assert.AreEqual(0, g.Edges["B"].Length)
        Assert.AreEqual(1, g.Edges["A"].Length)

    [<TestMethod>]
    member this.AddOneEdgeThenReverse() = 
        // A -> B
        // then
        // B -> A
        let g =  addEdge "A" "B" (DirectedGraph<string>.empty ()) |> reverse
        Assert.AreEqual(2, g.Edges.Keys |> Seq.length)
        Assert.AreEqual(2, g.Vertices.Length)
        Assert.AreEqual(1, g.Edges["B"].Length)
        Assert.AreEqual(0, g.Edges["A"].Length)

    [<TestMethod>]
    member this.AddNewEdge() = 
        // C <- A -> B
        let g =
            DirectedGraph<string>.empty () 
            |> addEdge "A" "B"
            |> addEdge "A" "C"
            // |> addEdge "D" "A"
        Assert.AreEqual(3, g.Edges.Keys |> Seq.length)
        Assert.AreEqual(3, g.Vertices.Length)
        Assert.AreEqual(0, g.Edges["B"].Length)
        Assert.AreEqual(0, g.Edges["C"].Length)
        Assert.AreEqual(2, g.Edges["A"].Length)

    [<TestMethod>]
    member this.AddEdgeToExistingVertex() = 
        //      D
        //      |
        //      v
        // C <- A -> B
        let g =
            DirectedGraph<string>.empty () 
            |> addEdge "A" "B"
            |> addEdge "A" "C"
            |> addEdge "D" "A"
        Assert.AreEqual(4, g.Vertices.Length)
        Assert.AreEqual(2, g.Edges["A"].Length)
        Assert.AreEqual(
            3,
            edges g |> Seq.length)

    [<TestMethod>]
    member this.AddDuplicateEdge() = 
        // A -> B
        let g =
            DirectedGraph<string>.empty () 
            |> addEdge "A" "B"
            |> addEdge "A" "B"
        Assert.AreEqual(2, g.Vertices.Length)
        Assert.AreEqual(1, g.Edges["A"].Length)

    [<TestMethod>]
    member this.Loop() = 
        // A -> B
        // ^    |
        // \   /
        //   C
        let g =
            DirectedGraph<string>.empty () 
            |> addEdge "A" "B"
            |> addEdge "B" "C"
            |> addEdge "C" "A"

        Assert.AreEqual(3, g.Vertices.Length)
        Assert.AreEqual(1, g.Edges["A"].Length)
        Assert.AreEqual(1, g.Edges["B"].Length)
        Assert.AreEqual(1, g.Edges["C"].Length)
        Assert.AreEqual(
            3,
            edges g |> Seq.length)
    
