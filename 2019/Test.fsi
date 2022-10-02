#load "Wires.fs"
open Wires

let wire1 = parseWire "R8,U5,L5,D3"
let wire2 = parseWire "U7,R6,D4,L4"
let iii = intersections wire1 wire2
iii