module UnitTests
open System.IO
open CoreTypeSystem
open NUnit.Framework
open FsUnit

// Change TagSeq list to TFJ string
let rec ToString (lst:TagSeq) : string =
    match lst with
    | [] -> ""
    | (Tag.Plus, n1)::xs -> "+" + n1.ToString() + ToString xs
    | (Tag.Minus, n1)::xs -> "-" + n1.ToString() + ToString xs
    | (Tag.Max, n1)::xs -> "#" + n1.ToString() + ToString xs
    | (Tag.Join, n1)::xs -> ":" + n1.ToString() + ToString xs
    | x::xs -> x.ToString() + ToString xs

//+1-1 -> #1
let ast1 = [Leaf (Tag.Plus, 1); Leaf (Tag.Minus, 1)]
let checkAST1 = ToString (infer ast1 []) |> should equal "#1"

//+1+1-1-1 -> #2
let ast2 = [Leaf (Tag.Plus, 1); Leaf (Tag.Plus, 1);Leaf (Tag.Minus, 1);Leaf (Tag.Minus, 1)]
let checkAST2 = ToString (infer ast2 []) |> should equal "#2"

//+1(-1)-1 -> #2
let ast3 = [Leaf (Tag.Plus, 1); Branch ([Leaf (Tag.Minus, 1)]); Leaf (Tag.Minus, 1)] 
let checkAST3 = ToString (infer ast3 []) |> should equal "#2"

//+1+1-1(-1)-1 -> #2
let ast4 = [Leaf (Tag.Plus, 1); Leaf (Tag.Plus, 1); Leaf (Tag.Minus, 1);Branch ([Leaf (Tag.Minus, 1)]); Leaf (Tag.Minus, 1)] 
let checkAST4 = ToString (infer ast4 []) |> should equal "#2"

//+1(+1-1-1)-1 -> #3
let ast5 = [Leaf (Tag.Plus, 1); Branch ([Leaf (Tag.Plus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1)]); Leaf (Tag.Minus, 1)] 
let checkAST5 = ToString (infer ast5 []) |> should equal "#3"

//+2(+2-2-2)-2   -> #6
let ast6 = [Leaf (Tag.Plus, 2); Branch ([Leaf (Tag.Plus, 2); Leaf (Tag.Minus, 2); Leaf (Tag.Minus, 2)]); Leaf (Tag.Minus, 2)] 
let checkAST6 = ToString (infer ast6 []) |> should equal "#6"

//+2(+1-1-1-1)-1-1 -> #5
let ast7 = [Leaf (Tag.Plus, 2); Branch ([Leaf (Tag.Plus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1)]); Leaf (Tag.Minus, 1);Leaf (Tag.Minus, 1)] 
let checkAST7 = ToString (infer ast7 []) |> should equal "#5"

//+2(+1-1-1-1)+1(+2-2-1-1-1)-1+3-3-1+4-4-1
let ast8 = [Leaf (Tag.Plus, 2); Branch ([Leaf (Tag.Plus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1)]); Leaf (Tag.Plus, 1); Branch ([Leaf (Tag.Plus, 2); Leaf (Tag.Minus, 2); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1); Leaf (Tag.Minus, 1)]); Leaf (Tag.Minus, 1); Leaf (Tag.Plus, 3); Leaf (Tag.Minus, 3);Leaf (Tag.Minus, 1);Leaf (Tag.Plus, 4); Leaf (Tag.Minus, 4);Leaf (Tag.Minus, 1)] 
let checkAST8 = ToString (infer ast8 []) |> should equal "#11"

let ast9 = [Leaf (Tag.Plus,1); Leaf (Tag.Plus,1); Leaf (Tag.Minus,1); Branch ([Leaf (Tag.Minus, 1)]); Leaf (Tag.Minus, 1)]
let checkAST9 = ToString (infer ast9 []) |> should equal "#2"

printfn "\nAll tests were executed. Press a key to exit.\n"  
System.Console.ReadKey(true) |> ignore  