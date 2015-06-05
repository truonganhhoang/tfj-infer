module CoreTypeSystem
open Microsoft.FSharp.Collections

type Tag =
   | Plus = 0
   | Minus = 1
   | Max = 2
   | Join = 3
   
type TagNum = Tag * int

type TagSeq = TagNum list

type Tree = 
   | Branch of Tree list
   | Leaf of TagNum 

// Define seq function
let rec seq (lst: TagSeq) : TagSeq =
    match lst with
    | [] -> []
    | (_, 0)::xs -> seq xs
    | (Tag.Plus, n1)::(Tag.Plus, n2) :: xs -> seq ((Tag.Plus,n1+n2)::xs)
    | (Tag.Max, n1)::(Tag.Max, n2) :: xs -> seq ((Tag.Max,max n1 n2)::seq xs)
    | (Tag.Minus, n1)::(Tag.Minus, n2) :: xs -> seq ((Tag.Minus,n1+n2)::xs)
    | (Tag.Plus, n1)::(Tag.Minus, n2) :: xs -> 
        if n1 >= n2 then 
            seq ((Tag.Plus,n1-n2)::(Tag.Max,n2)::xs) 
        else 
            seq ((Tag.Max,n1)::(Tag.Minus,n2-n1)::xs) 
    | (Tag.Plus, n1)::(Tag.Max, n)::(Tag.Minus, n2) :: xs -> 
        let m = min n1 n2 in 
            if n1 > n2 then 
                seq ((Tag.Plus,n1-m)::(Tag.Max, n+m)::xs) 
             elif n1 = n2 then 
                seq ((Tag.Max, n+m)::xs) 
            else 
                seq ((Tag.Max, n+m)::(Tag.Minus,n2-m)::xs) 
    | x::xs -> x :: (seq xs)

// Define join function
let rec join (lst: TagSeq) : TagSeq =
    match lst with
    | [] -> []
    | (Tag.Minus, n1)::xs -> 
        if n1 > 0 then 
            (Tag.Join, 1) :: (join ((Tag.Minus, n1-1)::xs)) 
        else 
            (join xs)
    | x::xs -> x::(join xs)

// Define merge function
let rec merge (lst1: TagSeq) (lst2: TagSeq) : TagSeq = 
    if List.isEmpty lst1 then lst2 
        elif List.isEmpty lst2 then lst1 
    else
        let tag1 = fst (List.head lst1) in
        let tag2 = fst (List.head lst2) in
        if tag1 = Tag.Max && tag2 = Tag.Max then 
            (Tag.Max, (snd (List.head lst1)) + (snd (List.head lst2))) :: (merge (List.tail lst1) (List.tail lst2))
        elif tag1 = Tag.Join && tag2 = Tag.Join then 
            (Tag.Join, (snd (List.head lst1)) + (snd (List.head lst2))) :: (merge (List.tail lst1) (List.tail lst2)) 
        elif tag1 = Tag.Max && tag2 = Tag.Join then 
            (Tag.Max, snd (List.head lst1)) :: (merge (List.tail lst1) lst2) 
        elif tag1 = Tag.Join && tag2 = Tag.Max then 
            (Tag.Max, snd (List.head lst2)) :: (merge lst1 (List.tail lst2)) 
        else failwith "Error in merge"

// Define joint commit function
let rec jc (lst1: TagSeq) (lst2: TagSeq) : TagSeq =
    match lst1 with
    | [] -> if List.isEmpty lst2 then 
                [] 
            else lst2
    | (Tag.Plus,m1)::[] -> 
        match lst2 with
        | [] -> lst1
        | (Tag.Max,l1)::(Tag.Join,l2)::xs2 -> 
            if m1 > 1 then 
                jc [(Tag.Plus,m1-1)] (seq((Tag.Max,l1+l2) :: xs2)) 
            else 
                (Tag.Max,l1+l2) :: xs2
         |(Tag.Join, l2)::xs2 -> 
            if m1 > 1 then 
                jc [(Tag.Plus, m1-1)] (seq ((Tag.Max, l2) :: xs2)) 
            else
                (Tag.Max, l2) :: xs2
        | otherwise -> failwith "Need attention in jc 1"
    | (Tag.Plus,n1)::(Tag.Max,n2)::[] -> 
        match lst2 with
        | [] -> lst1
        | (Tag.Max,l1)::(Tag.Join,l2)::xs2 -> 
            if n1 >= 1 then 
                jc ((Tag.Plus,(n1-1)) :: [Tag.Max,(max (n2+1) (l1+l2))]) xs2 
            else 
                jc [Tag.Join,max (n2+1) (l1+l2)] xs2
        | (Tag.Join, l2)::xs2 -> 
            if n1 > 1 then 
                jc [(Tag.Plus, n1-1)] (seq ((Tag.Max, l2) :: xs2)) 
            else 
                (Tag.Max, max (n2+1) l2) :: xs2
        | otherwise -> failwith "Need attention in jc 2"
    | x::xs -> []

// Define choice function
let choice (lst1: TagSeq) (lst2: TagSeq) : TagSeq =
    match lst1 with
    | [] -> if List.isEmpty lst2 then 
                [] 
            else 
                lst2
    | x::xs -> 
        match  lst2 with
        | [] -> lst1
        | x2::xs2 -> 
            if (fst x) = (fst x2) && (xs = xs2) && (fst x) = Tag.Max then 
                if (snd x >= snd x2) then 
                    lst1 
                else 
                    lst2
            else []

let rec infer (branch: Tree list) (headseq:TagSeq) = 
    match branch with 
    | [] -> seq headseq
    | x::xs -> 
        match x with
            | Leaf tagnum -> 
                let newhead = seq (List.append headseq [tagnum]) in
                    infer xs newhead
            | Branch br -> 
                let child = join (infer br []) in 
                let parent = join (infer xs []) in
                let tailseq = seq (merge child parent) in
                seq (jc headseq tailseq)

let rec addTree (lst1: char list) : Tree list =
    match lst1 with
    | [] -> []
    | '-'::xs -> List.append [Leaf (Tag.Minus, 1)] (addTree xs)
    | '+'::xs -> List.append [Leaf (Tag.Plus, 1)] (addTree xs)
    | '('::xs -> [Branch (addTree xs)]
    | ')'::xs -> addTree xs
    | x::xs -> addTree xs

