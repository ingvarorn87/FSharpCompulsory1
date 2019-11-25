// Learn more about F# at http://fsharp.org

open System
open System.Linq


// Part 1: count
let rec count weaklyACList listItem =
    match weaklyACList with
    | [] -> 0
    | x :: [] ->
        if x = listItem then 1
        else 0
    | x :: xs ->
        (if x = listItem then 1
         else 0)
        + count xs listItem


// Part 2: Insert
let rec insert weaklyACList listItem =
    match weaklyACList with
    | [] -> [ listItem ]
    | x :: [] when listItem <= x -> [ listItem ] @ [ x ]
    | x :: [] when listItem > x -> [ x ] @ [ listItem ]
    | x :: xs when listItem <= x -> listItem :: [ x ] @ xs
    | x :: xs when listItem > x -> x :: (insert xs listItem)
    | _ -> failwith "Incomplete match on %A"


