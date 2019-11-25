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
//F# Interactive window:
//  val count : weaklyACList:'a list -> listItem:'a -> int when 'a : equality
//  count[1;1;1;1;2;3;3] 1;;
//  Expected outcome is , val it : int = 4

// Part 2: Insert
let rec insert weaklyACList listItem =
    match weaklyACList with
    | [] -> [ listItem ]
    | x :: [] when listItem <= x -> [ listItem ] @ [ x ]
    | x :: [] when listItem > x -> [ x ] @ [ listItem ]
    | x :: xs when listItem <= x -> listItem :: [ x ] @ xs
    | x :: xs when listItem > x -> x :: (insert xs listItem)
    | _ -> failwith "Incomplete match on %A"


// Part 3: Intersect
let intersect (firstList, secondList) = List.filter (fun elm -> List.contains elm firstList) secondList


// Part 4: Plus
let plus (firstList, secondList) =
    let rec plus' list resultlist =
        match list with
        | [] -> resultlist
        | x :: xs -> insert resultlist x |> plus' xs
    plus' secondList firstList

// Part 5: Minus
let minus (minuendList, subtrahendList) =
    let rec removeItem list itemToRemove resultList =
        match list with
        | [] -> resultList
        | x :: [] when x = itemToRemove -> resultList
        | x :: xs when x = itemToRemove -> resultList @ xs
        | x :: xs -> resultList @ [ x ] @ removeItem xs itemToRemove resultList

    let rec removeList listToRemove resultlist =
        match listToRemove with
        | [] -> resultlist
        | x :: xs -> removeList xs (removeItem resultlist x [])

    removeList subtrahendList minuendList