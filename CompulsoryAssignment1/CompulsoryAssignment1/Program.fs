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
//F# Interactive window:
//  val insert : weaklyACList:'a list -> listItem:'a -> int when 'a : comparison
//  count[1;1;1;1;2;3;3] 1;;
//  Expected outcome is , val it : int list = [1;1;1;1;1;2;3;3]


// Part 3: Intersect
let intersect (firstList, secondList) = List.filter (fun elm -> List.contains elm firstList) secondList
//F# Interactive window:
// val intersect : firstList:'a list * secondList:'a list when 'a : equality
//intersect([1;2;3;4;4;5],[2;3;4;4]);;
//val it : int list = [2; 3; 4; 4]


// Part 4: Plus
let plus (firstList, secondList) =
    let rec plus' list resultlist =
        match list with
        | [] -> resultlist
        | x :: xs -> insert resultlist x |> plus' xs
    plus' secondList firstList
//F# Interactive window:
// val plus : firstList:'a list * secondList:'a list -> 'a list when 'a : comparison
//plus ([1;1;2;2;3;3], [1;2;3]);;
//val it : int list = [1; 1; 1; 2; 2; 2; 3; 3; 3]


// Part 5: Minus
let minus (firstList, secondList) =
    let rec removeTheItem list itemToRemove finalResultList =
        match list with
        | [] -> finalResultList
        | x :: [] when x = itemToRemove -> finalResultList
        | x :: xs when x = itemToRemove -> finalResultList @ xs
        | x :: xs -> finalResultList @ [ x ] @ removeTheItem xs itemToRemove finalResultList

    let rec removeTheList listToRemoveFrom finalResultList =
        match listToRemoveFrom with
        | [] -> finalResultList
        | x :: xs -> removeTheList xs (removeTheItem finalResultList x [])

    removeTheList firstList secondList

    //F# Interactive window:
    //val minus : firstList:'a list * secondList:'a list -> 'a list when 'a : equality
    //minus([1;1;1;2;2:3;3], [1;1;1;2;3]);;
    //val it : int list = [2; 3]