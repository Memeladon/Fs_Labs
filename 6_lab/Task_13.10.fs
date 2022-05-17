open System
open System.Diagnostics

let listening list1 list2 =
    let rec subFunc list1 list2 counter =
        match list1 with
        |head::tail -> 
            let rec subSubFunc list item count =
                match list with
                |h::t ->
                    let nextCount = if (item = h) then count + 1 else count
                    subSubFunc t item nextCount
                |[] -> count     
            let nextCounter = subSubFunc list2 head 0
            subFunc tail list2 nextCounter
        |[] -> counter
    subFunc list1 list2 0

[<EntryPoint>]
let main argv =

    let list1 = [7;9;7;7;9;7;7;7]
    let list2 = [5;6;3;1;8;7;0;0]
    printfn "different element: %d" (listening list1 list2)
    0
