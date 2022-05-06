open System

let counter list element =
    let rec subCounter list element count  =
        match list with
        | [] -> count
        | head::tail ->
            let newCount = if (head = element) then count + 1 else count
            subCounter tail element newCount
    subCounter list element 0

let findFunc list =
    let rec subFunc list primalList element =
        match list with
        | [] -> element
        | head::tail ->
            let newElement = if (counter primalList head) = 1 then head else element
            subFunc tail primalList newElement
    subFunc list list list.Head

[<EntryPoint>]
let main argv =

    let list = [7;8;7;7;7;7;7;7]
    printfn "different element: %d" (findFunc list)

    0
