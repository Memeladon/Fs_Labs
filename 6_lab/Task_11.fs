open System

let rec CreateList size = 
    if size <= 0 then []
    else 
        let head = Console.ReadLine() |> Int32.Parse
        let tail = createList (size - 1)
        head::tail

let rec WriteList list = 
    match list with
    |head::tail ->
        printfn "%O" head
        writeList tail
    |[] -> ()
    
let ListEdit list func = 
    let rec loop list func nextList = 
        match list with
        |slot1::tail ->
            let slot2 = if tail <> [] then tail.Head else 1
            let slot3 = if tail <> [] then (if tail.Tail <> [] then tail.Tail.Head else 1) else 1
            let ReceivingFunc = func slot1 slot2 slot3
            let nextNextList = nextList @ [ReceivingFunc]
            let editedList =  if tail <> [] then (if tail.Tail <> [] then tail.Tail.Tail else []) else []
            loop editedList func nextNextList
        |[] -> nextList
    loop list func []

[<EntryPoint>]
let main argv = 
    printfn "Enter the number of list size: "
    let list = (ListEdit (CreateList (Console.ReadLine() |> Int32.Parse)) (fun a b c -> a + b + c))
    WriteList list
    0
