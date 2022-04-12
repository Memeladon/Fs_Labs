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

[<EntryPoint>]
let main argv = 
    printfn "Enter the number of list size: "
    let list = (CreateList (Console.ReadLine() |> Int32.Parse)) (fun a b c -> a + b + c))
    WriteList list
    0
