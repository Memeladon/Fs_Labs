open System

let rec createList size = 
    if size <= 0 then []
    else 
        let head = Console.ReadLine() |> Int32.Parse
        let tail = createList (size - 1)
        head::tail

[<EntryPoint>]
let main argv = 
    printfn "Enter the number of list size: "
    let list = (createList (Console.ReadLine() |> Int32.Parse)) (fun a b c -> a + b + c))
