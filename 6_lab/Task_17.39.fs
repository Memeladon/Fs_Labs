let rec printList list =
    match list with
    |head::tail ->
        printf "%A  " head
        printList tail
    |[] -> ()

let printEvensOdds (list:List<int>) =
    let rec getEvens list evens index= 
        match list with
        |head::tail ->
            let nextEvens = if index % 2 = 0 then evens @ [head] else evens
            getEvens tail nextEvens (index + 1)
        |[] -> evens 
    let evens = getEvens list [] 0

    let rec getOdds list evens index= 
        match list with
        |head::tail ->
            let nextEvens = if index % 2 = 1 then evens @ [head] else evens
            getOdds tail nextEvens (index + 1)
        |[] -> evens 
    let odds = getOdds list [] 0

    printList evens
    printfn ""
    printList odds
