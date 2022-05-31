let countItemsGreaterThanSumOfPrevious (list:List<int>) =
    let rec loop list sum count =
        match list with
        |head::tail ->
            let nextCount = count + if head > sum then 1 else 0
            let nextSum = sum + head
            loop tail nextSum nextCount
        |[] -> count
    loop list.Tail list.Head 0

let Demo = 
    printfn "%A" (countItemsGreaterThanSumOfPrevious [1;2;4;8;16;32;64;128;256])
    0
