let findMax list = 
    let rec loop list curMax  = 
        match list with
        |head::tail ->
            let nextCurMax = if head > curMax then head else curMax
            loop tail nextCurMax
        |[] -> curMax
    if list <> [] then  loop list list.Head else 0

let findItems list =
    let rec loop list resultList maxItem flag=
        match list with
        |head::tail ->
            let flag1 = if head = maxItem then 1 else flag
            let nextResultList = if flag = 1 then resultList @ [head] else resultList
            loop tail nextResultList maxItem flag1
        |[] -> resultList
    loop list [] (findMax list) 0
