let findMaxOdd list = 
    let rec loop list curMax  = 
        match list with
        |head::tail ->
            let nextCurMax = if head > curMax && head %  2 = 1 then head else curMax
            loop tail nextCurMax
        |[] -> curMax
    if list <> [] then  loop list list.Head else 0
