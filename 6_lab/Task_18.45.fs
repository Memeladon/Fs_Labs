let sumBetween list a b =
    let rec loop list a b sum = 
        match list with
        |head::tail ->
            let nextSum = sum + (if head > a && head < b then head else 0)
            loop tail a b nextSum
        |[] -> sum
    loop list a b 0
