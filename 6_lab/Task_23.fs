//23
let findMin list = 
    let rec loop list curMax  = 
        match list with
        |head::tail ->
            let nextCurMax = if head < curMax then head else curMax
            loop tail nextCurMax
        |[] -> curMax
    loop list list.Head

let findTwoMins list = 
    let rec loop list firstMin secondMin =
        match list with 
        |head::tail ->             
            let nextSecondMin = if head = firstMin then (if tail <> [] then findMin tail else secondMin)  else secondMin
            let nextTail = if head = firstMin then [] else tail
            loop nextTail firstMin nextSecondMin
        |[] -> (firstMin,secondMin)
    loop list (findMin list) list.Head
