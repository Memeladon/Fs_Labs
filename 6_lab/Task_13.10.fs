//10.1
let memberList item list = 
    let rec loop item list flag=
                match list with
                |h::t -> 
                    let nextFlag = if item = h then true else false
                    if nextFlag then (loop item [] nextFlag) else loop item t nextFlag
                |[]-> flag
    loop item list false

let firstFindMatching list list2 = 
    let rec loop list list2 count = 
        match list with
        |head::tail ->          
            let nextCount = count + (if memberList head list2 then 1 else 0)
            loop tail list2 nextCount
        |[] -> count
    loop list list2 0

//10.2
let secondFindMatching list (list2:List<int>) =
    let rec loop list  (list2:List<int>) count =
        match list with
        |head::tail ->
            let nextCount = count + (if head = list2.Head then 1 else 0)
            loop tail list2.Tail nextCount
        |[] -> count
    loop list list2 0
