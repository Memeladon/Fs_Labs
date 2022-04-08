let rec GreatestCommonDivisor x y = 
    if x = 0 || y = 0 then x + y
    else 
        let nextX = 
            if x > y then x % y
            else x
        let nextY = 
            if x <= y then y % x
            else y
        GreatestCommonDivisor nextX nextY

let processDivisorsOfNumber n func elementary =
    let rec loop n func elementary curDiv = 
        if curDiv = 0 then elementary
        else
            let nextElementary = 
                if n % curDiv = 0 then func elementary curDiv
                else elementary
            let nextDiv = curDiv - 1
            loop n func nextElementary nextDiv
    loop n func elementary n

let roundOff (n:int) func elementary = 
    let rec loop n func elementary exp =
        if exp <= 0 then elementary
        else
            let nextElementary =
                if  GreatestCommonDivisor n exp = 1 then func elementary exp
                else elementary
            let nextExp = exp - 1           
            loop n func nextElementary nextExp  
    loop n func elementary n

// 17.1
let processDivisorsWithCondition (n:int) func cond elementary =
        let subFunc elementary div = 
            if cond div then func elementary div
            else elementary
        processDivisorsOfNumber n subFunc elementary

// 17.2
let roundCoprimeWithCondition (n:int) func cond elementary = 
    let subFunc elementary div = 
        if cond div then func elementary div
        else elementary
    roundOff n subFunc elementary

[<EntryPoint>]
let main argv =           
    printfn  "%A" (processDivisorsWithCondition 11 (fun x y -> x + y) (fun d -> d > 4) 0)
    printf  "%A" (roundCoprimeWithCondition 11 (fun x y -> x + y) (fun d -> d % 2 = 1) 0)
    0 