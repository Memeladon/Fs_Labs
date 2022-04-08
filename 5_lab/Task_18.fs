open System
//18.1
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
let processDivisorsOfNumber x func elementary =
    let rec loop x func elementary curDiv = 
        if curDiv = 0 then elementary
        else
            let nextElementary = 
                if x % curDiv = 0 then func elementary curDiv
                else elementary
            let nextDiv = curDiv - 1
            loop x func nextElementary nextDiv
    loop x func elementary x
let PrimeCheck x =
    let rec PrimeCheckElementary div =
        if div >= x then true
        else
            if (x % div = 0) then false
            else 
                let nextDiv = div + 1
                PrimeCheckElementary nextDiv
    PrimeCheckElementary 2
let processDivisorsWithCondition x cond func elementary =
        let subFunc elementary div = 
            if cond div then 
                func elementary div
            else elementary
        processDivisorsOfNumber x subFunc elementary
let MaxPrimeDiv x =
    processDivisorsWithCondition x (PrimeCheck) (fun x y -> max x y) 1
        
        [<EntryPoint>]
let main argv =
    printfn "Enter the number: "
    let x = Console.ReadLine() |> Int32.Parse

     printfn "Maximum prime divisor of a number:: %d" (MaxPrimeDiv x)
