open System

let WorkingWithNumbers n func initial =
    let rec SumDigitsOfANumber n func elementary curDiv = 
        if curDiv = 0 then elementary
        else
            let nextElementary = 
                if n % curDiv = 0 then func elementary curDiv
                else elementary
            let nextDiv = curDiv - 1
            SumDigitsOfANumber n func nextElementary nextDiv
    SumDigitsOfANumber n func initial n

[<EntryPoint>]
let main argv =    
    printf "Sum of divisors: %A" (WorkingWithNumbers 21 (fun x y -> x + y) 0)
    0