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


let roundPrime (n:int) func elementary = 
    let rec loop n func elementary exp =
        if exp <= 0 then elementary
        else
            let nextElementary =
                if  GreatestCommonDivisor n exp = 1 then func elementary exp
                else elementary
            let nextExp = exp - 1           
            loop n func nextElementary nextExp  
    loop n func elementary n

let EulerNumber n = 
    roundPrime n (fun x y -> x + 1) 0

[<EntryPoint>]
let main argv =           
    printf "Euler's number: %A \n" (EulerNumber 5)
    printf "Greatest common divisor: %A" (GreatestCommonDivisor 12 60)
    0 
