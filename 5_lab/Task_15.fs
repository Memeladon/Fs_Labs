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
    

[<EntryPoint>]
let main argv =       
    printf "Sum of mutually prime numbers: %A" (roundOff 5 (fun (x:int) (y:int) -> x + y) 0)
    0 
