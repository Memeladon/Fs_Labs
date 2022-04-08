open System

//Multiplying
let rec MultiplyingUP (n:int) =
    if n = 0 then 1
    else (n % 10) * MultiplyingUP(n / 10)    

let MultiplyingDOWN (n:int) = 
    let rec SubMultiplyingDOWN (n:int) curMult = 
        if n = 0 then curMult   
        else
            let nextN = n / 10
            let nextDigit = n % 10
            let nextMult = curMult * nextDigit
            SubMultiplyingDOWN nextN nextMult
    SubMultiplyingDOWN n 1

//Max 
let rec MaxUP (n:int) = 
    if n < 10 then n
    else 
        max (n % 10) (MaxUP (n / 10))

let MaxDOWN (n:int) = 
    let rec SubMaxDOWN (n:int) curMax =
        if n = 0 then curMax
        else
            let nextMax = 
                if n % 10 > curMax then n % 10 
                else curMax
            let nextN = n / 10
            SubMaxDOWN nextN nextMax
    SubMaxDOWN n (n % 10)
  
//Min
let rec MinUP (n:int) =
    if n < 10 then n
    else
        min (n % 10) (MinUP (n / 10))

let MinDOWN (n:int) =
    let rec SubMinUP (n:int) curMin =
        if n = 0 then curMin
        else
            let nextMin = 
                if n % 10 < curMin then n % 10
                else curMin
            let nextN = n / 10
            SubMinUP nextN nextMin

    SubMinUP n (n % 10)

[<EntryPoint>]
let main argv =    
    printf "Enter a number: "
    let n = Console.ReadLine() |> Int32.Parse
    printf "Product of digits (up): \t %A \n" (MultiplyingUP n)
    printf "Product of digits (down): \t %A \n" (MultiplyingDOWN n)
    printf "Maximal digit (up):     \t %A \n" (MaxUP n)
    printf "Maximal digit (down):   \t %A \n" (MaxDOWN n)
    printf "Minimal digit (up):     \t %A \n" (MinUP n)
    printf "Minimal digit (down):   \t %A \n" (MinDOWN n)
    0