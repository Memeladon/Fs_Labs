open System
//18.1 Максимальный простой делитель числа
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
    // Простое число
let PrimeCheck x =
    let rec PrimeCheckElementary div =
        if div >= x then true
        else
            if (x % div = 0) then false
            else 
                let nextDiv = div + 1
                PrimeCheckElementary nextDiv
    PrimeCheckElementary 2

let processDivisorsWithCondition x func cond  elementary =
        let subFunc elementary div = 
            if func div then 
                cond elementary div
            else elementary
        processDivisorsOfNumber x subFunc elementary
let MaxPrimeDiv x =
    processDivisorsWithCondition x (PrimeCheck) (fun x y -> max x y) 1

//18.2 Произведение цифр числа, не делящихся на 5
let elementaryNum elementary cond =
    let rec subFunc x mult =
        if(x = 0) then mult
        else
            let nextAcc = if cond (x % 10) then mult*(x%10) else mult
            let nextX = x / 10
            subFunc nextX nextAcc
    subFunc elementary 1

let multNotFive x =
    elementaryNum x (fun x -> x%5<>0)

//18.3 НОД максимального нечетного непростого делителя числа и произведения цифр данного числа
let dividersFunc x func elementary=
    let rec subFunc x func elementary curDiv =
        if curDiv = 0 then elementary
        else
            let nextElementary= 
                if x % curDiv = 0 then 
                    func elementary curDiv 
                else elementary
            let nextDiv = curDiv - 1
            subFunc x func nextElementary nextDiv
    subFunc x func elementary x

let oddPrimeDivisor x func cond elementary =
    let func1 elementary cur = 
        if func cur then 
            cond elementary cur 
        else elementary
    dividersFunc x func1 elementary

let GreatestCommonDivisorMax x=
    GreatestCommonDivisor (oddPrimeDivisor x (fun x -> x%2 > 0 && not(PrimeCheck x)) (fun x y -> max x y) 1) (elementaryNum x (fun x -> true))
      
[<EntryPoint>]
let main argv =
    printfn "Enter the number: "
    let x = Console.ReadLine() |> Int32.Parse

    printfn "Maximum prime divisor of a number:: %d" (MaxPrimeDiv x)
    printfn "product of digits of a number not divisible by 5: %d" (multNotFive x)
    printfn "GCD of the maximum odd non-simple divisor of a number and the product of the digits of a given number: %d" (GreatestCommonDivisorMax x)

    0
