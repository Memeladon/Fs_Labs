open System

let processAnswer  QandA = 
    match QandA with
        |"Prolog"|"F#"-> "Well, you sycophant!"
        |"Python" -> "And you are good!"
        |"C++"|"c++"|"Cpp"|"cpp"-> "Do you love optimization that much? Not bad"
        |"C#"|"с#"|"Cs"|"cs"-> "Do you love versatility? Not bad"
        |"JavaScript"|"javascript"|"JS"|"js"-> "Dynamic web pages?.. To each his own"
        |"Java"-> "2 + 2 =  22... Ok.."
        
        |other -> "Good choice, I guess..."
    
[<EntryPoint>]
let main argv =
    printf "Which programming language do you like the most?: " 
    let QandA = Console.ReadLine()
    Console.WriteLine(processAnswer QandA)
    0