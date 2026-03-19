open System

type 't tree = 
    Node of 't * 't tree * 't tree | Nil 

let rec insertNode currentTree numberToInsert =
    match currentTree with
    | Nil -> Node(numberToInsert, Nil, Nil)
    | Node(nodeValue, leftSubtree, rightSubtree) ->
        if numberToInsert < nodeValue then 
            Node(nodeValue,
            insertNode leftSubtree numberToInsert, rightSubtree)
        else 
            Node(nodeValue,
            leftSubtree, insertNode rightSubtree numberToInsert)

let rec printTree currentTree indent =
    match currentTree with
    | Nil -> ()
    | Node(nodeValue, leftSubtree, rightSubtree) ->
        printTree rightSubtree (indent + 4)
        printfn "%s%d" (String(' ', indent)) nodeValue
        printTree leftSubtree (indent + 4)

let rec treeFold folder accumulator currentTree =
    match currentTree with
    | Nil -> accumulator
    | Node(nodeValue, leftSubtree, rightSubtree) ->
        let leftAcc =
            treeFold folder accumulator leftSubtree

        let currentAcc =
            folder leftAcc nodeValue

        treeFold folder currentAcc rightSubtree

let countByDigits currentCount number requiredDigits =
    let digits =
        number.ToString().Length

    if digits =
        requiredDigits then 
            currentCount + 1
    else
        currentCount

let rec readNaturalNumber promptMessage =
    printf "%s" promptMessage

    let input =
        Console.ReadLine().Trim()

    match System.Int32.TryParse(input) with
    | true, value when value > 0 -> value
    | _ ->
        printfn "Ошибка! Введите натуральное число больше 0."
        readNaturalNumber promptMessage

[<EntryPoint>]
let main argv =

    let random =
        Random()

    let numberCount =
        readNaturalNumber "Сколько чисел в дереве? "

    let numbersList =
        [ for i in 1..numberCount -> random.Next(1, 1000) ]
    printfn "Список чисел: %A" numbersList

    let tree =
        List.fold insertNode Nil numbersList
    printfn "\nИсходное дерево:"
    printTree tree 0

    let requiredDigits =
        readNaturalNumber "\nВведите значность для подсчёта: "

    let count =
        treeFold (fun acc value -> 
            countByDigits acc value requiredDigits)
            0 tree

    printfn "\nКоличество чисел с %d цифрами" requiredDigits
    printfn "в дереве: %d" count

    0