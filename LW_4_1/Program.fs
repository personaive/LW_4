open System

type 't tree = 
    Node of 't * 't tree * 't tree | Nil // обобщённый тип дерева

let addDigitToNumber digit number =
    if number >= 0 then
        int (string digit + string number)
    else
        let absoluteString = string (-number)
        int ("-" + string digit + absoluteString)

let rec mapTree applyFunction currentTree =
    match currentTree with
    | Nil -> Nil
    | Node(nodeValue, leftSubTree, rightSubTree) ->
        Node(applyFunction nodeValue,
             mapTree applyFunction leftSubTree,
             mapTree applyFunction rightSubTree)

let addDigitToTree digit originalTree = 
    mapTree (addDigitToNumber digit) originalTree

let rec insertNode currentTree numberToInsert =
    match currentTree with
    | Nil -> Node(numberToInsert, Nil, Nil)
    | Node(nodeValue, leftSubTree, rightSubTree) ->
        if numberToInsert < nodeValue then 
            Node(nodeValue,
            insertNode leftSubTree numberToInsert,rightSubTree)
        else 
            Node(nodeValue,
            leftSubTree, insertNode rightSubTree numberToInsert)

let rec printTree currentTree indent =
    match currentTree with
    | Nil -> ()
    | Node(nodeValue, leftSubTree, rightSubTree) ->
        printTree rightSubTree (indent + 4)
        printfn "%s%d" (String(' ', indent)) nodeValue
        printTree leftSubTree (indent + 4)

let rec readDigit () =
    printf "Введите цифру для добавления: "
    let input = 
        Console.ReadLine().Trim() // убираем лишние пробелы

    if input.Length = 1 && Char.IsDigit(input.[0]) then 
        int input // конвертируем в число
    else
        printfn "Некорректный ввод. Введите от 0 до 9."
        readDigit ()

[<EntryPoint>]
let main argv =

    let randomGenerator = 
        Random() // генератор случайных чисел

    printf "Сколько чисел в дереве?"
    let numberCount = 
        Console.ReadLine() |> int

    let numbersList = 
        [ for i in 1..numberCount ->
            randomGenerator.Next(-150, 160) ] // создаём список

    printfn "Список чисел: %A" numbersList

    let tree = 
        List.fold insertNode Nil numbersList

    printfn "\nИсходное дерево:"
    printTree tree 0 // вывод исходного дерева

    let digitToAdd =
        readDigit () // ввод цифры для добавления

    let newTree =
        addDigitToTree digitToAdd tree

    printfn "\nНовое дерево:"
    printTree newTree 0

    0