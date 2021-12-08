namespace Advent

module Day8_P2 =
    open System
    open System.IO

    let inputFile = "./Day8/input_8.txt"
    let testFile = "./Day8/testdata.txt"

    let explode (input:string) =
        input.Split(' ', StringSplitOptions.RemoveEmptyEntries) 
        |> Seq.toList
        |> List.map (fun x -> (x.ToCharArray() |> Seq.toList))

    let inputLines fileName =
        File.ReadAllLines fileName
        |> Seq.map (fun line -> line.Split('|') |> (fun x -> (x[0],x[1])))
        |> Seq.map (fun (keys,output) -> ((explode keys),(explode output)))
        |> Seq.toList

    let getUniques input =
        let uniques = 
            [ 2; 4; 3; 7 ]
            |> List.fold
                (fun (acc: char list list) len ->
                    acc
                    @ (input |> List.filter (fun x -> x.Length = len)))
                []
        let filtered = 
            List.except uniques input |> List.map (Set)

        (uniques |> List.map (Set) |> Seq.zip [1;4;7;8] |> Map.ofSeq, filtered)

    let filterByUnique uniques inputs =
        uniques 
        |> List.fold (fun acc unique -> (acc |> List.filter (fun x -> Set.isSubset unique x))) inputs

    let getKeys ((uniques: Map<int, Set<char>>), inputs) =
        let nine = filterByUnique [ uniques[4] ] inputs |> List.head
        let leftLeg = Set.difference uniques[8] nine |> Seq.head
        let (zero, three) = 
            inputs 
            |> filterByUnique [uniques[1]] 
            |> List.filter (fun x -> x <> nine)
            |> (fun x -> 
                ((x |> List.find (fun y -> y.Contains(leftLeg))), (x |> List.find(fun y -> not (y.Contains(leftLeg)))) ))
        
        let five = 
            [nine; zero; three]
            |> List.fold (fun acc x -> acc |> List.filter (fun y -> y<>x)) inputs
            |> List.find (fun x -> not (x.Contains(leftLeg)))

        let (two,six) =
            [nine; zero; three; five]
            |> List.fold (fun acc x -> acc |> List.filter (fun y -> y<>x)) inputs
            |> (fun x -> ((x |> List.find (fun y -> y.Count = 5)),(x |> List.find (fun y -> y.Count = 6))))

        List.zip [zero; uniques[1]; two; three; uniques[4]; five;six;uniques[7]; uniques[8]; nine] ([0..9]|>List.map (string))
        |> Map.ofList 

    let decodeOutput input =
        let (signal, output) = input
        let codes = 
            getUniques signal
            |> getKeys

        output |> List.map (Set) |> List.fold (fun (acc: string) x -> acc + codes[x]) "" |> int

    let decodeReport input =
        input |> List.map (decodeOutput)

    let p2 = 
        inputLines inputFile
        |> decodeReport
        |> List.sum
