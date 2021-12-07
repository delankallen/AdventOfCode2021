namespace Advent

module Day7_P1 =
    open System.IO

    let inputFile = "./Day7/input_7.txt"
    let testFile = "./Day7/testdata.txt"

    let inputLines fileName =
        File.ReadAllLines fileName
        |> Seq.map (fun x -> x.Split(','))
        |> Seq.last
        |> Seq.map int
        |> Seq.toList

    let testValues = [16;1;2;0;4;2;7;1;2;14]

    let sumPos x =
        [0..x] |> List.sum

    let countedValues = inputLines inputFile |> List.countBy id
    let common = countedValues |> List.map (fun (_,b) -> b) |> List.max

    let mostCommonPosition = countedValues |> List.filter (fun (a,b) -> b = common) |> fun x -> fst x[0]

    let getFuel origin des =
        origin - des |> abs |> sumPos

    let wat lines =
        [0..(lines |> List.max)] 
        |> List.map (fun pos -> lines |> List.map (fun origin -> getFuel origin pos) |> List.sum) 
        |> List.min

    let p1 = 
        inputLines inputFile |> wat