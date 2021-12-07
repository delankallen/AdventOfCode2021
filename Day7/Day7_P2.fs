namespace Advent

module Day7_P2 =
    open System.IO

    let inputFile = "./Day7/input_7.txt"
    let testFile = "./Day7/testdata.txt"

    let inputLines fileName =
        File.ReadAllLines fileName
        |> Seq.map (fun x -> x.Split(','))
        |> Seq.last
        |> Seq.map int
        |> Seq.toList

    let sumPos x =
        [0..x] |> List.sum

    let getFuel origin des =
        origin - des |> abs |> sumPos

    let lineUpTheCrabs lines =
        [0..(lines |> List.max)] 
        |> List.map (fun pos -> lines |> List.map (fun origin -> getFuel origin pos) |> List.sum) 
        |> List.min

    let p2 = 
        inputLines inputFile |> lineUpTheCrabs