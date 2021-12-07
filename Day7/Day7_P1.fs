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

    let getFuel origin des =
        origin - des |> abs

    let lineUpTheCrabs lines =
        [0..(lines |> List.max)] 
        |> List.map (fun pos -> lines |> List.map (fun origin -> getFuel origin pos) |> List.sum) 
        |> List.min

    let p1 = 
        inputLines inputFile |> lineUpTheCrabs