namespace Advent

module Day2_P2 = 
    open System.IO

    let fileName = "./Day2/input_2.txt"
    let testFile = "test_in.txt"

    let inputLines =
        File.ReadAllLines fileName
        |> Array.toList
        |> List.map (fun x -> x.Split(' ') |> fun y -> (y[0], int y[1]))

    type Submarine = { Horizontal: int; Depth: int; Aim: int }

    let forward horUnit sub =
        { Horizontal = sub.Horizontal + horUnit
          Depth = sub.Depth + (sub.Aim * horUnit)
          Aim = sub.Aim }

    let up upUnit sub =
        { Horizontal = sub.Horizontal
          Depth = sub.Depth
          Aim = sub.Aim - upUnit }

    let down downUnit sub =
        { Horizontal = sub.Horizontal
          Depth = sub.Depth 
          Aim = sub.Aim + downUnit}

    let processCommand sub com =
        match com with
        | ("forward", unit) -> forward unit sub
        | ("up", unit) -> up unit sub
        | ("down", unit) -> down unit sub
        | _ -> sub

    let p2 = 
        let sub = {Horizontal = 0; Depth = 0; Aim = 0}

        inputLines 
        |> List.fold (fun acc x -> processCommand acc x) sub
        |> fun x -> x.Depth * x.Horizontal

    // printfn $"Part 1: {p2}"