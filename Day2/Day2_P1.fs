namespace Advent

module Day2_P1 =
    open System.IO

    let fileName = "./Day2/input_2.txt"
    let testFile = "test_in.txt"

    let inputLines =
        File.ReadAllLines fileName
        |> Array.toList
        |> List.map (fun x -> x.Split(' ') |> fun y -> (y[0], int y[1]))

    type Submarine = { Horizontal: int; Depth: int }

    let forward horUnit sub =
        { Horizontal = sub.Horizontal + horUnit
          Depth = sub.Depth }

    let up upUnit sub =
        { Horizontal = sub.Horizontal
          Depth = sub.Depth - upUnit }

    let down downUnit sub =
        { Horizontal = sub.Horizontal; Depth = sub.Depth + downUnit }

    let processCommand sub com =
        match com with
        | ("forward", unit) -> forward unit sub
        | ("up", unit) -> up unit sub
        | ("down", unit) -> down unit sub
        | _ -> sub

    let p1 = 
        let sub = {Horizontal = 0; Depth = 0}
        inputLines |> List.fold (fun acc x -> processCommand acc x) sub
        |> fun x -> x.Depth * x.Horizontal
        