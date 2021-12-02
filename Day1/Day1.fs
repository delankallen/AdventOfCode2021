namespace Advent

module Day1 =
    open System.IO

    let fileName = "./Day1/input_1.txt"

    let inputLines =
        File.ReadAllLines fileName
        |> Array.toList
        |> List.map int

    let rec increasedValues s =
        match s with
        | x :: y :: _ ->
            if y > x then
                y :: (increasedValues s.Tail)
            else
                (increasedValues s.Tail)
        | _ -> []

    let p1 =
        increasedValues inputLines |> List.length

    let wat =
        List.fold (fun acc x -> if x > x then x :: acc else acc) [] inputLines

    let rec threeVals s =
        match s with
        | x :: y :: z :: _ -> [ x; y; z ] :: (threeVals s.Tail)
        | _ -> []

    let p2 =
        inputLines
        |> threeVals
        |> List.map (fun x -> x |> List.sum)
        |> increasedValues
        |> List.length
