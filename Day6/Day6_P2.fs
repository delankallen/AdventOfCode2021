namespace Advent

module Day6_P2 =
    open System.IO

    let inputFile = "./Day6/input_6.txt"
    let testFile = "./Day6/testdata.txt"

    let inputLines fileName =
        File.ReadAllLines fileName
        |> Seq.map (fun x -> x.Split(','))
        |> Seq.last
        |> Seq.map int

    let fishSpawner babies =
        seq [for _ in 1..babies do yield 8]

    let wat = function 
        | -1 -> 6
        | x -> x-1

    // let fishSpawnedInLifeTime daysLeft fish =
    //     [1..((daysLeft+fish)/6 )] |> List.map (fun x -> 7*x-fish) |> List.map (fun x -> x+fish) |> List.sum |> fun x -> x/7

    // [3;4;3;1;2] 
    // |> List.map (fishSpawnedInLifeTime 18)

    let calcNew days fish =
        (7-fish)+(7 * (days/6))

    let fishDay population =
        let (newPop, babies) = population |> Seq.mapFold (fun babies fish -> (match fish with | 0 -> 6 | _ -> fish - 1), (match fish with | 0 -> babies + 1 | _ -> babies) ) 0
        // let spawners = newPop |> Seq.filter ((=)6) |> Seq.map (fun _ -> 8)

        Seq.append newPop (fishSpawner babies)

    let fishToString population =
        population |> Seq.fold (fun acc fish -> acc + $"{fish},") ""

    let rec runTheClock (days:int list) population =
        // printfn $"After {days.Head} days: {fishDay population |> fishToString}"
        match days with
        | [] -> population
        | _::rest -> 
            fishDay population |> runTheClock rest

    let p2 = "frank"