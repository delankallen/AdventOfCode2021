namespace Advent

module Day5_P2 =
    open System.IO

    let inputFile = "./Day5/input_5.txt"
    let testFile = "./Day5/testdata.txt"

    let inputLines fileName =
        File.ReadAllLines fileName
        |> Seq.map (fun line -> 
            line.Split(" -> ") 
            |> Seq.map (fun x -> 
                x.Split(',') 
                |> (fun cord -> (int cord[0], int cord[1])))
            |> Seq.toList)
            |> Seq.toList

    let urMom = inputLines testFile

    let max x y =
        if x < y then
            [x..y]
        else 
            [y..x]

    let between (x1,y1) (x2,y2) =
        match ((x1, y1), (x2, y2)) with
        | _ when x1 = x2 ->
            List.allPairs [x1] (max y1 y2)
        | _ when y1 = y2 ->
            List.allPairs (max x1 x2) [y1]
        | _ when (y2 - y1)/(x2 - x1) = 1 -> 
            List.zip (max x1 x2) (max y1 y2)
        | _ -> 
            List.zip (max x1 x2) (List.rev (max y1 y2))

    let rec mapVents (coords: (int * int) list) (map: Map<(int*int), int>) = 
        match coords with
        | [] -> map
        | coord::rest -> 
            match map.ContainsKey coord with
            | true -> map.Add(coord, map[coord] + 1)
            | false -> map.Add(coord, 1)
            |> mapVents rest

    let rec markVents (coords: (int * int) list list) (map: Map<(int*int), int>) =
        match coords with
        | [] -> map
        | (origin::des::_)::rest -> 
            between origin des
            |> fun loc -> mapVents loc map 
            |> markVents rest
            
    let p2 = 
        markVents (inputLines inputFile) Map.empty
        |> Map.filter (fun _ v -> v >= 2)
        |> fun x -> x.Count
