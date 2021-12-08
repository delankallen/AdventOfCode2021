namespace Advent

module Day8_P1 =
    open System
    open System.IO

    let inputFile = "./Day8/input_8.txt"
    let testFile = "./Day8/testdata.txt"

    let inputLines fileName =
        File.ReadAllLines fileName
        |> Seq.map (fun line -> line.Split('|')[1])
        |> Seq.map (fun line -> line.Split(' ',  StringSplitOptions.RemoveEmptyEntries) |> Seq.toList)
        |> Seq.toList
        |> List.reduce (@)

    let getUniqueNums input = 
        [2; 4; 3; 7] 
        |> List.fold (fun acc len -> 
            (input |> List.filter (fun (x:string) -> 
                x.Length = len))@acc) 
            []

    let p1 = 
        inputLines inputFile
        |> getUniqueNums
        |> fun x -> x.Length