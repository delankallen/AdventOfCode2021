namespace Advent

module Day3_P1 =
    open System.IO

    let fileName = "./Day3/input_3.txt"
    let testFile = "test_in.txt"

    let inputLines =
        File.ReadAllLines fileName
        |> Array.toList

    let countBits (listIn: List<string>) = 
        [
        for i in 0..listIn[0].Length-1 do
            yield listIn |> List.countBy (fun x -> x[i])
        ]

    let gammaEpsilon (report: List<List<char * int>>) = 
        report 
        |> List.map (fun x -> if (snd x[0] > snd x[1]) then (fst x[0], fst x[1]) else (fst x[1], fst x[0]))
        |> List.map (fun (a,b) -> (string a, string b))
        |> List.fold (fun (x, y) (a,b) -> ( x+a , y+b) ) ("0b", "0b")
        |> fun (a,b) -> (int a, int b)

    let p1 = inputLines |> countBits |> gammaEpsilon |> fun (a,b) -> a*b;
        