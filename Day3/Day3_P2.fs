namespace Advent

module Day3_P2 =
    open System.IO    

    let fileName = "./Day3/input_3.txt"
    let testFile = "test_in.txt"    

    let inputLines =
        File.ReadAllLines fileName
        |> Array.toList

    let testValues = [
        "00100"
        "11110"
        "10110"
        "10111"
        "10101"
        "01111"
        "00111"
        "11100"
        "10000"
        "11001"
        "00010"
        "01010"
        ]

    let countBits (listIn: List<string>) = 
        [
        for i in 0..listIn[0].Length-1 do
            yield listIn |> List.countBy (fun x -> x[i])
        ]

    let mostLeastCommon bit (report: List<char * int>) = 
        let (bitX, x)::(bitY, y)::_ = report

        match (bitX, x, bitY, y) with
        | _ when x = y  -> (bit, bit)
        | _ when x > y -> (bitX, bitY)
        | _ -> (bitY, bitX)
        | _ -> ('f','f')

    let wat = countBits testValues

    let oxy (report:List<string>) i=
        report
        |> List.countBy (fun x -> x[i])
        |> mostLeastCommon '1'
        |> fst

    let cO2 (report:List<string>) i=
        report
        |> List.countBy (fun x -> x[i])
        |> mostLeastCommon '0'
        |> snd

    let rec getOxy (state:List<string>) i =
        let most = (oxy state i)
        match state |> List.filter (fun y -> y[i] = most) with
        | x::[] -> x
        | x -> x |> fun x -> getOxy x (i+1)

    let rec getCo2 (state:List<string>) i =
        let most = (cO2 state i)
        match state |> List.filter (fun y -> y[i] = most) with
        | x::[] -> x
        | x -> x |> fun x -> getCo2 x (i+1)

    let oxyVal = $"0b{getOxy inputLines 0}" |> int
    let cO2Val = $"0b{getCo2 inputLines 0}" |> int

    let p2 = oxyVal * cO2Val
        