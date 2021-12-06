namespace Advent

module Day4_P1 =
    open System
    open System.IO

    let inputFile = "./Day4/input_4.txt"
    let testFile = "./Day4/testdata.txt"

    type Space =
        | Empty of int
        | Marked of int

    type Board = Space list list
    type Draws = int list
    type SquidGame = { Draws: Draws; Boards: Board list }

    let markRow drawnNum (row: Space list) =
        row
        |> List.map (fun s ->
            match s with
            | Empty e when e = drawnNum -> Marked e
            | _ -> s)

    let markBoard drawnNum (board: Board) : Board =
        board
        |> List.map (fun row -> row |> markRow drawnNum)

    let markBoards drawnNum (boards: Board list) =
        boards
        |> List.map (fun board -> markBoard drawnNum board)

    let updateGame draw squid : SquidGame =
        {Draws = squid.Draws; Boards = markBoards draw squid.Boards }

    let getBoards boards : Board list=
        boards
        |> List.filter (fun x -> x <> "")
        |> List.map (fun x ->
            x.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map (fun x -> x |> int |> Empty)
            |> Seq.toList)
        |> List.chunkBySize 5

    let inputLines fileName =
        File.ReadAllLines fileName
        |> Seq.toList
        |> fun x -> { Draws = x.Head.Split(',') |> Seq.map int |> Seq.toList; Boards = x.Tail |> getBoards }

    let checkRow row =
        row |> List.forall (fun x -> match x with | Marked _ -> true | _ -> false)
    let checkBoardRows (board:Board) : Board =
        board |> List.filter checkRow
    let checkBoardColumns (board: Board) : Board =
        [ for i in 0..board.Length - 1 do yield board |> List.map (fun x -> x[i])]
        |> List.filter checkRow

    let checkBoard (board:Board) =
        match (checkBoardRows board, checkBoardColumns board) with
        | ([],[]) -> None
        | (_, _) -> Some board

    let checkBoards squid  = 
        squid.Boards |> List.tryPick checkBoard

    let sumRow row =
        row |> List.fold (fun acc x -> acc + match x with | Empty y -> y | _ -> 0) 0

    let boardScore (board, draw) =
        board 
        |> List.fold (fun acc x -> acc + (sumRow x) ) 0
        |> (*) draw

    let rec winningBoardAndDraw draw squid =
        match squid |> checkBoards with
        | Some x -> (x, draw)
        | None -> 
            updateGame squid.Draws.Head {Draws = squid.Draws.Tail; Boards = squid.Boards} 
            |> winningBoardAndDraw squid.Draws.Head

    let p1 =
        inputLines inputFile 
        |> winningBoardAndDraw 0
        |> boardScore
