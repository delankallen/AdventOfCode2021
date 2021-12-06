namespace Advent

module Day4_P2 =
    open System
    open System.IO

    let inputFile = "./Day4/input_4.txt"
    let testFile = "./Day4/testdata.txt"

    type Space =
        | Empty of int
        | Marked of int

    type BasicBoard = Space list list
    type Board = 
    | NotWonBoard of Space list list
    | WinningBoard of Space list list
    with 
        member x.Value =
            match x with
            | NotWonBoard b
            | WinningBoard b -> b

    type Draws = int list
    type SquidGame = { Draws: Draws; Boards: Board list }

    let markRow drawnNum (row: Space list) =
        row
        |> List.map (fun s ->
            match s with
            | Empty e when e = drawnNum -> Marked e
            | _ -> s)

    let markBoard drawnNum board: Board =
        match board with
        | NotWonBoard x -> x |> List.map (fun row -> row |> markRow drawnNum) |> NotWonBoard
        | _ -> board

    let markBoards drawnNum (boards: Board list) =
        boards
        |> List.map (fun board -> markBoard drawnNum board)

    let updateGame draw squid : SquidGame =
        {Draws = squid.Draws; Boards = markBoards draw squid.Boards }

    let getBoards boards =
        boards
        |> List.filter (fun x -> x <> "")
        |> List.map (fun x ->
            x.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map (fun x -> x |> int |> Empty)
            |> Seq.toList)
        |> List.chunkBySize 5
        |> List.map NotWonBoard

    let inputLines fileName =
        File.ReadAllLines fileName
        |> Seq.toList
        |> fun x -> { Draws = x.Head.Split(',') |> Seq.map int |> Seq.toList; Boards = x.Tail |> getBoards}

    let checkRow row =
        row |> List.forall (fun x -> match x with | Marked _ -> true | _ -> false)

    let checkBoardRows board =
        match board with
        | NotWonBoard x when (x |> List.filter checkRow) <> [] -> WinningBoard x
        | _  -> board

    let checkBoardColumns board =
        match board with
        | NotWonBoard x when ([ for i in 0..x.Length - 1 do yield x |> List.map (fun x -> x[i])] |> List.filter checkRow) <> [] -> 
            WinningBoard x
        | _ -> board

    let checkBoard squid =
        squid.Boards
        |> List.map (fun x -> checkBoardRows x)
        |> List.map (fun x -> checkBoardColumns x)
        |> fun boards -> {Draws = squid.Draws; Boards = boards}

    let sumRow row =
        row |> List.fold (fun acc x -> acc + match x with | Empty y -> y | _ -> 0) 0

    let boardScore (board, draw) =
        board 
        |> List.fold (fun acc x -> acc + (sumRow x) ) 0
        |> (*) draw

    let boardWon (boards: Board list) =
        boards
        |> List.filter (fun board -> match board with | WinningBoard _ -> true | _ -> false)

    let lastBoardWon (boards: Board list) =
        boards 
        |> List.filter (fun board -> match board with | NotWonBoard _ -> true | _ ->  false)

    let rec winningBoardAndDraw draw squid =
        squid
        |> checkBoard
        |> fun s -> 
            match s.Boards |> boardWon with
            | [] ->
                updateGame s.Draws.Head {Draws = s.Draws.Tail; Boards = s.Boards} 
                |> winningBoardAndDraw s.Draws.Head
            | (WinningBoard x)::_ -> (x, draw)

    let rec waitForWin (board:Board list) (draws: int list) =
        board 
        |> List.map (fun b -> markBoard draws.Head b)
        |> List.map (fun x -> checkBoardRows x)
        |> List.map (fun x -> checkBoardColumns x)
        |> fun b -> 
            match boardWon b with
            | [] -> waitForWin b draws.Tail
            | _ -> (b.Head.Value, draws.Head)


    let rec lastWinningBoardAndDraw draw squid =
        squid
        |> checkBoard
        |> fun s -> 
            match s.Boards |> lastBoardWon with
            | boards when boards.Length <= 1 -> (waitForWin boards s.Draws)
            | _ -> 
                updateGame s.Draws.Head {Draws = s.Draws.Tail; Boards = s.Boards} 
                |> lastWinningBoardAndDraw s.Draws.Head

    let p2 = 
        inputLines inputFile 
        |> lastWinningBoardAndDraw 0 
        |> boardScore