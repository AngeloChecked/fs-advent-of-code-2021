namespace AdventOfCode2021

module DayFour =
    type Board = List<List<int>>
    type Bingo = { Sheets: List<Board>;  ChosenNumbers: List<int>}

    let aggregateBoards (board:Board, boards:List<Board>) (line:string) : Board * List<Board> = 
        let lineToNumbers (nline:string) = 
            nline.ToCharArray() 
            |> Array.chunkBySize 3 
            |> Array.map System.String 
            |> Array.map int
            |> Array.toList 

        match (line, board) with
        | ("", _) -> (board, boards) 
        | (_, [_;_;_;_]) -> ([], boards @ [board @ [lineToNumbers line]])
        | _ -> if (line.Split(',') |> Array.length) > 1  
                then (board, boards)  
                else (board @ [lineToNumbers line], boards)

    let parseBingo (lines:List<string>) : Bingo =
        let numbers = lines.Head.Split(',') |> Array.map int |> Array.toList

        let start: Board * List<Board> = ([],[])
        let (_, boards) = lines |> List.fold aggregateBoards start 

        {Sheets=boards; ChosenNumbers=numbers}
    
    let winnerScore {Sheets=boards; ChosenNumbers=numbers} =
        12

    let boardWinner (board:Board) (numbers:List<int>) =
        let isRowWinner = 
            board 
            |> List.exists(List.forall(fun rowNum -> numbers |> List.contains rowNum))
        let isColWinner = 
            board 
            |> List.transpose 
            |> List.exists(List.forall(fun colNum -> numbers |> List.contains colNum) )        
        (isRowWinner || isColWinner) 

    let winnerBoardScore (board:Board) (numbers:List<int>) = 
        let allBoardNums = 
            board
            |> List.concat
            |> Set.ofList 

        let unmarkedScoreSum = 
            numbers 
            |> Set.ofList
            |> Set.difference allBoardNums 
            |> Set.toList
            |> List.sum
        let winnerNumber = List.last numbers
        unmarkedScoreSum * winnerNumber 
    
    let runGame { Sheets=boards; ChosenNumbers=numbers} = 
        let rec loopUntilWinner position = 
            let extractedNumbers = numbers |> List.take position 

            try 
               let winner = boards |> List.find( fun board -> boardWinner board extractedNumbers) 
               winnerBoardScore winner extractedNumbers 
            with
                | _ -> loopUntilWinner (position+1) 

        loopUntilWinner 1 

    let runGamePart2 { Sheets=boards; ChosenNumbers=numbers} = 
        let rec loopUntilWinner position inPlayBoards (winningScores:List<int>) = 
            try 
               let extractedNumbers = numbers |> List.take position 
               let indexedBoard = inPlayBoards |> List.indexed 
               let winners = indexedBoard |> List.filter( fun (_, board) -> boardWinner board extractedNumbers) 
               
               let boardsWithoutWinners = 
                    indexedBoard 
                    |> List.filter(fun (i, _) -> winners |> List.forall(fun (indexToRemove, _) -> i <> indexToRemove) ) 
                    |> List.map snd 

               let scores =
                    winners 
                    |> List.map(fun (i, winner) -> winnerBoardScore winner extractedNumbers) 

               loopUntilWinner (position+1) boardsWithoutWinners (winningScores @ scores)
            with
                | :? System.InvalidOperationException -> winningScores
                | _ -> loopUntilWinner (position+1) inPlayBoards winningScores 

        loopUntilWinner 1 boards [] 