module AotFourTest

open Xunit
open AdventOfCode2021.Utils
open AdventOfCode2021.DayFour

let aotInputFile =
    $"{__SOURCE_DIRECTORY__}/resource/day4.txt"

let rawInput = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"""

let expectedBoards = [
            [
                    [ 22;13; 17; 11;  0];
                    [  8; 2; 23;  4; 24];
                    [ 21; 9; 14; 16;  7];
                    [  6;10;  3; 18;  5];
                    [  1;12; 20; 15; 19];
            ];
            [
                    [ 3; 15;  0;  2; 22 ]
                    [ 9; 18; 13; 17;  5 ]
                    [ 19;  8;  7; 25; 23 ]
                    [ 20; 11; 10; 24;  4 ]
                    [ 14; 21; 16; 12;  6 ]
            ];
            [
                    [ 14; 21; 17; 24;  4; ]
                    [ 10; 16; 15;  9; 19; ]
                    [ 18;  8; 23; 26; 20; ]
                    [ 22; 11; 13;  6;  5; ]
                    [ 2;  0; 12;  3;  7; ]
            ]
    ] 

[<Fact>]
let ``parse boards`` () =
    let inputLines = rawInput.Split('\n') |> Array.toList

    let (_, boards) = inputLines |> List.fold aggregateBoards ([],[]) 

    Assert.Equal<List<List<List<int>>>>(expectedBoards, boards)

[<Fact>]
let ``parse inputs`` () =
    let inputLines = rawInput.Split('\n') |> Array.toList

    let output = parseBingo inputLines 
    let expect = 
        { ChosenNumbers = [7;4;9;5;11;17;23;2;0;14;21;24;10;16;13;6;15;25;12;22;18;20;8;19;3;26;1]
        ; Sheets = expectedBoards 
        }

    Assert.Equal(expect, output)

[<Fact>]
let ``board wins when a column or row is completed`` () =
    let aBoard = expectedBoards.[2] 
    let winnerNumbers =  [7;4;9;5;11;17;23;2;0;14;21;24]
    let notWinnerNumbers =  [7;4;9;5;11;17;23;2;0;14;21]

    let isWinner = boardWinner aBoard winnerNumbers 
    let isNotWinner = boardWinner aBoard notWinnerNumbers 

    Assert.Equal(true,isWinner)
    Assert.Equal(false,isNotWinner)

let ``get winning board score`` () =
    let aBoard = expectedBoards.[2] 
    let winnerNumbers =  [7;4;9;5;11;17;23;2;0;14;21;24]

    let score = winnerBoardScore aBoard winnerNumbers 

    Assert.Equal(4512, score)

[<Fact>]
let ``Aot solution 4`` () =
    let inputLines = read aotInputFile |> Seq.toList 
    let bingo = parseBingo inputLines 

    let score = runGame bingo

    Assert.Equal(51034, score)

[<Fact>]
let ``Aot Part2`` () =
    let inputLines = rawInput.Split('\n') |> Array.toList
    let bingo = parseBingo inputLines 

    let allWinnerScore = runGamePart2 bingo

    Assert.Equal<List<int>>([4512; 2192; 1924], allWinnerScore)

[<Fact>]
let ``Aot solution 4 Part2`` () =
    let inputLines = read aotInputFile |> Seq.toList 
    let bingo = parseBingo inputLines 

    let score = runGamePart2 bingo

    let lastScore = score |> List.last
    Assert.Equal(5434, lastScore)
