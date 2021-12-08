module AotFiveTest

open Xunit
open AdventOfCode2021.Utils
open AdventOfCode2021.DayFive

let aotInputFile =
    $"{__SOURCE_DIRECTORY__}/resource/day5.txt"

[<Fact>]
let ``parse boards`` () =
    let rawInput = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"""
    let inputLines = rawInput.Split('\n') |> Array.toList
    let output = parseCoos inputLines 
    let expect = [
        {From=(0,9); To=(5,9)}
        {From=(8,0); To=(0,8)}
        {From=(9,4); To=(3,4)}
        {From=(2,2); To=(2,1)}
        {From=(7,0); To=(7,4)}
        {From=(6,4); To=(2,0)}
        {From=(0,9); To=(2,9)}
        {From=(3,4); To=(1,4)}
        {From=(0,0); To=(8,8)}
        {From=(5,5); To=(8,2)}
    ]
    Assert.Equal<List<Area>>(expect, output)
    
[<Fact>]
let ``from Area to all coos`` () =
    let area = {From=(6,4); To=(2,0)}

    let coos = allCoo area

    let expect = [
        (6,4); (5,4); (4,4); (3,4); (2,4);
        (6,3); (6,2); (6,1); (6,0); 
    ]
    Assert.Equal<List<int * int>>(expect, coos)

[<Fact>]
let ``from Area to all coos 2`` () =
    let area = {From=(1,1); To=(1,3)}

    let coos = allCoo area

    let expect = [ (1,1); (1,2); (1,3); ]
    Assert.Equal<List<int * int>>(expect, coos)

[<Fact>]
let ``from Area to all coos 3`` () =
    let area = {From=(9,7); To=(7,7)}

    let coos = allCoo area

    let expect = [ (9,7);(8,7);(7,7); ]
    Assert.Equal<List<int * int>>(expect, coos)
