namespace AdventOfCode2021

module DayFive =
    type Coo = int * int
    type Area = {From: Coo; To: Coo} 
    type AreaMap = Map<Area, int>

    let parseCoos (lines:List<string>) =
        lines
        |> List.map(fun line -> line.Split(" -> ") |> Array.toList )
        |> List.map(fun splited -> let from = splited.[0].Split(',') |> Array.map int
                                   let to_ = splited.[1].Split(',') |> Array.map int
                                   { From= (from.[0],from.[1]); To=(to_.[0],to_.[1]) }
                   )

    let x(coos:List<Area>) =
       1 

    let allCoo({From=(x1,y1); To=(x2,y2)}): List<int * int> =
        let xStep = if x1 < x2 then 1 else -1
        let yStep = if y1 < y2 then 1 else -1
        [for x in x1..(xStep)..x2 -> (x, y1) ] @ [for y in y1+(yStep)..(yStep)..y2 -> (x1, y)]
        