namespace AdventOfCode2021

module DayFive =
    type Coo = int * int
    type Area = {From: Coo; To: Coo} 
    type AreaMap = Map<int * int, int>

    let parseCoos (lines:List<string>) =
        lines
        |> List.map(fun line -> line.Split(" -> ") |> Array.toList )
        |> List.map(fun splited -> let from = splited.[0].Split(',') |> Array.map int
                                   let to_ = splited.[1].Split(',') |> Array.map int
                                   { From= (from.[0],from.[1]); To=(to_.[0],to_.[1]) }
                   )

    let allStraightCooFromArea({From=(x1,y1); To=(x2,y2)}): List<int * int> =
        let xStep = if x1 < x2 then 1 else -1
        let yStep = if y1 < y2 then 1 else -1
        [for x in x1..(xStep)..x2 -> (x, y1) ] @ [for y in y1+(yStep)..(yStep)..y2 -> (x1, y)]

    let allCooFromArea({From=(x1,y1); To=(x2,y2)}): List<int * int> =
        let cartesianProduct x y =
           x |> List.collect (fun xs -> y |> List.map (fun ys -> xs, ys))

        let xStep = if x1 < x2 then 1 else -1
        let yStep = if y1 < y2 then 1 else -1
        let x = [ for x in x1..(xStep)..x2 -> x] 
        let y = [ for y in y1..(yStep)..y2 -> y ] 

        if (List.length x) = (List.length y)
        then y |> List.zip(x)
        else cartesianProduct x y 

    let overlapAllArea(coos:List<Area>) areaToCoos =
        let map: AreaMap = Map []
        let rec sumCooTogether map coo =
            map |> Map.change coo (fun value ->
               match value with
               | Some value -> Some (value+1)
               | None -> Some 1
            )
        
        coos 
        |> List.map areaToCoos 
        |> List.concat
        |> List.fold sumCooTogether map

    let numberOfOverlappingStraightAreas areas = 
        let straightCoo = areas |> List.filter(fun { From=(x1,y1); To=(x2,y2) } -> x1 = x2 || y1=y2 ) 
        overlapAllArea straightCoo allStraightCooFromArea
        |> Map.filter(fun _ v -> v > 1)
        |> Map.toList 
        |> List.length

    let numberOfOverlappingAllAreas areas = 
        overlapAllArea areas allCooFromArea
        |> Map.filter(fun _ v -> v > 1)
        |> Map.toList 
        |> List.length
