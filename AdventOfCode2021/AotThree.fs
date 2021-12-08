namespace AdventOfCode2021

module DayThree =
    let linesToMatrix (inputs:List<string>) = 
        let inline charToInt c = int c - int '0'
        inputs 
        |> List.map(fun line -> line.ToCharArray() 
                                |> Array.map charToInt 
                                |> Array.toList
                                ) 
    
    type BitCount = { Zero: int; One: int }
    let countBitPerMatrixColumn (matrix:List<List<int>>) =
        matrix 
        |> List.transpose 
        |> List.map(
            fun column -> 
                { Zero = column |> List.filter( (=)0 ) |> List.length
                ; One = column |> List.filter( (=)1 ) |> List.length
                } 
        ) 
        
    let mostCommonPerMatrixColumn (matrix:List<List<int>>) =
        countBitPerMatrixColumn matrix
        |> List.map(fun {Zero=zero; One=one} -> if zero > one then 0 else 1) 
    
    let invertRow row =
        row 
        |> List.map(fun el -> if el=0 then 1 else 0 )
    
    let fromBinaryToDecimal (row:List<int>) =
        row
        |> List.rev 
        |> List.map float 
        |> List.fold(
            fun (s, index) el -> 
                if el = 0.0 
                then (s, index+1.0)
                else (s + 2.0**index, index+1.0) 
        ) (0.0, 0.0)
        |> fun (n,_) -> int n 
    
    let filterPerCriteria criteria (matrix:List<List<int>>) = 
        let rec filter (position:int) innerMatrix =
            match innerMatrix with 
            | [last] -> last
            | _ -> 
                let bitCounts = countBitPerMatrixColumn innerMatrix
                let bitToKeep = criteria bitCounts.[position]
                innerMatrix 
                |> List.filter(fun el -> el.[position] = bitToKeep) 
                |> filter (position+1) 
        filter 0 matrix
    
    let oxigenGeneratorCriteria {Zero=zero; One=one} =
        if zero > one 
        then 0
        else 1 
        
    let co2ScrubberCriteria {Zero=zero; One=one} =
        if one < zero 
        then 1
        else 0 