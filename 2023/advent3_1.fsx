open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let symbolMap' =
    File.ReadAllLines ($"__SOURCE_DIRECTORY__/sample3.txt")
    |> Seq.mapi (fun row line ->
        line.ReplaceLineEndings("").ToCharArray ()
        |> Seq.mapi (fun col chr ->
            if (not (Char.IsDigit(chr))) && chr <> '.'
            then Some (row, col)
            else None
            )
        )
    |> Seq.concat
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> Seq.map (fun (row,col) ->
        [ (row-1,col-1); (row-1,col); (row-1,col+1)   
          (row  ,col-1);              (row  ,col+1)   
          (row+1,col-1); (row+1,col); (row+1,col+1) ]
        )
    |> Seq.concat
let symbolMap = Set(symbolMap')

let numberIsTouching symbolMap number (row,startCol) =
    let numberLength = number.ToString().Length
    [startCol .. (startCol+numberLength-1)]
    |> Seq.exists (fun col ->
        Set.contains (row,col) symbolMap 
        )

let numbers =
    File.ReadAllLines ($"__SOURCE_DIRECTORY__/sample3.txt")
    |> Seq.mapi (fun row line ->
        Regex.Matches(line, "(?<Number>\d+)")
        |> Seq.filter (fun mm -> numberIsTouching symbolMap (int mm.Groups["Number"].Value) (row, mm.Groups["Number"].Index))
        |> Seq.map (fun mm ->
            let val' = mm.Groups["Number"].Value 
            (int mm.Groups["Number"].Value)
            )
        )
    |> Seq.concat
    |> Seq.sum
    
// Too high: 544638