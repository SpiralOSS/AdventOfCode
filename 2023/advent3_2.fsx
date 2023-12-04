open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let neighbor_gear = Dictionary<_,_>()
let gear_numbers = Dictionary<_,_>()

File.ReadAllLines ($"{__SOURCE_DIRECTORY__}/sample3.txt")
|> Seq.mapi (fun row line ->
    line.ToCharArray ()
    |> Seq.mapi (fun col chr ->
        if chr = '*'
        then Some (row, col)
        else None
        )
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> Seq.iter (fun (row,col) ->
        [ (row-1,col-1); (row-1,col); (row-1,col+1)   
          (row  ,col-1);              (row  ,col+1)   
          (row+1,col-1); (row+1,col); (row+1,col+1) ]
        |> List.iter (fun neighbor ->
            neighbor_gear[neighbor] <- (row,col)
            gear_numbers[(row,col)] <- ([]:int list))
        )
    )
|> Seq.iter (fun _ -> ())

let updateGears number (row:int,startCol) =
    let numberLength = number.ToString().Length
    [startCol .. (startCol+numberLength-1)]
    |> Seq.map (fun col -> (row,col))
    |> Seq.filter neighbor_gear.ContainsKey
    |> Seq.truncate 1
    |> Seq.map (fun (row,col) -> neighbor_gear[(row,col)])
    |> Seq.iter (fun (row,col) ->
        gear_numbers[(row,col)] <- number :: gear_numbers[(row,col)] 
        )

File.ReadAllLines ($"{__SOURCE_DIRECTORY__}/sample3.txt")
|> Seq.mapi (fun row line ->
    Regex.Matches(line, "(?<Number>\d+)")
    |> Seq.iter (fun mm -> updateGears (int mm.Groups["Number"].Value) (row, mm.Groups["Number"].Index))
    )
|> Seq.iter (fun _ -> ())

let out =
    gear_numbers
    |> Seq.filter (fun kv -> kv.Value.Length > 1)
    |> Seq.map (fun kv ->
        (1, kv.Value)
        ||> Seq.fold (fun acc num -> acc * num)
        )
    |> Seq.sum

printfn $"{out}"
