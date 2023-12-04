open System
open System.IO

let words = Map [
    "one", 1
    "two", 2
    "three", 3
    "four", 4
    "five", 5
    "six", 6
    "seven", 7
    "eight", 8
    "nine", 9
    "1", 1
    "2", 2
    "3", 3
    "4", 4
    "5", 5
    "6", 6
    "7", 7
    "8", 8
    "9", 9
]

module Map =
    let findr (map:Map<'a,_>) (elem:'a) =
        Map.find elem map

module String =
    let IndicesOf (line:string) (word:string) =
        let rec loop (acc:int list) (startIndex:int) =
            let ii = line.IndexOf(word, startIndex)
            if (ii < 0)
            then acc
            else loop (ii::acc) (ii+1)
        loop [] 0 |> Seq.rev

let replaceAll (line:string)  =
    words.Keys
    |> Seq.collect (fun word ->
        String.IndicesOf line word
        |> Seq.map (fun index -> (word, index))) 
    |> Seq.sortBy snd
    |> Seq.map fst
    |> Seq.map (Map.findr words)
    |> Seq.map string
    |> String.concat " "
    
let parseLine (line:string) : int =
    let chars =
        line.ToCharArray ()
        |> Array.choose (fun x ->
            if Char.IsDigit(x)
            then Some x
            else None
            )
    let (first, last) = (chars |> Array.head, chars |> Array.rev |> Array.head)
    Int32.Parse (first.ToString()) * 10 + Int32.Parse(last.ToString())

File.ReadAllLines ($"__SOURCE_DIRECTORY__/sample1.txt")
|> Seq.map replaceAll
|> Seq.map parseLine
|> Seq.sum