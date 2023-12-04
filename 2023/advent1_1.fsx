open System
open System.IO

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

let out =
    File.ReadAllLines ($"{__SOURCE_DIRECTORY__}/sample1.txt")
    |> Seq.map (fun x -> parseLine x)
    |> Seq.sum

printfn $"{out}"
