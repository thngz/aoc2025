open System.Text.RegularExpressions
// open System.Math

module Day1 =
    type Direction =
        | L
        | R

    type Instruction = { direction: Direction; turns: int }

    let getNumFromStr (input: string) : int =
        let m = Regex.Match(input, @"\d+")
        if m.Success then m.Value |> int else failwith "No int"


    let getPart1: int =
        let input = System.IO.File.ReadAllLines("./day1/input.txt") |> Array.toList
        // let input = [ "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82"; "L201" ]

        let instructions: List<Instruction> =
            input
            |> List.map (fun x ->
                match x with
                | ins when ins.StartsWith("L") ->
                    { direction = L
                      turns = getNumFromStr ins }
                | ins when ins.StartsWith("R") ->
                    { direction = R
                      turns = getNumFromStr ins }
                | _ -> failwith "Bruh what")

        
        let applyRotation (n: int) (by: int) =
            let rem = snd (System.Math.DivRem(n + by, 100))
            
            if rem < 0 then rem + 100 else rem

        snd (
            List.fold
                (fun acc ins ->
                    match ins.direction with
                    | L ->
                        let rotation = applyRotation (fst acc) -ins.turns
                        (rotation, if rotation = 0 then snd acc + 1 else snd acc)
                    | R ->
                        let rotation = applyRotation (fst acc) ins.turns
                        (rotation, if rotation = 0 then snd acc + 1 else snd acc))
                (50, 0)
                instructions
        )

    // -----------------------------------------PART 2-----------------------------------------------------

    let getPart2: int =
        let input = System.IO.File.ReadAllLines("./day1/input.txt") |> Array.toList
        // let input =
        //     [ "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82"; ]

        let instructions: List<Instruction> =
            input
            |> List.map (fun x ->
                match x with
                | ins when ins.StartsWith("L") ->
                    { direction = L
                      turns = getNumFromStr ins }
                | ins when ins.StartsWith("R") ->
                    { direction = R
                      turns = getNumFromStr ins }
                | _ -> failwith "Bruh what")

        let applyRotationAndGetCrossings (n: int) (by: int): (int * int) =
            let rem = (n + by) % 100
            let nextPos = if rem < 0 then rem + 100 else rem

            let crossings =
                if by > 0 then 
                    let dist = if n = 0 then 100 else 100 - n
                    if by >= dist then 1 + (by - dist) / 100 else 0
                else
                    let dist = if n = 0 then 100 else n
                    let absBy = -by
                    if absBy >= dist then 1 + (absBy - dist) / 100 else 0
            (nextPos, crossings)

        // 6153 too high
        snd (
            List.fold
                (fun acc ins ->
                    match ins.direction with
                    | L ->
                        let rotation, crossings = applyRotationAndGetCrossings (fst acc) -ins.turns
                        (rotation,  snd acc + crossings)
                    | R ->
                        let rotation, crossings = applyRotationAndGetCrossings (fst acc) ins.turns

                        (rotation,  snd acc + crossings))
                (50, 0)
                instructions
        )
