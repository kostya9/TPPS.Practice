open System
open System.IO
open EuroDiffusion

let run countries =
    let countries =
        [|for c in countries do
            match c with
            | Diffusion.Success(country) -> yield country
            | Diffusion.Error(e) ->
                failwith e
                |]
        
    let mutable grid =
        countries
        |> Seq.map Diffusion.generateCitiesFromCountry
        |> Seq.collect (fun c -> c)
        |> Diffusion.createSimulationGrid
        
    let mutable iteration = 0
    let mutable completeness = Diffusion.getInitialCompleteness grid (Seq.length countries)
    
    while not completeness.IsFullyCompleted do
        iteration <- iteration + 1
        grid <- Diffusion.updateCurrencies grid
        completeness <- Diffusion.checkCompleteness grid completeness (Seq.length countries) iteration
    
    let iterationByCountry =
        completeness.Cities
        |> Seq.groupBy (fun c -> c.City.Country)
        |> Seq.map(fun t -> fst t, Seq.maxBy (fun (c: Diffusion.CityCompleteState) -> c.Iteration) (snd t))
        |> Seq.sortBy (fun s -> (fst s).Name)
        |> Seq.sortBy (fun s -> (snd s).Iteration)
        
    for (country, iteration) in iterationByCountry do
        Console.WriteLine("[" + country.Name + "]: " + string iteration.Iteration)

let getInput (argv: string[]) =
    if Seq.length argv = 0 then
        Console.In
    else
        let file = System.IO.File.OpenRead(argv.[0])
        new StreamReader(file) :> TextReader 
        

let extractTestCasesFromInput (input: TextReader) =         
    seq {
        let mutable countriesNumber = input.ReadLine() |> int
        let mutable testCase = 1
        while countriesNumber <> 0 do
            if countriesNumber < 1 || countriesNumber > 20 then
                failwith "Expected countries count to be between 1 and 20"
            
            yield testCase, [
                for _ in [1..countriesNumber] do
                    let countryInfo = input.ReadLine()
                    let splitInfo = countryInfo.Split(" ")
                    let name = splitInfo.[0]
                    let xTop = splitInfo.[1] |> int
                    let yTop = splitInfo.[2] |> int
                    let xBot = splitInfo.[3] |> int
                    let yBot = splitInfo.[4] |> int
                    yield Diffusion.createCountry name xTop yTop xBot yBot
            ]
            
            countriesNumber <- input.ReadLine() |> int
            testCase <- testCase + 1
    }

[<EntryPoint>]
let main argv =
    
    use input = getInput argv
    for (case, countries) in extractTestCasesFromInput input do
        Console.WriteLine("Case Number " + string case)
        run countries

    0