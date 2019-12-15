open System
open System.IO
open EuroDiffusion
open EuroDiffusion.Diffusion
open Result

let runIterations countries initialGrid =
    let countryCount = Seq.length countries
    
    let mutable grid = initialGrid
    let mutable iteration = 0
    let mutable completeness = Diffusion.getInitialCompleteness grid countryCount
    let mutable stuck = false
    
    while not completeness.IsFullyCompleted && not stuck do
        iteration <- iteration + 1
        let prevGrid = grid
        grid <- Diffusion.updateCurrencies grid
        
        if (Diffusion.gridEquals prevGrid grid) then
            stuck <- true
        else
            completeness <- Diffusion.checkCompleteness grid completeness countryCount iteration
            
    if stuck
    then Result.Error("The simulation is stuck, please check the input")
    else Result.Success(completeness)
    
let getLastCompletedCity completedCities =
    Seq.maxBy (fun (c: Diffusion.CityCompleteState) -> c.Iteration) completedCities
    
let displayResult completenessResult =
    match completenessResult with
    | Error(errorMsg) -> Console.WriteLine("Could not complete simulation due to error: {0}", errorMsg)
    | Success(completeness: GridCompleteState) ->
        let iterationByCountry =
            completeness.Cities
            |> Seq.groupBy (fun c -> c.City.Country)
            |> Seq.map(fun (country, completedCities) -> country, getLastCompletedCity completedCities)
            |> Seq.sortBy (fun (country, lastCity) -> lastCity.Iteration, country.Name)
                    
        for (country, iteration) in iterationByCountry do
            Console.WriteLine("[" + country.Name + "]: " + string iteration.Iteration)

let runSimulation countries =
    countries
        |> Seq.map Diffusion.generateCitiesFromCountry
        |> Seq.collect (fun c -> c)
        |> Diffusion.createSimulationGrid
        |> Result.bind (runIterations countries)
        |> displayResult

let readCountries (input: TextReader) countriesCount = 
    [| for _ in [1..countriesCount] do
        let countryInfo = input.ReadLine()
        let splitInfo = countryInfo.Split(" ")
        let name = splitInfo.[0]
        let xTop = splitInfo.[1] |> int
        let yTop = splitInfo.[2] |> int
        let xBot = splitInfo.[3] |> int
        let yBot = splitInfo.[4] |> int
        yield Diffusion.createCountry name xTop yTop xBot yBot|]

let readCase (input: TextReader) =
    try
        let countriesCount = input.ReadLine() |> int
    
        if countriesCount = 0 then
            Result.Success(None)
        else if countriesCount < 1 || countriesCount > 20 then
            Result.Error("Expected countries count to be between 1 and 20")
        else
            let countries = readCountries input countriesCount
            
            // If at least one country is invalid -> disregard all countries
            match Seq.tryFind Result.isError countries with
            | Some(Result.Error(e)) -> Result.Error(e)
            | None -> Result.Success(Some(Array.choose chooseSuccess countries))
            | _ -> Result.Error("Unexpected Some(Success)")
    with
        | e -> Result.Error("Input format for the test case was incorrect")

let extractTestCasesFromInput (input: TextReader) =
    Seq.initInfinite (fun c -> c)
    |> Seq.takeWhile (fun _ -> input.Peek() <> -1)
    |> Seq.map (fun _ -> readCase input)
    |> Seq.takeWhile isSomeOrError
    |> Seq.choose Result.chooseSomeOrError
    |> Seq.mapi (fun i c -> (i + 1, c))
    |> Seq.toArray

let getInput (argv: string[]) =
    if Seq.length argv = 0 then
        Console.In
    else
        let file = System.IO.File.OpenRead(argv.[0])
        new StreamReader(file) :> TextReader

[<EntryPoint>]
let main argv =
    use input = getInput argv
    
    for (caseNumber, case) in extractTestCasesFromInput input do
        Console.WriteLine("Case Number " + string caseNumber)
        match case with
        | Result.Success(countries) ->
            runSimulation countries
        | Result.Error(e) ->
            Console.WriteLine("Could not run case due to error: {0}", e)

    0