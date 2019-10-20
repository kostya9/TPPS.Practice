open EuroDiffusion

[<EntryPoint>]
let main argv =
    let france = Diffusion.createCountry "France" 1 4 4 6
    let spain = Diffusion.createCountry "Spain" 3 1 6 3
    let portugal = Diffusion.createCountry "Portugal" 1 1 2 2
    
    let countries = [|france; spain; portugal|]
    
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
        
    let mutable completeness = Diffusion.createInitialCompleteState ()
    let mutable iteration = 0
    
    while not completeness.IsFullyCompleted do
        grid <- Diffusion.updateCurrencies grid
        completeness <- Diffusion.checkCompleteness grid completeness 3 iteration
        iteration <- iteration + 1
    

    0