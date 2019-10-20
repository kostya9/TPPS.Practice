module EuroDiffusion.Tests

open EuroDiffusion

open NUnit.Framework
open EuroDiffusion.Diffusion
[<SetUp>]
let Setup () =
    ()
    
let createSeq arr =
    seq {
        for el in arr -> el
    }
    
let createGrid countries =
    let countries =
        [|for c in countries do
            match c with
            | Diffusion.Success(country) -> yield country
            | Diffusion.Error(e) ->
                Assert.Fail(e)
                |]
    
    
    countries, countries
        |> Seq.map Diffusion.generateCitiesFromCountry
        |> Seq.collect (fun c -> c)
        |> Diffusion.createSimulationGrid

[<Test>]
let ShouldUpdateMoney_OfVerticalAdjacentCities () =
    let country1 = Diffusion.createCountry "C1" 1 1 1 1
    let country2 = Diffusion.createCountry "C2" 1 2 1 2
    
    let mutable (countries, grid) = createGrid [country1; country2]
    
        
    grid <- Diffusion.updateCurrencies grid
    
    match grid.[0].[0] with
    | Some(city) ->
        CollectionAssert.AreEqual(city.Money, [{Amount = 999_000; Country = countries.[0]}; {Amount = 1_000; Country = countries.[1]}])
    | None -> Assert.Fail("Expected a valid city")
    
    
[<Test>]
let ShouldUpdateMoney_OfHorizontalAdjacentCities () =
    let country1 = Diffusion.createCountry "C1" 1 1 1 1
    let country2 = Diffusion.createCountry "C2" 2 1 2 1
    
    let mutable (countries, grid) = createGrid [country1; country2]
        
    grid <- Diffusion.updateCurrencies grid
    
    match grid.[0].[0] with
    | Some(city) ->
        CollectionAssert.AreEqual(city.Money, [{Amount = 999_000; Country = countries.[0]}; {Amount = 1_000; Country = countries.[1]}])
    | None -> Assert.Fail("Expected a valid city")
    
[<Test>]
let ShouldBeCompleted_AfterOneIteration_WithTwoAdjacentCountries () =
    let country1 = Diffusion.createCountry "C1" 1 1 1 1
    let country2 = Diffusion.createCountry "C2" 2 1 2 1
    
    let mutable (countries, grid) = createGrid [country1; country2]
    let mutable completed = getInitialCompleteness grid 2
        
    Assert.False(completed.IsFullyCompleted)
        
    grid <- Diffusion.updateCurrencies grid
    let completed = Diffusion.checkCompleteness grid completed 2  1
    
    Assert.True(completed.IsFullyCompleted)
    
[<Test>]
let ShouldBeCompleted_AfterTwoIterations_WithThreeCountriesInRow () =
    let country1 = Diffusion.createCountry "C1" 1 1 1 1
    let country2 = Diffusion.createCountry "C2" 2 1 2 1
    let country3 = Diffusion.createCountry "C3" 3 1 3 1
    
    let mutable (countries, grid) = createGrid [country1; country2; country3]
    let mutable completed = getInitialCompleteness grid 3
        
    Assert.False(completed.IsFullyCompleted)
        
    grid <- Diffusion.updateCurrencies grid
    let completed = Diffusion.checkCompleteness grid completed 3 1
    
    Assert.False(completed.IsFullyCompleted)
    
    grid <- Diffusion.updateCurrencies grid
    let completed = Diffusion.checkCompleteness grid completed 3 2
    
    Assert.True(completed.IsFullyCompleted)