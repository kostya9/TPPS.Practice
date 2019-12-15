module EuroDiffusion.Diffusion
open EuroDiffusion
open Result

type Country = {Name: string; XTop: int; YTop: int; XBot: int; YBot: int}

type MotifCoins = {Amount: int; Country: Country}

type City = {X: int; Y: int; Country: Country; Money: MotifCoins[]}
    
let private validateCoordinatesRange x y =
    match (x, y) with
    | (x, _) when x < 1 || x > 10 -> Result.Error("Expected x to be in range of 1..10")
    | (_, y) when y < 1 || y > 10 -> Result.Error("Expected y to be in range of 1..10")
    | (_, _) -> Result.Success()

let private validateCoordinates xBot yBot xTop yTop =
    let validatedBot = validateCoordinatesRange xBot yBot
    let validatedTop = validateCoordinatesRange xTop yTop
    
    match (validatedTop, validatedBot) with
    | (Result.Error(msg), _) -> Result.Error(msg)
    | (_, Result.Error(msg)) -> Result.Error(msg)
    | (Result.Success(), Result.Success()) when xTop < xBot || yTop < yBot -> Error("Expecting xTop >= xBot, yTop >= yBot")
    | (Result.Success(), Result.Success()) -> Success()

let createCountry name xTop yTop xBot yBot =
    let validationResult = validateCoordinates xTop yTop xBot yBot
    
    if Seq.length name > 25 then
        Result.Error("Expected country name to be shorter than 25 characters")
    else
        match validationResult with
        | Result.Success() -> Result.Success({Name = name; XTop = xTop; YTop = yTop; XBot = xBot; YBot = yBot})
        | Result.Error(msg) -> Result.Error(msg)

let generateCitiesFromCountry country =
    seq { for x in country.XTop .. country.XBot do
            for y in country.YTop .. country.YBot do
              yield {X = x; Y = y; Country = country; Money = [|{Amount = 1_000_000; Country = country}|]}
        }

let createSimulationGrid cities =
    let duplicateCities =
        cities
        |> Seq.groupBy (fun c -> (c.X, c.Y))
        |> Seq.where (fun (_, group) -> Seq.length group > 1)
        |> Seq.length
        
    if (duplicateCities > 0) then
        Result.Error("The countries are overlapping, could not proceed to simulation")
    else
        let findCity x y =
            Seq.tryFind (function c -> c.X = x && c.Y = y) cities
            
        Result.Success([for y in 1..10 -> [for x in 1..10 -> findCity x y]])
    
    
let private representativeFactor = 1_000

let private generateNeighbors grid x y =
    seq {
          // Try out each direction to see if that is a valid neighbor
          let directions = [0,1; 0,-1;1,0;-1,0] 
          for (dx, dy) in directions do
              let targetY = y + dy
              if targetY >= 0 && targetY < List.length grid then
                  let row = grid.[targetY]
                  let targetX = x + dx
                  if targetX >= 0 && targetX < List.length row then
                     yield row.[x + dx]
    } |> Seq.choose id
      |> Seq.toArray

let private cityWithUpdatedBudget (grid: City option list list) (x: int) (y: int) =
    
    match grid.[y].[x] with
    | Some city ->
        
        let neighborToRepresentation neighbor =
            Seq.map (fun (m: MotifCoins) -> {Country = m.Country; Amount = m.Amount / representativeFactor}) neighbor.Money
            
        let neighbors = generateNeighbors grid x y

        let cityMoneyMinusRepresentatives =
            city.Money
            |> Seq.map (fun c -> {Country = c.Country; Amount = c.Amount - Seq.length neighbors * (c.Amount/representativeFactor)})
            
        let moneyFromNeighborRepresentatives =
            neighbors
            |> Seq.map neighborToRepresentation
            |> Seq.collect (fun c -> c)
             
        let updatedMoney =
            moneyFromNeighborRepresentatives
            |> Seq.append cityMoneyMinusRepresentatives
            |> Seq.groupBy (fun c -> c.Country)
            |> Seq.map (fun c -> {Country = fst c; Amount = snd c |> Seq.sumBy (fun c -> c.Amount)})
            |> Seq.filter (fun c -> c.Amount > 0)
            |> Seq.sortBy (fun m -> m.Country.Name)
        
        Some ({city with Money = Seq.toArray updatedMoney })
    | None -> None
    
let updateCurrencies (grid: City option list list) =
    List.mapi (fun y row -> List.mapi (fun x _ -> cityWithUpdatedBudget grid x y) row) grid
    
let moneyCompare moneyLeft moneyRight =
    let comparison =
        Seq.compareWith
            (fun (l: MotifCoins) r -> if l.Country = r.Country
                                         && l.Amount = r.Amount
                                         then 0
                                         else -1) moneyLeft moneyRight
    comparison = 0
    
let cityCompare cityLeft cityRight =
    match (cityLeft, cityRight) with
    | (Some(cityLeft), Some(cityRight)) -> if cityLeft.X = cityRight.X
                                              && cityLeft.Y = cityRight.Y
                                              && (moneyCompare cityLeft.Money cityRight.Money)
                                              then 0
                                              else -1 
    | _ -> 0
    
let gridEquals (gridLeft: City option list list) (gridRight: City option list list) =
    let comparison =
        Seq.compareWith
            (fun el1 el2 -> if (Seq.compareWith cityCompare el1 el2) = 0
                            then 0
                            else -1) gridLeft gridRight
    comparison = 0
     
type CityCompleteState = {City: City; Iteration: int}
type GridCompleteState = {Cities: CityCompleteState[]; IsFullyCompleted: bool }

let checkCompleteness (grid: City option list list) (prevState: GridCompleteState) countriesNumber iteration =
    let mutable isFullyCompleted = true
    let cities = [|
        for row in grid do
        for city in Seq.choose id row do
            let completedCity = Seq.tryFind (fun c -> c.City.X = city.X && c.City.Y = city.Y) prevState.Cities 
            match completedCity with
            | Some(state) -> yield state
            | None ->
                if Seq.length city.Money = countriesNumber then
                    // All motifs are represented in the city, so mark it as completed
                    yield {City = city; Iteration = iteration}
                else
                    isFullyCompleted <- false
    |]
    
    {Cities = cities; IsFullyCompleted = isFullyCompleted}

let getInitialCompleteness (grid: City option list list) countriesNumber =
    checkCompleteness grid {Cities = [||]; IsFullyCompleted = false} countriesNumber 0  