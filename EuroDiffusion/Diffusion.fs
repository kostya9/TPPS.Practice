module EuroDiffusion.Diffusion

type Country = {Name: string; XTop: int; YTop: int; XBot: int; YBot: int}

type MotifCoins = {Amount: int; Country: Country}

type City = {X: int; Y: int; Country: Country; Money: seq<MotifCoins>}

type Result<'T> =
    | Error of string
    | Success of 'T
    
let private validateCoordinatesRange x y =
    match (x, y) with
    | (x, y) when x < 1 || x > 10 -> Error("Expected x to be in range of 1..10")
    | (x, y) when y < 1 || y > 10 -> Error("Expected y to be in range of 1..10")
    | (x, y) -> Success()

let private validateCoordinates xBot yBot xTop yTop =
    let validatedBot = validateCoordinatesRange xBot yBot
    let validatedTop = validateCoordinatesRange xTop yTop
    
    match (validatedTop, validatedBot) with
    | (Error(msg), _) -> Error(msg)
    | (_, Error(msg)) -> Error(msg)
    | (Success(), Success()) when xTop < xBot || yTop < yBot -> Error("Expecting xTop >= xBot, yTop >= yBot")
    | (Success(), Success()) -> Success()

let createCountry name xTop yTop xBot yBot =
    let validationResult = validateCoordinates xTop yTop xBot yBot
    
    if Seq.length name > 25 then
        Error("Expected country name to be shorter than 25 characters")
    else
        match validationResult with
        | Success() -> Success({Name = name; XTop = xTop; YTop = yTop; XBot = xBot; YBot = yBot})
        | Error(msg) -> Error(msg)

let generateCitiesFromCountry country =
    seq { for x in country.XTop .. country.XBot do
            for y in country.YTop .. country.YBot do
              yield {X = x; Y = y; Country = country; Money = [{Amount = 1_000_000; Country = country}]}
        }

let createSimulationGrid cities =
    let findCity x y =
        Seq.tryFind (function c -> c.X = x && c.Y = y) cities
        
    [for y in 1..10 -> [for x in 1..10 -> findCity x y]]
    
let private cityWithUpdatedBudget (grid: City option list list) (x: int) (y: int) =
    match grid.[y].[x] with
    | Some city ->
        
        let neighborToRepresentation neighbor =
            Seq.map (fun (m: MotifCoins) -> {Country = m.Country; Amount = m.Amount / 1000}) neighbor.Money
            
        let neighbors =
                  seq {
                          for (dx, dy) in [0,1; 0,-1;1,0;-1,0] do
                              let targetY = y + dy
                              if targetY >= 0 && targetY < List.length grid then
                                  let row = grid.[targetY]
                                  let targetX = x + dx
                                  if targetX >= 0 && targetX < List.length row then
                                     yield row.[x + dx]
                  } |> Seq.choose id
                    |> Seq.toArray

        let cityMoneyMinusRepresentatives =
            city.Money
            |> Seq.map (fun c -> {Country = c.Country; Amount = c.Amount - Seq.length neighbors * (c.Amount/1000)})
             
        let updatedMoney =
            neighbors
            |> Seq.map neighborToRepresentation
            |> Seq.collect (fun c -> c)
            |> Seq.append cityMoneyMinusRepresentatives
            |> Seq.groupBy (fun c -> c.Country)
            |> Seq.map (fun c -> snd c)
            |> Seq.map (fun c -> {Country = (Seq.head c).Country; Amount = Seq.sumBy (fun c -> c.Amount) c})
            |> Seq.filter (fun c -> c.Amount > 0)
        
        Some ({city with Money = Seq.toArray updatedMoney })
    | None -> None
    
let updateCurrencies (grid: City option list list) =
    List.mapi (fun y r -> List.mapi (fun x c -> cityWithUpdatedBudget grid x y) grid.[y]) grid
    
    
type StateOfComplete<'T> = {Item: 'T; Iteration: int}
type GridCompleteState = {Cities: seq<StateOfComplete<City>>; IsFullyCompleted: bool }

let checkCompleteness (grid: City option list list) (prevState: GridCompleteState) countriesNumber iteration =
    let mutable isFullyCompleted = true
    let cities = [|
        for row in grid do
            for city in Seq.choose id row do
                match Seq.tryFind (fun c -> c.Item.X = city.X && c.Item.Y = city.Y) prevState.Cities with
                | Some(state) -> yield state
                | None ->
                    if Seq.length city.Money = countriesNumber then
                        yield {Item = city; Iteration = iteration}
                    else
                        isFullyCompleted <- false
    |]
    
    {Cities = cities; IsFullyCompleted = isFullyCompleted}

let getInitialCompleteness (grid: City option list list) countriesNumber =
    checkCompleteness grid {Cities = []; IsFullyCompleted = false} countriesNumber 0


    
                
            
                