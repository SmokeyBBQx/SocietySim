namespace MeetUp

open Canvas
open Color
open simpleGraph

module Math =
    let clamp v = 
        match v with
        | x when x < 0.0  -> 0.0
        | x when x > 1.0 -> 1.0
        | _ -> v

[<AbstractClass>]
type Participant (name: string, politicalview: float, influenciable: float) =
    // Makes it so you cant necessarily just guess IDs (not e). Ideally you'd make something 
    // completely unique, but then you'd need to keep track of other users... or just hash it.
    let _name : string = name

    static let random = System.Random() 

    let GetRandomNumberJump : int =
        random.Next(0, 20)

    static let mutable _lastCreatedID = random.Next(0, 50)
    do _lastCreatedID <- _lastCreatedID + GetRandomNumberJump

    let _id = _lastCreatedID

    let mutable _dead = false
    let mutable _politicalview = politicalview
    let mutable _influenciable = influenciable

    interface System.IComparable with
        member this.CompareTo something =
            match something with
            | :? Participant as other -> this.Id.CompareTo other.Id
            | _ -> -1
    override this.GetHashCode() = this.Id.GetHashCode()
    override this.Equals(something) =
        match something with
        | :? Participant as other -> this.Id.Equals other.Id
        | _ -> false

    member this.Id = _id

    member this.name = _name

    member this.PoliticalView
        with get() = _politicalview
        and set(v) = 
        _politicalview <- Math.clamp v
   
    // Influenciable is a constant
    member this.Influenciable
        with get() = _influenciable

    member this.dead : bool = _dead
    member this.Kill() = 
        _dead <- true

    /// A score on how well two participants are similar politically
    /// MatchScore is 1.0 if it is a perfect match and 0.0 if the opposite
    /// 
    /// Get difference in score, and convert to 1..0 scale
    // abstract member MatchScore: other: Participant -> float
    abstract member MatchScore : other : Participant -> float
    default this.MatchScore (other: Participant) = 
        let diff = this.PoliticalView-other.PoliticalView
        1.0 - (System.Math.Abs diff)
    /// Calculate how much the political views should change for this 
    /// participant when meeting another.
    abstract member OnInteraction: other:Participant -> isAMatch:bool -> float

[<AbstractClass>]
type Stubborn(name, politicalview, skill : float option) =
    inherit Participant(name, politicalview, 0)
    
    static let random = System.Random() 

    let mutable _skill : float = 
        match skill with
            | Some x -> skill.Value
            | _ -> random.NextDouble()

    member this.Skill
        with get() = _skill
        and set(v) = 
        _skill <- Math.clamp v

    abstract member SpecialInteraction : other : Participant -> unit

    // Stubborn Class can't have their political view changed.
    override this.OnInteraction(other: Participant) (isAMatch: bool) =
        politicalview

type Politician(name, politicalview, skill) =
    inherit Stubborn(name, politicalview, skill)
    
    static let random = System.Random() 

    override this.SpecialInteraction (other: Participant): unit =
        let randomNumber : float = random.NextDouble()
        match other with
        | p when (p :? Stubborn) -> ()
        | _ -> if randomNumber < base.Skill then other.PoliticalView <- politicalview
        
type Terrorist(name, politicalview, skill) =
    inherit Stubborn(name, politicalview, skill)
    
    static let random = System.Random() 

    override this.SpecialInteraction (other: Participant): unit =
        let randomNumber : float = random.NextDouble()
        if randomNumber < base.Skill then other.Kill() else this.Kill()

type Fool(name, politicalview) =
    inherit Participant (name, politicalview, 1)
    
    override this.OnInteraction (other : Participant) (isMatch : bool) : float =
        match other with
        |x when (other :? Stubborn) -> other.PoliticalView
        |_ -> politicalview

// If the match is good, then this should move in the diretion of the other, 
// if it's bad then it should move in the away from other
type Citizen(name: string, politicalview, influenciable) =
    inherit Participant (name, politicalview, influenciable)
    
    override this.OnInteraction (other: Participant) (isAMatch: bool): float = 
            if isAMatch then 
                politicalview + influenciable * (other.PoliticalView - politicalview)
            else 
                politicalview - influenciable * (other.PoliticalView - politicalview)

type State = Participant List * Graph<Participant>

module internal NameGenerator =
    let vowels = [|"a"; "aa"; "e";"ea"; "i"; "o"; "u"; "y"; "æ"; "ø"; "å";"ai";"eo"|]
    let consonants = [|"b";"c";"d";"f";"g";"h";"j";"k";"kk";"l"; "ll";"th";"gn"; "m";"mm";"n";"nn";"p";"q";"r";"s"; "rs";"t";"st";"v";"w";"x";"z"|] 
    let consonantSuffixes = [
        "sen"; "datter"; "gaard"; "løse"; "sted"; "bo";
        "lund"; "holm"; "bjerg"; "dal"; "mark"; "vang"; "skov";
        "høj"; "lundt"; "stad"; "borg"; "strup"; "bæk"; "slev";
        "rup"; "gebra"; "ler"; "stein"; "ein"]
    let vowelSuffixes = [
        "inge"; "um"; "erne"; "ist"; "er";
        "ø"; "e"; "ing"; "inger"; "ium"; "or";
        "a"; "ia"; "ea"]

    let random = System.Random()

    let GetRandomBool (chance: float) : bool =
        if chance > 1 then
            printfn "WARNING: You've used an invalid value for GetRandomBool! The input chance should be clamped between 0.0 and 1.0"
        random.NextDouble() < chance

    let GenerateName() : string =
        if GetRandomBool(0.05) then
            "Bob"
        else
            let NameElementLength = random.Next(4, 14)
            let StartWithVowel : bool = GetRandomBool(0.5)
            let mutable Name = ""
            for i in 1 .. (NameElementLength - 1) do
                if (StartWithVowel && i % 2 = 1) || (not StartWithVowel && i % 2 = 0)then
                    let vowelIndex = random.Next(0, (vowels.Length))
                    let vowel = vowels[vowelIndex]
                    Name <- Name + vowel
                else
                    let consonantIndex = random.Next(0, (consonants.Length))
                    let consonant = consonants[consonantIndex]
                    Name <- Name + consonant
            if GetRandomBool(0.557) && NameElementLength < 7 then
                if (StartWithVowel && Name.Length % 2 = 1) && (not StartWithVowel && Name.Length % 2 = 1) then
                    let vowelIndex = random.Next(0, (vowelSuffixes.Length))
                    let vowel = vowelSuffixes[vowelIndex]
                    Name <- Name + vowel
                else
                    let consonantIndex = random.Next(0, (consonantSuffixes.Length))
                    let consonant = consonantSuffixes[consonantIndex]
                    Name <- Name + consonant
            Name

module internal Simulation =
    let random = System.Random()

    let GenerateParticipant() : Participant =
        let id = random.Next(0, 101)
        let politicalOpinion = random.NextDouble()
        let influenciable = random.NextDouble()
        let name = NameGenerator.GenerateName()

        match id with
            | x when x < 8 -> Politician(name, politicalOpinion, None)
            | x when x > 8 && x < 16 -> Terrorist(name, politicalOpinion, None)
            | x when x > 16 && x < 30 -> Fool(name, politicalOpinion)
            | _ -> Citizen(name, politicalOpinion, influenciable)

    let GenerateParticipants (AmountOfParticipants: uint) : Participant list =
         let mutable participants = []
         for i in 0 .. (int AmountOfParticipants - 1) do
            participants <- (GenerateParticipant()) :: participants
         participants

    let CreateVertices(participants : Participant list) (graph : Graph<Participant>) =
        let mutable g = graph
        for i in 0..(participants.Length-1) do
            g <- g |> addVertex participants[i]
        g

    let CreateEdges(participants : Participant list) (graph : Graph<Participant>) =
        let mutable g = graph
        for i in 0..(participants.Length-1) do
            for j in (i+1)..(participants.Length-1) do
                let matchScore = participants[i].MatchScore(participants[j])
                if matchScore > 0.5 then
                    g <- g |> addEdge participants[i] participants[j]
        g

    let UpdateEdges (participant : Participant) (participants : Participant list) (graph : Graph<Participant>) =
        let mutable g = graph
        for i in 0..(participants.Length-1) do
            let matchScore = participant.MatchScore(participants[i])
            if matchScore > 0.5 then
                g <- g |> addEdge participant participants[i]
            else
                g <- g |> removeEdge participant participants[i]
        g

    let trySpecialInteraction (participant: Participant) (other: Participant) =
     match participant with
        | :? Stubborn as s -> s.SpecialInteraction other
        | _ -> ()

    let HandleInteraction (state: State) =
        let participants = fst state
        let graph = snd state
        
        let aliveParticipants = participants |> List.filter (fun p -> not p.dead)
        if aliveParticipants.Length < 2 then graph else

        let selected = List.randomSample 2 aliveParticipants
        let matchScore = selected[0].MatchScore(selected[1])
        let isMatch = matchScore > 0.5
        
        selected[0].PoliticalView <- selected[0].OnInteraction selected[1] isMatch
        selected[1].PoliticalView <- selected[1].OnInteraction selected[0] isMatch

        trySpecialInteraction selected[0] selected[1]
        trySpecialInteraction selected[1] selected[0]

        graph 
            |> UpdateEdges selected[0] participants
            |> UpdateEdges selected[1] participants

module internal Graphics =
    let maxParticipantSize = 20

    let ConvertDegreesToRadians (angle: float) : float =
        System.Math.PI/180.0*angle

    let GetDistanceFromEdge (PercentageOfWindowToBorder : float) (w: int) (h: int) : float =
        float (System.Math.Min(w,h)) * PercentageOfWindowToBorder

    let GetCoordinates (angle) (i) (radius) : (float * float) =
        let x : float = sin(angle * i) * radius
        let y : float = cos(angle * i) * radius
        x, y

    let DrawParticipant (participant : Participant) (x : float,y : float) (w: int) (h: int) (participantCount: int) (font: Font) =
        let participantSize = System.Math.Min(maxParticipantSize, (System.Math.Min(w,h)) / participantCount)
        let color = 
            if participant.dead then
                fromRgb 100 100 100 
            else
                fromRgb (int (participant.PoliticalView * 255.0)) 0 (int ((1.0-participant.PoliticalView) * 255.0))

        let mutable typeText = 
            match participant with
            | p when (p :? Terrorist) -> "Terrorist"
            | p when (p :? Politician) -> "Politician"
            | p when (p :? Citizen) -> "Citizen"
            | p when (p :? Fool) -> "Fool"
            | _ -> "None"
        typeText <- typeText + "\n" + participant.name

        let centerY= float w/2.0
        let centerX= float h/2.0
        let mutable txt = text white font typeText
        let mutable angle = (atan2 y x)

        txt <- translate (centerX + x/10.0) (centerY + y/10.0) txt
        txt <- rotate 0 0 angle txt

        let politicalViewText = (float (int (participant.PoliticalView*100.0))/100.0).ToString()
        let pVTSize = measureText font politicalViewText
        let mutable politicalViewTree = text white font politicalViewText
        politicalViewTree <- translate (centerX - fst pVTSize / 2.0) (centerY - snd pVTSize / 2.0) politicalViewTree

        let participantShape = translate centerX centerY (filledEllipse color participantSize participantSize)
        politicalViewTree |> onto txt |> onto participantShape

    let DrawGraph (state: State) (w: int) (h: int) (font: Font) =
        let participants = fst state
        let mutable result : PrimitiveTree list = []
        let angleDegrees : float = 360.0/float participants.Length
        let angleRadians = ConvertDegreesToRadians angleDegrees
        let radius : float = float (System.Math.Min(w,h))/2.0 - GetDistanceFromEdge 0.125 w h

        for i in 0..(participants.Length-1) do
            let coords = GetCoordinates angleRadians (float i) radius 
            let mutable participant : PrimitiveTree = DrawParticipant participants[i] coords w h participants.Length font
            participant <- translate (fst coords) (snd coords) participant
            result <- participant::result
        
        for i in 0..(participants.Length-1) do
                let coords = GetCoordinates angleRadians (float i) radius 
                for j in 0..(participants.Length-1) do
                    let otherCoords = GetCoordinates angleRadians (float j) radius
                    if hasEdge participants[i] participants[j] (snd state) then
                        let color = fromRgba 20 230 65 255
                        let mutable line = cubicBezier color 2 (fst coords, snd coords)  (fst coords, snd coords) (fst otherCoords, snd otherCoords) (fst otherCoords, snd otherCoords)
                        line <- translate (float w/2.0) (float h/2.0) line
                        result <- line::result
        
        result

module Main =
    let w, h = 512, 512
    let font : Font = makeFont "arial" 9.0
    let maxParticipants = 20u
    let mutable visualGraph : PrimitiveTree = filledEllipse black 1 1

    let draw state =    
        visualGraph <- filledEllipse black 1 1
        Graphics.DrawGraph state w h font 
        |> List.iter (fun x -> visualGraph <- visualGraph |> onto x)
        visualGraph |> make 

    let react (state: State) (event: Event) = 
        match event with
        | Event.TimerTick ->
            let aliveParticipants = (fst state) |> List.filter (fun p -> not p.dead)
            Some (aliveParticipants, Simulation.HandleInteraction state)
        | _ -> None

    let start (participantAmount: uint) (delayTime: int) = 
        if participantAmount > maxParticipants then 
            printfn "%s %d %s" "WARNING: Max participant amount exceeded! Limited to:" maxParticipants "participants"

        let participants = Simulation.GenerateParticipants(System.Math.Min(maxParticipants, participantAmount))
        let graph = Simulation.CreateVertices participants empty |> Simulation.CreateEdges participants
        interact "animate" w h (Some delayTime) draw react (participants, graph)