namespace MeetUp

open Canvas
open simpleGraph

// Math Utils
module Math =
    // Clamps a value between 0.0 and 1.0
    val clamp : v:float -> float

// Abstract base calss for all participants in the simulation
[<AbstractClass>]
type Participant =
    // Creates a new participant with a name, political view (0.0-1.0), and influenciability (0.0-1.0)
    new : name:string * politicalview:float * influenciable:float -> Participant

    // Unique identifier of the participant
    member Id : int

    // Name of the participant
    member name : string

    // Political view value between 0.0 and 1.0
    member PoliticalView : float with get, set

    // How easily influenced this participant is (0.0-1.0)
    member Influenciable : float

    // Whether this participant is dead
    member dead : bool

    // Kills this participant
    member Kill : unit -> unit

    // Calculates how similar two participants are politically (1.0 = perfect match, 0.0 = opposite)
    abstract member MatchScore : other:Participant -> float

    // Calculates how much political views should change when meeting another participant
    abstract member OnInteraction : other:Participant -> isAMatch:bool -> float

    interface System.IComparable

// Abstract base class for participants who cannot be influenced but instead have a special ability
[<AbstractClass>]
type Stubborn =
    inherit Participant

    // Creates a stubborn participant with optional skill level
    new : name:string * politicalview:float * skill:float option -> Stubborn

    // Skill level of this participant (0.0-1.0)
    member Skill : float with get, set

    // Special interaction behavior for stubborn participants. Other may be any type of participant.
    abstract member SpecialInteraction : other:Participant -> unit

// A politician whose special interaction is that they can instantly influence others to their view
type Politician =
    inherit Stubborn

    new : name:string * politicalview:float * skill:float option -> Politician

// A terrorist whose special interaction is tthat they can kill other participants
type Terrorist =
    inherit Stubborn

    new : name:string * politicalview:float * skill:float option -> Terrorist

// A fool who is completely influenced by stubborn participants
type Fool =
    inherit Participant

    new : name:string * politicalview:float -> Fool

// A regular citizen who is influenced by matches
type Citizen =
    inherit Participant

    new : name:string * politicalview:float * influenciable:float -> Citizen

// The state of the simulation
type State = Participant List * Graph<Participant>

// Main simulation module
module Main =
    // Width of the window
    val w : int

    // Height of the window
    val h : int

    // Font for participant names
    val font : Font

    // Max participants
    // Values above the default 20 makes the simulation unstable and unreadable. 
    val maxParticipants : uint

    // Draws the current state of the simulation
    val draw : state:List<Participant> * Graph<Participant> -> Picture

    // Handles events and updates simulation state
    val react : state:State -> event:Event -> State option

    // Starts the simulation with specified number of participants and delay time in milliseconds. 
    // If max participants is exceeded, it will default to maxParticipants.
    val start : participantAmount:uint -> delayTime:int -> unit