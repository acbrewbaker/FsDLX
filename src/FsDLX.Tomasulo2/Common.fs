namespace FsDLX.Tomasulo2
open FsDLX.Common

type Busy = bool
type Op = Opcode option
type Vj = int
type Vk = int
type Qj = string option
type Qk = string option
type A  = int
type ResultReady = bool
type ResultWritten = bool
type Result = bool
type ReservationStation = Busy * Op * Vj * Vk * Qj * Qk * A * ResultReady * ResultWritten * Result
type Register = int * string option
type RegisterFile =
    | GPR of Register[]
    | FPR of Register[]

type MaxCycles = int
type RemainingCycles = int
type RS = ReservationStation list
type FUnit = MaxCycles * RemainingCycles * Busy * RS ref

type Opcode = 
    | IType of string * int
    | RType of string * int * int
    | JType of string * int

type FunCode = int

type DstReg = 
    | NONE 
    | GPR of int
    | FPR of int

type S1Reg = 
    | NONE 
    | GPR of int
    | FPR of int

type S2Reg = 
    | NONE 
    | GPR of int
    | FPR of int

type Imm =
    | NONE
    | A of (int*int) option

type InstructionInfo = Opcode * FunCode * DstReg * S1Reg * S2Reg * Imm
type InstructionKind =
    | Integer
    | Trap

    static member Identify (i:int) =
        if i = 0 then Integer else Trap

type Instruction =
    | Definition of InstructionKind * InstructionInfo
    | Int of int

type Memory = int[]
type Stall = bool
type Halt = bool

type CDBSrc = string
type CDBResult = int
type CDB = (CDBSrc * CDBResult) option

type Clock = int
type PC = int

type Issue = int -> Stall
type Execute = unit -> unit
type Write = CDB -> CDB

type InstructionState = 
    | Issue of (int -> Stall)
    | Execute of (unit -> unit)
    | Write of (CDB -> CDB)

type FunctionalUnit =
    | IntegerUnit of FUnit[]
    | TrapUnit of FUnit[]

    member fu.Foo (istate:InstructionState) = fu |> function
        | IntegerUnit iunit -> ()
        | TrapUnit tunit -> ()

//
//    static member Identify (i:int) (fu:FunctionalUnits) =
//        InstructionKind.Identify i |> function
//        | Integer -> fu.x
//        | Trap -> fu.[1]
//
//and FunctionalUnits() =
//    member val x = 0

//    | BU of FUnit
//    | MU of FUnit
//    | FPU of FUnit

//    member fu.Issue istate = fu |> function
//        | IU iu -> iu |> Array.tryFindIndex (fun (_,_,busy,_) -> not busy) |> function
//            | Some u ->
//                let intUnit = iu.[u]
//                let maxCC, remCC, busy, RS = intUnit
//                if not busy then
//                    (!RS) |> Array.tryFindIndex (fun r -> not(r.Busy)) |> function
//                    | Some r -> (!RS).[r].Qj <- Some "derp"
//                    | None -> ()
//            | None -> ()
//        | TU tu -> ()



type SimulatorState = Clock * PC * CDB * FunctionalUnit[] * InstructionState
type Log = SimulatorState list

type Simulator = 
    | Halt 
    | Stall
    | Trap
    | Dispatch
    | Log