//[<AutoOpen>]
//module FsDLX.Tomasulo.Interface
namespace FsDLX.Tomasulo

//type Hardware(memSize:int) =
//    let cdb = CDB()
//    member val Clock            = Clock.GetInstance with get
//    member val PC               = PC.GetInstance with get
//    member val CDB              = cdb with get
//    member val Mem              = Memory.GetInstance memSize
//    member val GPR              = RegisterFile.InitGPR with get
//    member val FPR              = RegisterFile.InitFPR with get
//    member val FunctionalUnits  = FU.InitAll cdb with get
//    
//    new() = Hardware(Config.DefaultMemorySize)
//
//    member h.UpdateReservationStations() =
//        h.FunctionalUnits |> Array.iter (fun fu -> fu.UpdateRS())
            


type Simulator() =
    let cdb = CDB()
    let Clock = Clock.GetInstance
    let mutable PC = 0
    let memory = Memory.GetInstance Config.DefaultMemorySize
    let gpr = RegisterFile.InitGPR
    let fpr = RegisterFile.InitFPR
    let funits = FunctionalUnits() //FU.InitAll()
    let mutable halt = false
    

    let finished() = funits.Finished()

    
    let branchInBranchUnit() = false


    // The write step examines each reservation station for an instruction whose result 
    // is ready to be written to the CDB. More than one instruction may be ready to write 
    // in the same clock cycle. Only one instruction will be permitted to write using the 
    // CDB and others will be stalled. (However, a store or PC update could occur in the 
    // same clock cycle as a write using the CDB since those don't use the CDB.) Notice 
    // that the reservation stations are not updated using the result on the CDB until 
    // after the issue. This is in order to properly simulate the time in which these steps 
    // would occur.
    let write() = None


    // The execute step examines each group of reservation stations. When applied to a group 
    // of reservation stations, the execute will do one of the following:
    //   - begin execution of an instruction if its operands are available and a corresponding 
    //     functional unit is available.
    //   - decrease the execution count of an instruction that is already being executed by 
    //     the corresponding functional unit
    //   - compute the result of an executing instruction if the execution count is 0 and set 
    //     the result and result ready fields of the reservation station.
    let execute() = ()

    // The issue step will examine the opcode of  the instruction and issue the instruction 
    // to the appropriate unit. If each reservation station in the unit is busy, the issue 
    // fails and is reattempted in the next clock cycle.
    let issue (instruction:int) = 
        let k = InstructionKind.ofInt instruction
        let stall = 
            InstructionKind.ofInt instruction |> function
            | Integer ->
                funits.IntegerUnits |> Array.tryFindIndex (fun u -> not(u.Busy))
            | Trap ->
            | Branch ->
            | Memory ->
            | FloatingPoint ->
  
//        (Instruction.ofInt instruction, InstructionKind.ofInt instruction) |> function
//        | i, InstructionKind.Integer ->
//        | i, InstructionKind.Trap ->
//        | i, InstructionKind.Branch ->
//        | i, InstructionKind.Memory ->
//        | i, InstructionKind.FloatingPoint ->


        false


    member s.Run() =
        while not(halt) && not(finished()) do
            // get name of RS writing to CDB and the value to be written
            cdb.Result <- write()
            execute()
            if not(halt) && not(branchInBranchUnit()) then
                let instruction = memory.[PC]
                // stall set to true if issue fails
                let stall = issue(instruction)
                if not(halt) && not(stall) then PC <- PC + 4
            // update RSs using name and value
            updateReservationStations(cdb)

        



