module FsDLX.Tomasulo2.FunctionalUnits

module IntegerUnit =
    let issue = 
        InstructionState.Issue 
            (fun x -> false)

module TrapUnit =
    let x = 0

