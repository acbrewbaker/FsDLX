namespace FSharpWpfMvvmTemplate.Model

type Expense =
    {   ExpenseType : string
        ExpenseAmount : string }

type InstructionStates =
    { Instruction : string
      Issue : string
      Execute : string
      WriteResult : string
      Cycle : string }
//
//type ReservationStations = 
//    { Name : string }
//
//type GPRRegisterStatusQi =
//    { G0 : string }
