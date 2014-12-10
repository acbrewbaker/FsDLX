module FsDLX.Tomasulo.Instruction

type Instruction =
    {
        Opcode      : Opcode
        FunCode     : int
        DstReg      : string
        //DstRegBit   : DstRegBit
        S1Reg       : string
        S1RegBit    : int
        S2Reg       : string
        S2RegBit    : int
        ImmedField  : bool
        ImmedFieldStartBit  : int
        ImmedFieldEndBit  : int

    }

