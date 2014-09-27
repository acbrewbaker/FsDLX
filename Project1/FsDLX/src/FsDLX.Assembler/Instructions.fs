module FsDLX.Instructions

//ITYPE INSTRUCTIONS
module IType =
    let lookup = 
        [
            "addi", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "addui", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "andi", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "beqz", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "bnez", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "jalr", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "jr", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "lb", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "lbu", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "ld", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "lf", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "lh", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "lhi", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "lhu", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "lw", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "ori", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "sb", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "sd", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "seqi", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "sf", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "sgei", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "sgti", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "sh", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "slei", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "slli", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "slti", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "snei", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "srai", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "srli", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "subi", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "subui", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "sw", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "trap", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
            "xori", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()
        ] |> List.map (fun (o,f) -> (Opcode.String(o), f)) |> Map.ofList<Opcode, InstructionFunction>

//RTYPE INSTRUCTIONS
module RType =
    let lookup = 
        [
            "add", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "addd", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "addf", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "addu", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "and", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "cvtd2f", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "cvtd2i", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "cvtf2d", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "cvtf2i", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "cvti2d", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "cvti2f", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "div", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "divd", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "divf", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "divu", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "movd", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "movf", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "movfp2i", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "movi2fp", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "mult", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "multd", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "multf", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "multu", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "nop", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "or", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "seq", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "sge", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "sgt", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "sle", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "sll", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "slt", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "sne", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "sra", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "srl", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "sub", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "subd", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "subf", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "subu", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
            "xor", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()
        ] |> List.map (fun (o,f) -> (Opcode.String(o), f))|> Map.ofList<Opcode, InstructionFunction>

// JTYPE INSTRUCTIONS
module JType =
    let lookup = 
        [
            "j", fun _ _ _ (name:Immediate) -> ()
            "jal", fun _ _ _ (name:Immediate) -> ()
        ] |> List.map (fun (o,f) -> (Opcode.String(o), f)) |> Map.ofList<Opcode, InstructionFunction>
