module FsDLX.Assembler.Tokens

type Comment = 
    | COMMENTONLY

module Directive =
    type T = 
        | TEXT
        | DATA
        | ALIGN
        | ASCIIZ
        | DOUBLE
        | FLOAT
        | WORD
        | SPACE

module Register =
    type T = 
        | R
        | F

module BasePlusOffset =
    type T = 
        | VALPLUSREG
        | VALPLUSLABEL
        | REGPLUSVAL
        | REGPLUSLABEL

module Label =
    type T = 
        | NEW
        | INLINE

module Immediate =
    type T = 
        | VALUE
        | LABEL
        | REGISTER
        | BASEPLUSOFFSET

module Operand =
    type T = 
        | RS1
        | RS2
        | RD
        | IMMEDIATE

module Opcode =     
    type IType = 
        | BEQZ
        | BNEZ
        | ADDI
        | ADDUI
        | SUBI
        | SUBUI
        | ANDI
        | ORI
        | XORI
        | LHI
        | TRAP
        | JR
        | JALR
        | SLLI
        | SRLI
        | SRAI
        | SEQI
        | SNEI
        | SLTI
        | SGTI
        | SLEI
        | SGEI
        | LB
        | LH
        | LW
        | LBU
        | LHU
        | LF
        | LD
        | SB
        | SH
        | SW
        | SF
        | SD
    
    type RType = 
        | NOP
        | SLL
        | SRL
        | SRA
        | ADD
        | ADDU
        | SUB
        | SUBU
        | AND
        | OR
        | XOR
        | SEQ
        | SNE
        | SLT
        | SGT
        | SLE
        | SGE
        | MOVF
        | MOVD
        | MOVFP2I
        | MOVI2FP
        | ADDF
        | SUBF
        | MULTF
        | DIVF
        | ADDD
        | SUBD
        | MULTD
        | DIVD
        | CVTF2D
        | CVTF2I
        | CVTD2F
        | CVTD2I
        | CVTI2F
        | CVTI2D
        | MULT
        | DIV
        | MULTU
        | DIVU
    
    type JType = 
        | J
        | JAL
