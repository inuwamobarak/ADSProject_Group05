// ADS I Class Project
// Pipelined RISC-V Core - ID Stage
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
Instruction Decode (ID) Stage: decoding and operand fetch

Extracted Fields from 32-bit Instruction (see RISC-V specification for reference):
    opcode: instruction format identifier
    funct3: selects variant within instruction format
    funct7: further specifies operation type (R-type only)
    rd: destination register address
    rs1: first source register address
    rs2: second source register address
    imm: 12-bit immediate value (I-type, sign-extended)

Register File Interfaces:
    regFileReq_A, regFileResp_A: read port for rs1 operand
    regFileReq_B, regFileResp_B: read port for rs2 operand

Internal Signals:
    Combinational decoders for instructions

Functionality:
    Decode opcode to determine instruction and identify operation (ADD, SUB, XOR, ...)
    Output: uop (operation code), rd, operandA (from rs1), operandB (rs2 or immediate)

Outputs:
    uop: micro-operation code (identifies instruction type)
    rd: destination register index
    operandA: first operand
    operandB: second operand 
    XcptInvalid: exception flag for invalid instructions
*/

package core_tile

import chisel3._
import chisel3.util._
import uopc._

// -----------------------------------------
// Decode Stage
// -----------------------------------------

class ID extends Module {
  val io = IO(new Bundle {
    val instr = Input(UInt(32.W))
    val pc    = Input(UInt(32.W))   // PC of this instruction (for branch target calc)

    val regReqA = Output(new regFileReadReq)
    val regReqB = Output(new regFileReadReq)
    val regRespA = Input(new regFileReadResp)
    val regRespB = Input(new regFileReadResp)

    val rs1 = Output(UInt(5.W))
    val rs2 = Output(UInt(5.W))
    
    val uop = Output(uopc())
    val rd  = Output(UInt(5.W))
    val operandA = Output(UInt(32.W))
    val operandB = Output(UInt(32.W))
    val xcptInvalid = Output(Bool())

    // Branch/jump specific
    val branchTarget = Output(UInt(32.W))  // PC-relative target (JAL, B-type)
    val isBranch     = Output(Bool())      // conditional branch
    val isJump       = Output(Bool())      // unconditional jump (JAL/JALR)
    val isJALR       = Output(Bool())      // JALR needs rs1 from regfile
 
    // PC redirect for JAL (can be resolved here)
    val jalRedirect   = Output(Bool())
    val jalTarget     = Output(UInt(32.W))
  })

  val opcode = io.instr(6,0)
  val rd     = io.instr(11,7)
  val funct3 = io.instr(14,12)
  val rs1    = io.instr(19,15)
  val rs2    = io.instr(24,20)
  val funct7 = io.instr(31,25)

  val immI = io.instr(31,20).asSInt.asUInt
  val immB   = Cat(io.instr(31), io.instr(7), io.instr(30, 25), io.instr(11, 8), 0.U(1.W)).asSInt.pad(32).asUInt
  val immJ   = Cat(io.instr(31), io.instr(19, 12), io.instr(20), io.instr(30, 21), 0.U(1.W)).asSInt.pad(32).asUInt
 
  io.rs1 := rs1
  io.rs2 := rs2

  io.regReqA.addr := rs1
  io.regReqB.addr := rs2

  io.operandA := io.regRespA.data
  io.operandB := io.regRespB.data

  io.rd := rd
  io.uop := uopc.NOP
  io.xcptInvalid := false.B

  io.branchTarget := io.pc + immB   // B-type default
  io.isBranch     := false.B
  io.isJump       := false.B
  io.isJALR       := false.B
  io.jalRedirect  := false.B
  io.jalTarget    := io.pc + immJ

  switch(opcode) {

    // -------------------------
    // R-type instructions
    // -------------------------
    is("b0110011".U) {
        switch(funct3) {
        is("b000".U) {
            io.uop := Mux(funct7 === "b0100000".U, uopc.SUB, uopc.ADD)
        }
        is("b001".U) { io.uop := uopc.SLL }
        is("b010".U) { io.uop := uopc.SLT }
        is("b011".U) { io.uop := uopc.SLTU }
        is("b100".U) { io.uop := uopc.XOR }
        is("b101".U) {
            io.uop := Mux(funct7 === "b0100000".U, uopc.SRA, uopc.SRL)
        }
        is("b110".U) { io.uop := uopc.OR }
        is("b111".U) { io.uop := uopc.AND }
        }
    }

    // -------------------------
    // I-type instructions
    // -------------------------
    is("b0010011".U) {
        io.operandB := immI
        switch(funct3) {
        is("b000".U) { io.uop := uopc.ADDI }
        is("b010".U) { io.uop := uopc.SLTI }
        is("b011".U) { io.uop := uopc.SLTIU }
        is("b100".U) { io.uop := uopc.XORI }
        is("b110".U) { io.uop := uopc.ORI }
        is("b111".U) { io.uop := uopc.ANDI }
        is("b001".U) { io.uop := uopc.SLLI }
        is("b101".U) {
            io.uop := Mux(funct7 === "b0100000".U, uopc.SRAI, uopc.SRLI)
        }
        }
    }
    
    // -------------------------
    // B-type instructions
    // -------------------------
    is("b1100011".U) {
      io.isBranch     := true.B
      io.branchTarget := io.pc + immB
      switch(funct3) {
        is("b000".U) { io.uop := uopc.BEQ }
        is("b001".U) { io.uop := uopc.BNE }
        is("b100".U) { io.uop := uopc.BLT }
        is("b101".U) { io.uop := uopc.BGE }
        is("b110".U) { io.uop := uopc.BLTU }
        is("b111".U) { io.uop := uopc.BGEU }
      }
      // Static not-taken: no redirect from ID
      // operandA and operandB are rs1 and rs2 (for comparison in EX)
    }
    
    // -------------------------
    // JAL
    // -------------------------
    is("b1101111".U) {
      io.uop       := uopc.JAL
      io.isJump    := true.B
      // rd gets PC+4 (link address) — pass as operandB; operandA = PC
      io.operandA  := io.pc
      io.operandB  := 4.U
      // JAL target is PC-relative, known here
      io.jalRedirect := true.B
      io.jalTarget   := io.pc + immJ
    }
 
    // -------------------------
    // JALR
    // -------------------------
    is("b1100111".U) {
      io.uop    := uopc.JALR
      io.isJump := true.B
      io.isJALR := true.B
      // rd gets PC+4; rs1 + imm is the target (computed in EX)
      // operandA = rs1 (for target addr), operandB = imm
      // We'll also pass PC+4 as the link value via a separate path
      // For simplicity: ALU computes rs1+imm (target), link stored via extra signal
      io.operandA := io.regRespA.data
      io.operandB := immI
      // JALR redirect resolved in EX stage
    }
  }

    // -------------------------
    // Invalid instruction
    // -------------------------
    when(
     opcode =/= "b0110011".U &&
     opcode =/= "b0010011".U &&
     opcode =/= "b1100011".U &&
     opcode =/= "b1101111".U &&
     opcode =/= "b1100111".U
     ) {
     io.xcptInvalid := true.B
    }
}
