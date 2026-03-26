// ADS I Class Project
// Pipelined RISC-V Core - EX Stage
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
Instruction Execute (EX) Stage: ALU operations and exception detection

Instantiated Modules:
    ALU: Integrate your module from Assignment02 for arithmetic/logical operations

ALU Interface:
    alu.io.operandA: first operand input
    alu.io.operandB: second operand input
    alu.io.operation: operation code controlling ALU function
    alu.io.aluResult: computation result output

Internal Signals:
    Map uopc codes to ALUOp values

Functionality:
    Map instruction uop to ALU operation code
    Pass operands to ALU
    Output results to pipeline

Outputs:
    aluResult: computation result from ALU
    exception: pass exception flag
*/

package core_tile

import chisel3._
import chisel3.util._
import Assignment02.{ALU, ALUOp}
import uopc._

// -----------------------------------------
// Execute Stage
// -----------------------------------------

class EX extends Module {
  val io = IO(new Bundle {
    val uop = Input(uopc())
    val operandA = Input(UInt(32.W))
    val operandB = Input(UInt(32.W))
    val aluResult = Output(UInt(32.W))
    val xcptInvalid = Input(Bool())
    val outXcptInvalid = Output(Bool())

    // Branch/jump inputs
    val branchTarget = Input(UInt(32.W))   // PC+imm (from ID, for B-type / JAL)
    val isBranch     = Input(Bool())
    val isJump       = Input(Bool())
    val isJALR       = Input(Bool())
    val pc           = Input(UInt(32.W))   // PC of this instruction
    val linkAddr     = Input(UInt(32.W))   // PC+4 for JAL/JALR
 
    // Branch/jump resolution outputs
    val branchTaken   = Output(Bool())     // was branch actually taken?
    val jumpTarget    = Output(UInt(32.W)) // resolved target (for taken branch/jump)
    val doRedirect    = Output(Bool())     // tell IF to redirect
    val flushIFID     = Output(Bool())     // flush IF+ID barriers (mispredicted branch)
    val rdWriteEn     = Output(Bool())     // should rd be written? (false for branches)
    val rdLinkData    = Output(UInt(32.W)) // PC+4 for JAL/JALR to write to rd
 
    // For BTB update
    val btbUpdate        = Output(Bool())
    val btbUpdatePC      = Output(UInt(32.W))
    val btbUpdateTarget  = Output(UInt(32.W))
    val btbMispredicted  = Output(Bool())
    val wasBTBPredTaken  = Input(Bool())   // what the BTB predicted (from ID barrier)

  })

  val alu = Module(new ALU)

  alu.io.operandA := io.operandA
  alu.io.operandB := io.operandB

  alu.io.operation := MuxLookup(io.uop.asUInt, ALUOp.ADD, Seq(
    // R-type
    uopc.ADD.asUInt  -> ALUOp.ADD,
    uopc.SUB.asUInt  -> ALUOp.SUB,
    uopc.AND.asUInt  -> ALUOp.AND,
    uopc.OR.asUInt   -> ALUOp.OR,
    uopc.XOR.asUInt  -> ALUOp.XOR,
    uopc.SLL.asUInt  -> ALUOp.SLL,
    uopc.SRL.asUInt  -> ALUOp.SRL,
    uopc.SRA.asUInt  -> ALUOp.SRA,
    uopc.SLT.asUInt  -> ALUOp.SLT,
    uopc.SLTU.asUInt -> ALUOp.SLTU,

    // I-type
    uopc.ADDI.asUInt  -> ALUOp.ADD,
    uopc.ANDI.asUInt  -> ALUOp.AND,
    uopc.ORI.asUInt   -> ALUOp.OR,
    uopc.XORI.asUInt  -> ALUOp.XOR,
    uopc.SLLI.asUInt  -> ALUOp.SLL,
    uopc.SRLI.asUInt  -> ALUOp.SRL,
    uopc.SRAI.asUInt  -> ALUOp.SRA,
    uopc.SLTI.asUInt  -> ALUOp.SLT,
    uopc.SLTIU.asUInt -> ALUOp.SLTU,

    // JAL/JALR: ALU computes link (PC+4) or jump target
    // For JAL: operandA=PC, operandB=4 → result = PC+4 (link) — but we already have linkAddr
    // For JALR: operandA=rs1, operandB=imm → result = rs1+imm (target)
    uopc.JAL.asUInt   -> ALUOp.ADD,
    uopc.JALR.asUInt  -> ALUOp.ADD,
    // Branches: we don't use ALU result for writeback; comparison done below
    uopc.BEQ.asUInt   -> ALUOp.SUB,
    uopc.BNE.asUInt   -> ALUOp.SUB,
    uopc.BLT.asUInt   -> ALUOp.SLT,
    uopc.BGE.asUInt   -> ALUOp.SLT,
    uopc.BLTU.asUInt  -> ALUOp.SLTU,
    uopc.BGEU.asUInt  -> ALUOp.SLTU 
  ))

  // -------------------------
  // Branch condition evaluation
  // -------------------------
  val actuallyTaken = WireDefault(false.B)
  switch(io.uop) {
    is(uopc.BEQ)  { actuallyTaken := (io.operandA === io.operandB) }
    is(uopc.BNE)  { actuallyTaken := (io.operandA =/= io.operandB) }
    is(uopc.BLT)  { actuallyTaken := (io.operandA.asSInt < io.operandB.asSInt) }
    is(uopc.BGE)  { actuallyTaken := (io.operandA.asSInt >= io.operandB.asSInt) }
    is(uopc.BLTU) { actuallyTaken := (io.operandA < io.operandB) }
    is(uopc.BGEU) { actuallyTaken := (io.operandA >= io.operandB) }
  }
 
  // JALR target: rs1 + imm (with bit[0] cleared per spec)
  val jalrTarget = Cat(alu.io.aluResult(31, 1), 0.U(1.W))
 
  // -------------------------
  // Redirect / flush logic
  // -------------------------
  // Static prediction: branches assumed not-taken, jumps always taken
  // Branch misprediction = branch was actually taken (static) or BTB was wrong (dynamic)
  val branchMispredicted = io.isBranch && actuallyTaken && !io.wasBTBPredTaken ||
                           io.isBranch && !actuallyTaken && io.wasBTBPredTaken
 
  io.branchTaken  := actuallyTaken
  io.doRedirect   := (io.isBranch && actuallyTaken) || (io.isJump && io.isJALR)
  io.flushIFID    := branchMispredicted || (io.isJump && io.isJALR)
  io.jumpTarget   := Mux(io.isJALR, jalrTarget, io.branchTarget)
 
  // -------------------------
  // ALU result / writeback
  // -------------------------
  // For JAL/JALR: write link address (PC+4) to rd
  // For branches: no rd writeback
  // For others: write ALU result
  val isJALorJALR = io.uop === uopc.JAL || io.uop === uopc.JALR
  io.rdWriteEn  := !io.isBranch
  io.rdLinkData := io.linkAddr   // PC+4
 
  // What gets passed to MEM/WB as aluResult:
  // - JAL/JALR: link address (PC+4)
  // - Branch: 0 (not written back)
  // - Others: ALU result
  io.aluResult := MuxCase(alu.io.aluResult, Seq(
    isJALorJALR  -> io.linkAddr,
    io.isBranch  -> 0.U
  ))
 
  io.outXcptInvalid := io.xcptInvalid
 
  // -------------------------
  // BTB update signals
  // -------------------------
  // Send update when a branch is resolved
  io.btbUpdate       := io.isBranch
  io.btbUpdatePC     := io.pc
  io.btbUpdateTarget := io.branchTarget
  io.btbMispredicted := branchMispredicted
}