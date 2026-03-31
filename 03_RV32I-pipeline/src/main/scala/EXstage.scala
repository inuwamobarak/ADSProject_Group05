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

class EX extends Module {
  val io = IO(new Bundle {
    val uop = Input(uopc())
    val operandA = Input(UInt(32.W))
    val operandB = Input(UInt(32.W))
    val pc = Input(UInt(32.W))
    val imm = Input(UInt(32.W))

    val aluResult = Output(UInt(32.W))
    val writeBackData = Output(UInt(32.W))

    val xcptInvalid = Input(Bool())
    val outXcptInvalid = Output(Bool())

    val branchTaken = Output(Bool())
    val branchTarget = Output(UInt(32.W))
  })

  val alu = Module(new ALU)

  alu.io.operandA := io.operandA
  alu.io.operandB := io.operandB

  alu.io.operation := MuxLookup(io.uop.asUInt, ALUOp.ADD, Seq(
    uopc.ADD.asUInt   -> ALUOp.ADD,
    uopc.SUB.asUInt   -> ALUOp.SUB,
    uopc.AND.asUInt   -> ALUOp.AND,
    uopc.OR.asUInt    -> ALUOp.OR,
    uopc.XOR.asUInt   -> ALUOp.XOR,
    uopc.SLL.asUInt   -> ALUOp.SLL,
    uopc.SRL.asUInt   -> ALUOp.SRL,
    uopc.SRA.asUInt   -> ALUOp.SRA,
    uopc.SLT.asUInt   -> ALUOp.SLT,
    uopc.SLTU.asUInt  -> ALUOp.SLTU,

    uopc.ADDI.asUInt  -> ALUOp.ADD,
    uopc.ANDI.asUInt  -> ALUOp.AND,
    uopc.ORI.asUInt   -> ALUOp.OR,
    uopc.XORI.asUInt  -> ALUOp.XOR,
    uopc.SLLI.asUInt  -> ALUOp.SLL,
    uopc.SRLI.asUInt  -> ALUOp.SRL,
    uopc.SRAI.asUInt  -> ALUOp.SRA,
    uopc.SLTI.asUInt  -> ALUOp.SLT,
    uopc.SLTIU.asUInt -> ALUOp.SLTU
  ))

  io.aluResult := alu.io.aluResult
  io.writeBackData := alu.io.aluResult
  io.outXcptInvalid := io.xcptInvalid

  io.branchTaken := false.B
  io.branchTarget := 0.U

  switch(io.uop) {
    is(uopc.BEQ) {
      io.branchTaken := io.operandA === io.operandB
      io.branchTarget := io.pc + io.imm
    }

    is(uopc.BNE) {
      io.branchTaken := io.operandA =/= io.operandB
      io.branchTarget := io.pc + io.imm
    }

    is(uopc.BLT) {
      io.branchTaken := io.operandA.asSInt < io.operandB.asSInt
      io.branchTarget := io.pc + io.imm
    }

    is(uopc.BGE) {
      io.branchTaken := io.operandA.asSInt >= io.operandB.asSInt
      io.branchTarget := io.pc + io.imm
    }

    is(uopc.BLTU) {
      io.branchTaken := io.operandA < io.operandB
      io.branchTarget := io.pc + io.imm
    }

    is(uopc.BGEU) {
      io.branchTaken := io.operandA >= io.operandB
      io.branchTarget := io.pc + io.imm
    }

    is(uopc.JAL) {
      io.branchTaken := true.B
      io.branchTarget := io.pc + io.imm
      io.writeBackData := io.pc + 4.U
    }

    is(uopc.JALR) {
      io.branchTaken := true.B
      io.branchTarget := (io.operandA + io.imm) & "hfffffffe".U
      io.writeBackData := io.pc + 4.U
    }
  }
}