// ADS I Class Project
// Pipelined RISC-V Core - ID Barrier
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
ID-Barrier: pipeline register between Decode and Execute stages

Internal Registers:
    uop: micro-operation code (from uopc enum)
    rd: destination register index, initialized to 0
    operandA: first source operand, initialized to 0
    operandB: second operand/immediate, initialized to 0

Inputs:
    inUOP: micro-operation code from ID stage
    inRD: destination register from ID stage
    inOperandA: first operand from ID stage
    inOperandB: second operand/immediate from ID stage
    inXcptInvalid: exception flag from ID stage

Outputs:
    outUOP: micro-operation code to EX stage
    outRD: destination register to EX stage
    outOperandA: first operand to EX stage
    outOperandB: second operand to EX stage
    outXcptInvalid: exception flag to EX stage
Functionality:
    Save all input signals to a register and output them in the following clock cycle
*/

package core_tile

import chisel3._
import uopc._

// -----------------------------------------
// ID-Barrier
// -----------------------------------------

class IDBarrier extends Module {
  val io = IO(new Bundle {
    val inUOP = Input(uopc())
    val inRD  = Input(UInt(5.W))
    val inOperandA = Input(UInt(32.W))
    val inOperandB = Input(UInt(32.W))
    val inXcptInvalid = Input(Bool())

    val inRS1 = Input(UInt(5.W))
    val inRS2 = Input(UInt(5.W))
    val inPC  = Input(UInt(32.W))
    val inImm = Input(UInt(32.W))

    val flush = Input(Bool())

    val outUOP = Output(uopc())
    val outRD  = Output(UInt(5.W))
    val outOperandA = Output(UInt(32.W))
    val outOperandB = Output(UInt(32.W))
    val outXcptInvalid = Output(Bool())

    val outRS1 = Output(UInt(5.W))
    val outRS2 = Output(UInt(5.W))
    val outPC  = Output(UInt(32.W))
    val outImm = Output(UInt(32.W))
  })

  io.outUOP := RegNext(Mux(io.flush, uopc.NOP, io.inUOP), uopc.NOP)
  io.outRD  := RegNext(Mux(io.flush, 0.U, io.inRD), 0.U)
  io.outOperandA := RegNext(Mux(io.flush, 0.U, io.inOperandA), 0.U)
  io.outOperandB := RegNext(Mux(io.flush, 0.U, io.inOperandB), 0.U)
  io.outXcptInvalid := RegNext(Mux(io.flush, false.B, io.inXcptInvalid), false.B)

  io.outRS1 := RegNext(Mux(io.flush, 0.U, io.inRS1), 0.U)
  io.outRS2 := RegNext(Mux(io.flush, 0.U, io.inRS2), 0.U)
  io.outPC  := RegNext(Mux(io.flush, 0.U, io.inPC), 0.U)
  io.outImm := RegNext(Mux(io.flush, 0.U, io.inImm), 0.U)
}