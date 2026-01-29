// ADS I Class Project
// Pipelined RISC-V Core - MEM Barrier
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
MEM-Barrier: pipeline register between Memory and Writeback stages

Internal Registers:
    aluResult: computation result (or future load data)
    rd: destination register index
    exception: exception flag

Inputs:
    inAluResult: result from MEM stage
    inRD: destination register from MEM stage
    inException: exception flag from MEM stage

Outputs:
    outAluResult: result to WB stage
    outRD: destination register to WB stage
    outException: exception flag to WB stage

Functionality:
    Save all input signals to a register and output them in the following clock cycle
*/

package core_tile

import chisel3._

// -----------------------------------------
// MEM-Barrier
// -----------------------------------------

class MEMBarrier extends Module {
  val io = IO(new Bundle {
    val inAluResult = Input(UInt(32.W))
    val inRD = Input(UInt(5.W))
    val inException = Input(Bool())

    val outAluResult = Output(UInt(32.W))
    val outRD = Output(UInt(5.W))
    val outException = Output(Bool())
  })

  io.outAluResult := RegNext(io.inAluResult, 0.U)
  io.outRD := RegNext(io.inRD, 0.U)
  io.outException := RegNext(io.inException, false.B)
}