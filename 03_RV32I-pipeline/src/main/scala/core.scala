// ADS I Class Project
// Pipelined RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/15/2023 by Tobias Jauch (@tojauch)

/*
The goal of this task is to implement a 5-stage pipeline that features a subset of RV32I (all R-type and I-type instructions). 

    Instruction Memory:
        The CPU has an instruction memory (IMem) with 4096 words, each of 32 bits.
        The content of IMem is loaded from a binary file specified during the instantiation of the MultiCycleRV32Icore module.

    CPU Registers:
        The CPU has a program counter (PC) and a register file (regFile) with 32 registers, each holding a 32-bit value.
        Register x0 is hard-wired to zero.

    Microarchitectural Registers / Wires:
        Various signals are defined as either registers or wires depending on whether they need to be used in the same cycle or in a later cycle.

    Processor Stages:
        The FSM of the processor has five stages: fetch, decode, execute, memory, and writeback.
        All stages are active at the same time and process different instructions simultaneously.

        Fetch Stage:
            The instruction is fetched from the instruction memory based on the current value of the program counter (PC).

        Decode Stage:
            Instruction fields such as opcode, rd, funct3, and rs1 are extracted.
            For R-type instructions, additional fields like funct7 and rs2 are extracted.
            Control signals (isADD, isSUB, etc.) are set based on the opcode and funct3 values.
            Operands (operandA and operandB) are determined based on the instruction type.

        Execute Stage:
            Arithmetic and logic operations are performed based on the control signals and operands.
            The result is stored in the aluResult register.

        Memory Stage:
            No memory operations are implemented in this basic CPU.

        Writeback Stage:
            The result of the operation (writeBackData) is written back to the destination register (rd) in the register file.

    Check Result:
        The final result (writeBackData) is output to the io.check_res signal.
        The exception signal is also passed to the wrapper module. It indicates whether an invalid instruction has been encountered.
        In the fetch stage, a default value of 0 is assigned to io.check_res.
*/

package core_tile

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
import Assignment02.{ALU, ALUOp}
import uopc._


class PipelinedRV32Icore (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    //I/O ports
    val check_res = Output(UInt(32.W))
    val exception = Output(Bool())
  })


  // -----------------------------------------
  // Instantiate modules
  // -----------------------------------------

  val ifStage     = Module(new IF(BinaryFile))
  val ifBarrier   = Module(new IFBarrier)

  val idStage     = Module(new ID)
  val idBarrier   = Module(new IDBarrier)

  val exStage     = Module(new EX)
  val exBarrier   = Module(new EXBarrier)

  val memStage    = Module(new MEM)
  val memBarrier  = Module(new MEMBarrier)

  val wbStage     = Module(new WB)
  val wbBarrier   = Module(new WBBarrier)

  val regFile     = Module(new regFile)

  val forwardingUnit = Module(new ForwardingUnit)

  // -----------------------------------------
  // IF → IF Barrier
  // -----------------------------------------

  ifBarrier.io.inInstr := ifStage.io.instr

  // -----------------------------------------
  // IF Barrier → ID
  // -----------------------------------------

  idStage.io.instr := ifBarrier.io.outInstr

  // -----------------------------------------
  // Register File connections (ID ↔ RF)
  // -----------------------------------------

  regFile.io.req_1 := idStage.io.regReqA
  regFile.io.req_2 := idStage.io.regReqB

  idStage.io.regRespA := regFile.io.resp_1
  idStage.io.regRespB := regFile.io.resp_2

  // -----------------------------------------
  // ID → ID Barrier
  // -----------------------------------------

  idBarrier.io.inUOP          := idStage.io.uop
  idBarrier.io.inRD           := idStage.io.rd
  idBarrier.io.inOperandA     := idStage.io.operandA
  idBarrier.io.inOperandB     := idStage.io.operandB
  idBarrier.io.inXcptInvalid  := idStage.io.xcptInvalid
  idBarrier.io.inRS1 := idStage.io.rs1
  idBarrier.io.inRS2 := idStage.io.rs2

  // -----------------------------------------
  // ID Barrier → EX
  // -----------------------------------------

  exStage.io.uop        := idBarrier.io.outUOP
  exStage.io.operandA  := idBarrier.io.outOperandA
  exStage.io.operandB  := idBarrier.io.outOperandB
  exStage.io.xcptInvalid := idBarrier.io.outXcptInvalid

  // -----------------------------------------
  // EX → EX Barrier
  // -----------------------------------------

  exBarrier.io.inAluResult    := exStage.io.aluResult
  exBarrier.io.inRD           := idBarrier.io.outRD
  exBarrier.io.inXcptInvalid  := exStage.io.outXcptInvalid

    // -----------------------------------------
    // Forwarding Unit
    // -----------------------------------------

    // Connect ID Barrier to Forwarding Unit
    forwardingUnit.io.rs := idBarrier.io.outRS1
    forwardingUnit.io.rt := idBarrier.io.outRS2
    forwardingUnit.io.uop := idBarrier.io.outUOP
    forwardingUnit.io.rd_mem  := exBarrier.io.outRD
    forwardingUnit.io.rd_wb   := wbStage.io.rd_out
    forwardingUnit.io.c_reg_mem := exBarrier.io.outRD =/= 0.U
    forwardingUnit.io.c_reg_wb  := wbStage.io.rd_out =/= 0.U

    val operandA_final = MuxLookup(forwardingUnit.io.forwardA, idBarrier.io.outOperandA, Seq(
    "b10".U -> exBarrier.io.outAluResult, // Forward from MEM stage
    "b01".U -> wbStage.io.aluResult       // Forward from WB stage
    ))

    val operandB_final = MuxLookup(forwardingUnit.io.forwardB, idBarrier.io.outOperandB, Seq(
    "b10".U -> exBarrier.io.outAluResult, // Forward from MEM stage
    "b01".U -> wbStage.io.aluResult       // Forward from WB stage
    ))
    // Connect these final values to the EX stage
    exStage.io.operandA := operandA_final
    exStage.io.operandB := operandB_final

  // -----------------------------------------
  // EX Barrier → MEM
  // -----------------------------------------
  // MEM stage has no logic in this assignment

  memBarrier.io.inAluResult := exBarrier.io.outAluResult
  memBarrier.io.inRD        := exBarrier.io.outRD
  memBarrier.io.inException := exBarrier.io.outXcptInvalid

  // -----------------------------------------
  // MEM Barrier → WB
  // -----------------------------------------

  wbStage.io.aluResult := memBarrier.io.outAluResult
  wbStage.io.rd        := memBarrier.io.outRD

  // -----------------------------------------
  // WB → Register File
  // -----------------------------------------

  regFile.io.req_3 := wbStage.io.regFileReq

  // -----------------------------------------
  // WB → WB Barrier
  // -----------------------------------------

  wbBarrier.io.inCheckRes     := wbStage.io.check_res
  wbBarrier.io.inXcptInvalid := memBarrier.io.outException

  // -----------------------------------------
  // Outputs
  // -----------------------------------------

  io.check_res := wbBarrier.io.outCheckRes
  io.exception := wbBarrier.io.outXcptInvalid
}
