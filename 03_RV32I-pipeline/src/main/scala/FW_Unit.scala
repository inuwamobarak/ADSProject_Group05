package core_tile

import chisel3._
import chisel3.util._
import uopc._

class ForwardingUnit extends Module {
    val io = IO(new Bundle {
    // Data_in to FU
    val rs     = Input(UInt(5.W)) 
    val rt     = Input(UInt(5.W))
    val rd_mem     = Input(UInt(5.W))
    val rd_wb     = Input(UInt(5.W))
    val uop       = Input(uopc()) //only R-type uses rt as a register operand
    // Control signal to  FU
    val c_reg_mem = Input(Bool())
    val c_reg_wb  = Input(Bool())
    // Select values for the MUXs
    val forwardA    = Output(UInt(2.W)) // 00: ID/EX, 01: WB, 10: MEM
    val forwardB    = Output(UInt(2.W))
  })

  // Default: No forwarding
  io.forwardA := 0.U
  io.forwardB := 0.U
  
  // Forwarding for Operand A
  when(io.c_reg_mem && (io.rd_mem =/= 0.U) && (io.rs =/= 0.U) && (io.rs === io.rd_mem)){
    io.forwardA := 2.U 
  }.elsewhen(io.c_reg_wb && (io.rd_wb =/= 0.U) && (io.rs =/= 0.U) && (io.rs === io.rd_wb)) {
    io.forwardA := 1.U
  }.otherwise {
    io.forwardA := 0.U // "b00" -> No forwarding
}

  // Forwarding for Operand B - only for R-type instructions
  val isRType = io.uop === uopc.ADD || io.uop === uopc.SUB || io.uop === uopc.AND || 
                io.uop === uopc.OR || io.uop === uopc.XOR || io.uop === uopc.SLL ||
                io.uop === uopc.SRL || io.uop === uopc.SRA || io.uop === uopc.SLT ||
                io.uop === uopc.SLTU
                
  when(isRType && io.c_reg_mem && (io.rd_mem =/= 0.U) && (io.rt =/= 0.U) && (io.rt === io.rd_mem)) {
    io.forwardB := 2.U
  } .elsewhen(isRType && io.c_reg_wb && (io.rd_wb =/= 0.U) && (io.rt =/= 0.U) && (io.rt === io.rd_wb)) {
    io.forwardB := 1.U
  } .otherwise {
    io.forwardB := 0.U // "b00" -> No forwarding
}
}