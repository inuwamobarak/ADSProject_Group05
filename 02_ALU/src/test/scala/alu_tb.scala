// ADS I Class Project
// Pipelined RISC-V Core with Hazard Detection and Resolution
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 10/31/2025 by Tobias Jauch (tobias.jauch@rptu.de)

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import Assignment02._

// Test ADD operation
class ALUAddTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Add_Tester" should "test ADD operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      // Basic test
      dut.io.operandA.poke(10.U)
      dut.io.operandB.poke(10.U)
      dut.io.operation.poke(ALUOp.ADD)
      dut.io.aluResult.expect(20.U)
      dut.clock.step(1)

      // Test corner cases
      // 1. Overflow test (modulo 2^32)
      dut.io.operandA.poke("hffffffff".U)  // Max unsigned
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.ADD)
      dut.io.aluResult.expect(0.U)  // Wraps around
      dut.clock.step(1)

      // 2. Zero addition
      dut.io.operandA.poke(42.U)
      dut.io.operandB.poke(0.U)
      dut.io.operation.poke(ALUOp.ADD)
      dut.io.aluResult.expect(42.U)
      dut.clock.step(1)

      // 3. Large numbers
      dut.io.operandA.poke("h12345678".U)
      dut.io.operandB.poke("h9abcdef0".U)
      dut.io.operation.poke(ALUOp.ADD)
      dut.io.aluResult.expect("hacf13568".U)
      dut.clock.step(1)

      // 4. Negative numbers (two's complement)
      dut.io.operandA.poke("hfffffffe".U)  // -2
      dut.io.operandB.poke(3.U)
      dut.io.operation.poke(ALUOp.ADD)
      dut.io.aluResult.expect(1.U)
      dut.clock.step(1)

      // 5. Both operands negative
      dut.io.operandA.poke("hfffffffc".U)  // -4
      dut.io.operandB.poke("hfffffffe".U)  // -2
      dut.io.operation.poke(ALUOp.ADD)
      dut.io.aluResult.expect("hfffffffa".U)  // -6
      dut.clock.step(1)


    }
  }
}

// Test SUB operation
class ALUSubTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Sub_Tester" should "test SUB operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      // Basic subtraction
      dut.io.operandA.poke(20.U)
      dut.io.operandB.poke(10.U)
      dut.io.operation.poke(ALUOp.SUB)
      dut.io.aluResult.expect(10.U)
      dut.clock.step(1)

      // Underflow test
      dut.io.operandA.poke(0.U)
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SUB)
      dut.io.aluResult.expect("hffffffff".U)  // Two's complement wrap
      dut.clock.step(1)

      // Equal numbers
      dut.io.operandA.poke(42.U)
      dut.io.operandB.poke(42.U)
      dut.io.operation.poke(ALUOp.SUB)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)

      // Large numbers
      dut.io.operandA.poke("h9abcdef0".U)
      dut.io.operandB.poke("h12345678".U)
      dut.io.operation.poke(ALUOp.SUB)
      dut.io.aluResult.expect("h88888878".U)
      dut.clock.step(1)

      // Negative result
      dut.io.operandA.poke(5.U)
      dut.io.operandB.poke(10.U)
      dut.io.operation.poke(ALUOp.SUB)
      dut.io.aluResult.expect("hfffffffb".U)  // -5 in two's complement
      dut.clock.step(1)


    }
  }
}

// Test AND operation
class ALUAndTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_And_Tester" should "test AND operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      dut.io.operandA.poke("haa".U)
      dut.io.operandB.poke("h55".U)
      dut.io.operation.poke(ALUOp.AND)
      dut.io.aluResult.expect(0.U)  // aa & 55 = 00
      dut.clock.step(1)
    
      dut.io.operandA.poke("hff".U)
      dut.io.operandB.poke("hff".U)
      dut.io.operation.poke(ALUOp.AND)
      dut.io.aluResult.expect("hff".U)
      dut.clock.step(1)

      // Test with all bits
      dut.io.operandA.poke("hffffffff".U)
      dut.io.operandB.poke("h00000000".U)
      dut.io.operation.poke(ALUOp.AND)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)

      // Test with all bits set
      dut.io.operandA.poke("hffffffff".U)
      dut.io.operandB.poke("hffffffff".U)
      dut.io.operation.poke(ALUOp.AND)
      dut.io.aluResult.expect("hffffffff".U)
      dut.clock.step(1)

    }
  }
}