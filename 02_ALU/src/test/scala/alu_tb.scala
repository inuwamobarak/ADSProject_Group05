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

// Test OR operation
class ALUOrTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Or_Tester" should "test OR operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      // Basic OR
      dut.io.operandA.poke("haa".U)
      dut.io.operandB.poke("h55".U)
      dut.io.operation.poke(ALUOp.OR)
      dut.io.aluResult.expect("hff".U)  // aa | 55 = ff
      dut.clock.step(1)

      // All ones with zeros
      dut.io.operandA.poke("hffffffff".U)
      dut.io.operandB.poke("h00000000".U)
      dut.io.operation.poke(ALUOp.OR)
      dut.io.aluResult.expect("hffffffff".U)
      dut.clock.step(1)

      // Test with specific bit patterns
      dut.io.operandA.poke("h12345678".U)
      dut.io.operandB.poke("h87654321".U)
      dut.io.operation.poke(ALUOp.OR)
      dut.io.aluResult.expect("h97755779".U)
      dut.clock.step(1)

      // OR with same value
      dut.io.operandA.poke("hdeadbeef".U)
      dut.io.operandB.poke("hdeadbeef".U)
      dut.io.operation.poke(ALUOp.OR)
      dut.io.aluResult.expect("hdeadbeef".U)
      dut.clock.step(1)

    }
  }
}

// Test XOR operation
class ALUXorTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Xor_Tester" should "test XOR operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      // Basic XOR
      dut.io.operandA.poke("haa".U)
      dut.io.operandB.poke("h55".U)
      dut.io.operation.poke(ALUOp.XOR)
      dut.io.aluResult.expect("hff".U)  // aa ^ 55 = ff
      dut.clock.step(1)

      // XOR with same value (should be zero)
      dut.io.operandA.poke("hdeadbeef".U)
      dut.io.operandB.poke("hdeadbeef".U)
      dut.io.operation.poke(ALUOp.XOR)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)

      // XOR with all ones (bitwise NOT)
      dut.io.operandA.poke("h12345678".U)
      dut.io.operandB.poke("hffffffff".U)
      dut.io.operation.poke(ALUOp.XOR)
      dut.io.aluResult.expect("hedcba987".U)  // bitwise NOT of operandA
      dut.clock.step(1)

      // Test toggle bits
      dut.io.operandA.poke("h0000ffff".U)
      dut.io.operandB.poke("hffff0000".U)
      dut.io.operation.poke(ALUOp.XOR)
      dut.io.aluResult.expect("hffffffff".U)
      dut.clock.step(1)

    }
  }
}

// Test SLL operation with comprehensive corner cases
class ALUSllTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Sll_Tester" should "test SLL operation with all corner cases" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      // Test 1: Basic shift by 1
      dut.io.operandA.poke(1.U)
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect(2.U)
      dut.clock.step(1)

      // Test 2: Shift by 5 (only low 5 bits used)
      dut.io.operandA.poke(1.U)
      dut.io.operandB.poke(5.U)
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect(32.U)
      dut.clock.step(1)

      // Test 3: Shift amount > 31 (should use only low 5 bits - RV32I spec)
      dut.io.operandA.poke(1.U)
      dut.io.operandB.poke(33.U)  // 33 = 1 mod 32 (binary: 100001)
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect(2.U)  // Shift by 1, not 33
      dut.clock.step(1)

      // Test 4: Shift by 32 (should be same as shift by 0)
      dut.io.operandA.poke("h12345678".U)
      dut.io.operandB.poke(32.U)  // 32 = 0 mod 32
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect("h12345678".U)  // No shift
      dut.clock.step(1)

      // Test 5: Shift by 63 (should be same as shift by 31)
      dut.io.operandA.poke(1.U)
      dut.io.operandB.poke(63.U)  // 63 = 31 mod 32
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect("h80000000".U)  // Shift left by 31
      dut.clock.step(1)

      // Test 6: Shift zero
      dut.io.operandA.poke("hdeadbeef".U)
      dut.io.operandB.poke(0.U)
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect("hdeadbeef".U)
      dut.clock.step(1)

      // Test 7: Shift with MSB set (test no sign extension)
      dut.io.operandA.poke("h80000000".U)  // MSB = 1
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect(0.U)  // MSB shifts out, LSB becomes 0
      dut.clock.step(1)

      // Test 8: Large shift (shift out all bits)
      dut.io.operandA.poke("hffffffff".U)
      dut.io.operandB.poke(31.U)
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect("h80000000".U)  // Only MSB remains
      dut.clock.step(1)

      // Test 9: Shift by 16 (mid-range)
      dut.io.operandA.poke("h0000abcd".U)
      dut.io.operandB.poke(16.U)
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect("habcd0000".U)
      dut.clock.step(1)

      // Test 10: Verify only low 5 bits of operandB are used
      // operandB = 0b100100 (binary) = 36 decimal = 4 mod 32
      dut.io.operandA.poke("h0000000f".U)
      dut.io.operandB.poke(36.U)  // 36 = 4 mod 32
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect("h000000f0".U)  // Shift by 4, not 36
      dut.clock.step(1)

      // Test 11: Pattern shift test
      dut.io.operandA.poke("h12345678".U)
      dut.io.operandB.poke(8.U)
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect("h34567800".U)
      dut.clock.step(1)

      // Test 12: Boundary test - shift by 31 (maximum effective shift)
      dut.io.operandA.poke(1.U)
      dut.io.operandB.poke(31.U)
      dut.io.operation.poke(ALUOp.SLL)
      dut.io.aluResult.expect("h80000000".U)
      dut.clock.step(1)
    }
  }
}

// Test SRL operation (logical right shift)
class ALUSrlTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Srl_Tester" should "test SRL operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      // Basic logical right shift
      dut.io.operandA.poke("h80000000".U)  // MSB set
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SRL)
      dut.io.aluResult.expect("h40000000".U)  // MSB becomes 0, shift right logical
      dut.clock.step(1)

      // Shift by 31 (max for RV32I)
      dut.io.operandA.poke("hffffffff".U)
      dut.io.operandB.poke(31.U)
      dut.io.operation.poke(ALUOp.SRL)
      dut.io.aluResult.expect(1.U)  // All bits shifted out except LSB
      dut.clock.step(1)

      // Shift by >31 (should use only low 5 bits)
      dut.io.operandA.poke("h80000000".U)
      dut.io.operandB.poke(33.U)  // 33 = 1 mod 32
      dut.io.operation.poke(ALUOp.SRL)
      dut.io.aluResult.expect("h40000000".U)  // Shift by 1
      dut.clock.step(1)

      // Shift zero
      dut.io.operandA.poke("h12345678".U)
      dut.io.operandB.poke(0.U)
      dut.io.operation.poke(ALUOp.SRL)
      dut.io.aluResult.expect("h12345678".U)  // No shift
      dut.clock.step(1)

      // Test pattern shift
      dut.io.operandA.poke("h87654321".U)
      dut.io.operandB.poke(4.U)
      dut.io.operation.poke(ALUOp.SRL)
      dut.io.aluResult.expect("h08765432".U)  // Logical shift, MSB filled with 0
      dut.clock.step(1)

    }
  }
}

// Test SRA operation (arithmetic right shift)
class ALUSraTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Sra_Tester" should "test SRA operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      // Positive number arithmetic shift (same as SRL)
      dut.io.operandA.poke("h40000000".U)  // Positive (MSB = 0)
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SRA)
      dut.io.aluResult.expect("h20000000".U)
      dut.clock.step(1)

      // Negative number arithmetic shift (preserves sign)
      dut.io.operandA.poke("h80000000".U)  // Negative (MSB = 1) = -2147483648
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SRA)
      dut.io.aluResult.expect("hc0000000".U)  // -1073741824 (sign extended)
      dut.clock.step(1)

      // All ones (negative one) shifted
      dut.io.operandA.poke("hffffffff".U)  // -1
      dut.io.operandB.poke(16.U)
      dut.io.operation.poke(ALUOp.SRA)
      dut.io.aluResult.expect("hffffffff".U)  // Still -1 (sign extended)
      dut.clock.step(1)

      // Shift negative number by 31
      dut.io.operandA.poke("h80000001".U)  // Negative number
      dut.io.operandB.poke(31.U)
      dut.io.operation.poke(ALUOp.SRA)
      dut.io.aluResult.expect("hffffffff".U)  // All ones (sign extended)
      dut.clock.step(1)

      // Compare SRL vs SRA for same negative number
      val negativeValue = "h80000000".U
      dut.io.operandA.poke(negativeValue)
      dut.io.operandB.poke(8.U)
      dut.io.operation.poke(ALUOp.SRA)
      dut.io.aluResult.expect("hff800000".U)  // SRA: sign extended
      dut.clock.step(1)
      
      dut.io.operandA.poke(negativeValue)
      dut.io.operandB.poke(8.U)
      dut.io.operation.poke(ALUOp.SRL)
      dut.io.aluResult.expect("h00800000".U)  // SRL: zero extended
      dut.clock.step(1)

    }
  }
}

// Test SLT operation with comprehensive corner cases
class ALUSltTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Slt_Tester" should "test SLT operation with all corner cases" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      // Test 1: Basic positive comparison (10 < 20)
      dut.io.operandA.poke(10.U)
      dut.io.operandB.poke(20.U)
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(1.U)
      dut.clock.step(1)

      // Test 2: Negative < Positive
      dut.io.operandA.poke("hffffffff".U)  // -1 as signed
      dut.io.operandB.poke(0.U)           // 0 as signed
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(1.U)  // -1 < 0
      dut.clock.step(1)

      // Test 3: Positive > Positive (should be 0)
      dut.io.operandA.poke(20.U)
      dut.io.operandB.poke(10.U)
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)

      // Test 4: Equal numbers
      dut.io.operandA.poke(42.U)
      dut.io.operandB.poke(42.U)
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(0.U)
      dut.clock.step(1)

      // Test 5: Negative < Negative (more negative)
      dut.io.operandA.poke("h80000000".U)  // -2147483648 (most negative)
      dut.io.operandB.poke("hffffffff".U)  // -1
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(1.U)  // -2147483648 < -1
      dut.clock.step(1)

      // Test 6: Negative > Negative (less negative)
      dut.io.operandA.poke("hffffffff".U)  // -1
      dut.io.operandB.poke("h80000000".U)  // -2147483648
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(0.U)  // -1 > -2147483648
      dut.clock.step(1)

      // Test 7: Compare with max positive signed
      dut.io.operandA.poke("h7fffffff".U)  // 2147483647 (max positive signed)
      dut.io.operandB.poke("h80000000".U)  // -2147483648 (min negative signed)
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(0.U)  // 2147483647 > -2147483648
      dut.clock.step(1)

      // Test 8: Compare negative with max positive
      dut.io.operandA.poke("h80000000".U)  // -2147483648
      dut.io.operandB.poke("h7fffffff".U)  // 2147483647
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(1.U)  // -2147483648 < 2147483647
      dut.clock.step(1)

      // Test 9: Zero compared with positive
      dut.io.operandA.poke(0.U)
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(1.U)  // 0 < 1
      dut.clock.step(1)

      // Test 10: Zero compared with negative
      dut.io.operandA.poke(0.U)
      dut.io.operandB.poke("hffffffff".U)  // -1
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(0.U)  // 0 > -1
      dut.clock.step(1)

      // Test 11: Small negative vs small positive
      dut.io.operandA.poke("hfffffffe".U)  // -2
      dut.io.operandB.poke(1.U)            // 1
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(1.U)  // -2 < 1
      dut.clock.step(1)

      // Test 12: Test overflow boundary case
      dut.io.operandA.poke("h7fffffff".U)  // Max positive
      dut.io.operandB.poke("h80000000".U)  // Min negative
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(0.U)  // Positive > Negative
      dut.clock.step(1)

      // Test 13: Compare near zero negative
      dut.io.operandA.poke("hffffffff".U)  // -1
      dut.io.operandB.poke(0.U)            // 0
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(1.U)  // -1 < 0
      dut.clock.step(1)

      // Test 14: Compare near zero positive
      dut.io.operandA.poke(0.U)            // 0
      dut.io.operandB.poke(1.U)            // 1
      dut.io.operation.poke(ALUOp.SLT)
      dut.io.aluResult.expect(1.U)  // 0 < 1
      dut.clock.step(1)
      

    }
  }
}