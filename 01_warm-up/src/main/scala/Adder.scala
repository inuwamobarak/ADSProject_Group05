// ADS I Class Project
// Chisel Introduction
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 18/10/2022 by Tobias Jauch (@tojauch)

package adder

import chisel3._
import chisel3.util._


/** 
  * Half Adder Class 
  * 
  * Your task is to implement a basic half adder as presented in the lecture.
  * Each signal should only be one bit wide (inputs and outputs).
  * There should be no delay between input and output signals, we want to have
  * a combinational behaviour of the component.
  */
class HalfAdder extends Module{
  
  val io = IO(new Bundle {
    // Inputs: two 1-bit numbers to add
    val a  = Input(Bool())
    val b  = Input(Bool())
    // Outputs: sum and carry
    val s  = Output(Bool())  // sum
    val co = Output(Bool())  // carry out
    })

  // Half adder logic: s = a XOR b, co = a AND b
  io.s  := io.a ^ io.b
  io.co := io.a & io.b

}

/** 
  * Full Adder Class 
  * 
  * Your task is to implement a basic full adder. The component's behaviour should 
  * match the characteristics presented in the lecture. In addition, you are only allowed 
  * to use two half adders (use the class that you already implemented) and basic logic 
  * operators (AND, OR, ...).
  * Each signal should only be one bit wide (inputs and outputs).
  * There should be no delay between input and output signals, we want to have
  * a combinational behaviour of the component.
  */
class FullAdder extends Module{

  val io = IO(new Bundle {
    // Inputs: two 1-bit numbers and carry in
    val a  = Input(Bool())
    val b  = Input(Bool())
    val ci = Input(Bool())  // carry in
    // Outputs: sum and carry out
    val s  = Output(Bool())  // sum
    val co = Output(Bool())  // carry out
    })


  // Instantiate two half adders
  val ha1 = Module(new HalfAdder)
  val ha2 = Module(new HalfAdder)

  // Connect first half adder
  ha1.io.a := io.a
  ha1.io.b := io.b
  
  // Connect second half adder
  ha2.io.a := ha1.io.s        // sum from first HA
  ha2.io.b := io.ci           // carry in

  // Final outputs
  io.s  := ha2.io.s           // sum from second HA
  io.co := ha1.io.co | ha2.io.co  // carry out if either HA has carry

}

/** 
  * 4-bit Adder class 
  * 
  * Your task is to implement a 4-bit ripple-carry-adder. The component's behaviour should 
  * match the characteristics presented in the lecture.  Remember: An n-bit adder can be 
  * build using one half adder and n-1 full adders.
  * The inputs and the result should all be 4-bit wide, the carry-out only needs one bit.
  * There should be no delay between input and output signals, we want to have
  * a combinational behaviour of the component.
  */
class FourBitAdder extends Module{

  val io = IO(new Bundle {
    // Inputs: two 4-bit numbers
    val a  = Input(UInt(4.W))
    val b  = Input(UInt(4.W))
    // Outputs: 4-bit sum and 1-bit carry out
    val s  = Output(UInt(4.W))  // sum
    val co = Output(Bool())      // carry out
    })

  // Instantiate components
  val ha = Module(new HalfAdder)           // for bit 0
  val fa0 = Module(new FullAdder)          // for bit 1
  val fa1 = Module(new FullAdder)          // for bit 2
  val fa2 = Module(new FullAdder)          // for bit 3

  // Connect the half adder (bit 0 - LSB)
  ha.io.a := io.a(0)
  ha.io.b := io.b(0)
  // io.s(0) := ha.io.s
  
  // Connect first full adder (bit 1)
  fa0.io.a := io.a(1)
  fa0.io.b := io.b(1)
  fa0.io.ci := ha.io.co  // carry from half adder
  // io.s(1) := fa0.io.s
  
  // Connect second full adder (bit 2)
  fa1.io.a := io.a(2)
  fa1.io.b := io.b(2)
  fa1.io.ci := fa0.io.co  // carry from previous full adder
  // io.s(2) := fa1.io.s
  
  // Connect third full adder (bit 3 - MSB)
  fa2.io.a := io.a(3)
  fa2.io.b := io.b(3)
  fa2.io.ci := fa1.io.co  // carry from previous full adder
  // io.s(3) := fa2.io.s
  
  // Concatenate outputs (MSB first) into 4-bit sum
  io.s := Cat(fa2.io.s, fa1.io.s, fa0.io.s, ha.io.s)
  
  // Final carry out
  io.co := fa2.io.co
}
