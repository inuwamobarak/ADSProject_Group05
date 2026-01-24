// ADS I Class Project
// Assignment 02: Arithmetic Logic Unit and UVM Testbench
//
// Chair of Electronic Design Automation, RPTU University Kaiserslautern-Landau
// File created on 09/21/2025 by Tharindu Samarakoon (gug75kex@rptu.de)
// File updated on 10/31/2025 by Tobias Jauch (tobias.jauch@rptu.de)

`include "uvm_macros.svh"
import uvm_pkg::*;
import alu_tb_config_pkg::*;

class alu_seq_item extends uvm_sequence_item;

    // Fields of the sequence item
    rand bit [31:0] operandA;      // First ALU operand
    rand bit [31:0] operandB;      // Second ALU operand
    rand ALUOp      operation;     // ALU operation from enum
    bit [31:0]      aluResult;     // Expected result from ALU

    //The class with the factory
    `uvm_object_utils_begin(alu_seq_item)
        `uvm_field_int(operandA, UVM_DEFAULT)
        `uvm_field_int(operandB, UVM_DEFAULT)
        `uvm_field_enum(ALUOp, operation, UVM_DEFAULT)
        `uvm_field_int(aluResult, UVM_DEFAULT)
    `uvm_object_utils_end

    //Constraint for operation field
    constraint aluOp_constraint {
        operation inside {ADD, SUB, AND, OR, XOR, SLL, SRL, SRA, SLT, SLTU, PASSB};
    }

    virtual function string convert2str();
        return $sformatf("operandA: 0x%0x, operandB: 0x%0x, operation: %0p, aluResult: 0x%0x", operandA, operandB, operation, aluResult);
    endfunction

    function new(string name = "alu_seq_item"); 
        super.new(name);
    endfunction   

endclass
