module imem(
	input wire PCF,
	output reg [31:0] InstrF
);
/**********************************/
	/* instruction memory interface   */
	/**********************************/
	reg [31:0] imem [63:0];
	
	initial $readmemh("C:/Users/david/Desktop/RV32I CPU - Verilog/riscvtest.txt",imem);
	
	assign InstrF = imem[PCF[31:2]];	
endmodule