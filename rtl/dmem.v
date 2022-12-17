module dmem(	
	input clk,
	input wire [31:0]  ALUResultM, 
	input wire [31:0]  WriteDataM,
	input wire [1:0]   MemWriteM,
	output reg [31:0]  dmem
	
);
	localparam WORD_WRITE   = 2'b01;
	localparam NO_WRITE     = 2'b00;
	localparam HALF_WRITE   = 2'b10;
	localparam BYTE_WRITE   = 2'b11;
	reg [31:0] dmem[63:0];
	always@(*) ReadDataMTick = dmem[ALUResultM[31:2]]; //read
	always@(posedge clk) begin//write
		case(MemWriteM)
			NO_WRITE: begin end
			WORD_WRITE: dmem[ALUResultM[31:2]]<= WriteDataM[31:0];
			HALF_WRITE: case(ALUResultM[1])
						1'b0:dmem[ALUResultM[31:2]][15:0]<= WriteDataM[15:0];
						1'b1:dmem[ALUResultM[31:2]][31:16]<= WriteDataM[15:0];
						default: dmem[ALUResultM[31:2]]<= 32'bxxxx_xxxx_xxxx_xxxx_xxxx_xxxx_xxxx_xxxx;
				   endcase
			BYTE_WRITE: case(ALUResultM[1:0])
						2'b00:dmem[ALUResultM[31:2]][7:0] <= WriteDataM[7:0];
						2'b01:dmem[ALUResultM[31:2]][15:8] <= WriteDataM[7:0];
						2'b10:dmem[ALUResultM[31:2]][23:16] <= WriteDataM[7:0];
						2'b11:dmem[ALUResultM[31:2]][31:24] <= WriteDataM[7:0];
						default: dmem[ALUResultM[31:2]]<= 32'bxxxx_xxxx_xxxx_xxxx_xxxx_xxxx_xxxx_xxxx;
				   endcase 
				   
			default: dmem[ALUResultM[31:2]]    <= 32'bxxxx_xxxx_xxxx_xxxx_xxxx_xxxx_xxxx_xxxx;
		endcase
	 end
endmodule