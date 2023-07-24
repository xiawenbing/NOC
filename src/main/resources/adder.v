module AdderBlackBox (
   input  wire [31:0] a, 
   input  wire [31:0] b,
   input  wire clk,
   input  wire reset,
   output reg [31:0] sum   
);
  
always @(posedge clk or posedge reset) begin
   if (reset)
      sum <= 32'b0;
   else
      sum <= a + b; 
end

endmodule