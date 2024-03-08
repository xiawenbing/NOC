/*
 * File: SM3_top.v
 * Project: rtl
 * File Created: Sunday, 5th August 2018 9:03:57 am
 * Author: Chen Rui (raymond.rui.chen@qq.com>)
 * -----
 * Last Modified: Sunday, 5th August 2018 9:12:50 am
 * Modified By: Chen Rui (raymond.rui.chen@qq.com>)
 * -----
 * Copyright (c) 2018 - Chen Rui
 * All rights reserved.
 */
module SM3_top
	(
		clk_in,
		reset_n_in,
		SM3_en_in,
		msg_in,
		msg_valid_in,
		is_last_word_in,
		last_word_byte_in,
		sm3_result_out,
		sm3_finished_out
	);
	
`define		IDLE		2'b00
`define		PADDING		2'b01
`define		ITERATION	2'b10	

localparam	WIDTH = 32;	

input					clk_in,
						reset_n_in,
						SM3_en_in;
input	[WIDTH - 1 : 0]	msg_in;	
input					msg_valid_in;
input					is_last_word_in;	
input	[1:0]			last_word_byte_in;	
output	[255 : 0]		sm3_result_out;		
output					sm3_finished_out;	

reg						padding_en,
						iteration_en;
reg						sm3_finished_out;
reg		[1:0]			current_state,
						next_state;		
reg		[5:0]			index_j;
wire					padding_1_block_finished;
wire	[511:0]			msg_padded;
wire					padding_all_finished;
wire	[31:0]			word_expanded_p;
wire	[31:0]			word_expanded;
wire					msg_exp_finished;
reg						is_last_block;
wire					is_1st_msg_block;

always@(posedge clk_in)
	if(!reset_n_in)
		is_last_block 	<=	1'b0;
	else if(padding_all_finished)	
		is_last_block	<=	1'b1;
	else if(SM3_en_in == 1'b0)
		is_last_block	<=	1'b0;
	else
		is_last_block	<=	is_last_block;

always@(posedge clk_in)					
	if(!reset_n_in)
		current_state	<=	`IDLE;
	else
		current_state	<=	next_state;
		
always@(*)
	begin	
		next_state	=	`IDLE;
		case(current_state)
			`IDLE:
				if(SM3_en_in == 1'b1 && is_last_block != 1'b1)	
					next_state = `PADDING;
				else
					next_state = `IDLE;
			`PADDING:
				if(padding_1_block_finished)
					next_state = `ITERATION;
				else
					next_state = `PADDING;
			`ITERATION:
				if(msg_exp_finished)
					next_state = `IDLE;
				else
					next_state = `ITERATION;
			default:
					next_state = `IDLE;
		endcase
	end

always@(posedge clk_in)
	if(!reset_n_in)
		begin
			padding_en			<=	1'b0;
			iteration_en		<=	1'b0;
			sm3_finished_out	<=	1'b0;
		end
	else
		begin

			if(next_state == `PADDING)
				padding_en		<=	1'b1;
			else
				padding_en		<=	1'b0;
			
			if(current_state == `ITERATION)
				iteration_en	<=	1'b1;
			else
				iteration_en	<=	1'b0;
			
			if(current_state == `ITERATION && next_state == `IDLE && is_last_block ==  1'b1)
				sm3_finished_out	<=	1'b1;
			else
				sm3_finished_out	<=	1'b0;								
		end

always@(posedge clk_in)
	if(!reset_n_in)
		index_j	<=	'd0;
	else if(iteration_en)
		index_j	<=	index_j	+	1'b1;
	else
		index_j	<=	'd0;

msg_padding #(WIDTH) U_pad
	(
		.clk_in(						clk_in),
		.reset_n_in(					reset_n_in),
		.SM3_en_in(						SM3_en_in),
		.padding_en_in(					padding_en),
		.msg_in(						msg_in),
		.msg_valid_in(					msg_valid_in),
		.is_last_word_in(				is_last_word_in),
		.last_word_byte_in(				last_word_byte_in),
		.is_1st_msg_block_out(			is_1st_msg_block),
		.msg_padded_out(				msg_padded),
		.padding_1_block_finished_out(	padding_1_block_finished),
		.padding_all_finished_out(		padding_all_finished)
	);
	
msg_expansion U_exp
	(
		.clk_in(						clk_in),
		.reset_n_in(					reset_n_in),
		.start_in(						padding_1_block_finished),
		.index_j_in(					index_j),
		.message_in(					msg_padded),
		.word_p_out(					word_expanded_p),
		.word_out(						word_expanded),
		.msg_exp_finished_out(			msg_exp_finished)
	);	

compression_function U_cf
	(
		.clk_in(						clk_in),
		.reset_n_in(					reset_n_in),
		.start_in(						padding_1_block_finished),
		.index_j_in(					index_j),
		.is_1st_msg_block_in(			is_1st_msg_block),
		.word_expanded_p_in(			word_expanded_p),
		.word_expanded_in(				word_expanded),		
		.data_after_cf_out(				sm3_result_out)
	);	
		
endmodule		

/*
 * File: msg_padding.v
 * Project: rtl
 * File Created: Sunday, 5th August 2018 9:03:57 am
 * Author: Chen Rui (raymond.rui.chen@qq.com>)
 * -----
 * Last Modified: Sunday, 5th August 2018 9:12:47 am
 * Modified By: Chen Rui (raymond.rui.chen@qq.com>)
 * -----
 * Copyright (c) 2018 - Chen Rui
 * All rights reserved.
 */
module msg_padding
#(
	parameter	WIDTH = 32
)
	(
		clk_in,
		reset_n_in,
		SM3_en_in,
		padding_en_in,
		msg_in,
		msg_valid_in,
		is_last_word_in,
		last_word_byte_in,
		is_1st_msg_block_out,
		msg_padded_out,
		padding_1_block_finished_out,
		padding_all_finished_out
	);
	
`define		IDLE					5'd0
`define		DIRECT_PASS				5'd1
`define		CASE1					5'd2
`define		CASE2_2B00				5'd3
`define		CASE2_2B01				5'd4
`define		CASE2_2B10				5'd5
`define		CASE2_2B11				5'd6
`define		CASE2_2B11_S			5'd7
`define		CASE3_2B00				5'd8
`define		CASE3_2B01				5'd9
`define		CASE3_2B10				5'd10
`define		CASE3_2B11				5'd11
`define		CASE3_2B11_S			5'd12
`define		SPECIAL_ALL_ZERO		5'd13
`define		CASE4_2B11_S			5'd14
`define		SPECIAL_80_ZERO			5'd15
`define		FINISH_GENERAL			5'd16
`define		FINISH_SPECIAL_ALL_ZERO	5'd17	
`define		FINISH_80_ZERO			5'd18

input					clk_in,
						reset_n_in,
						SM3_en_in,
						padding_en_in;		
input	[WIDTH - 1 : 0]	msg_in;				
input					msg_valid_in;		
input					is_last_word_in;	
input	[1:0]			last_word_byte_in;	//2'b00, |X|0|0|0|; 
											//2'b01, |X|X|0|0|;
											//2'b10, |X|X|X|0|;
											//2'b11, |X|X|X|X|
output	[511:0]			msg_padded_out;		
output					padding_1_block_finished_out;
output					padding_all_finished_out;
output					is_1st_msg_block_out;

reg		[4:0]			current_state,
						next_state;				
reg		[511:0]			msg_padded_out;
reg		[31:0]			tmp_0,
						tmp_1,
						tmp_2,
						tmp_3,
						tmp_4,
						tmp_5,
						tmp_6,
						tmp_7,
						tmp_8,
						tmp_9,
						tmp_a,
						tmp_b,
						tmp_c,
						tmp_d,
						tmp_e,
						tmp_f;
reg		[3:0]			count;
reg						count_enable;
reg		[8:0]			bit_count;
reg		[54:0]			block_count;
reg		[31:0]			next_input_data;
reg						padding_1_block_finished_out;
reg						padding_all_finished_out;
wire	[511:0]			data_concatenation;
reg		[63:0]			msg_length;
reg						special_zero;		
reg						special_80;		

assign	is_1st_msg_block_out = block_count == 'd0; 
	
assign	data_concatenation	=	{
									tmp_0,
									tmp_1,
									tmp_2,
									tmp_3,
									tmp_4,
									tmp_5,
									tmp_6,
									tmp_7,
									tmp_8,
									tmp_9,
									tmp_a,
									tmp_b,
									tmp_c,
									tmp_d,
									tmp_e,
									tmp_f
								};
								
always@(*)
	if(current_state == `FINISH_GENERAL ||
		special_zero ||
		special_80)
		msg_length = {block_count,9'd0} +  {55'b0,bit_count};
	else
		msg_length = 'd0;

reg						reg_msg_valid;
wire					valid_data_start;
reg						reg_is_last_word;
reg		[1:0]			reg_last_word_byte;
						
always@(posedge clk_in)
	if(!reset_n_in)
		reg_msg_valid	<=	'd0;
	else 
		reg_msg_valid	<=	msg_valid_in;
		
		
always@(posedge clk_in)
	if(!reset_n_in)
		reg_is_last_word	<=	'd0;
	else 
		reg_is_last_word	<=	is_last_word_in;
		

always@(posedge clk_in)
	if(!reset_n_in)
		reg_last_word_byte	<=	'd0;
	else 
		reg_last_word_byte	<=	last_word_byte_in;
		
		
assign	valid_data_start = reg_msg_valid == 1'b0 && msg_valid_in == 1'b1;
	
always@(posedge clk_in)
if(!reset_n_in)
	begin
		tmp_0	<=	32'd0;
		tmp_1	<=	32'd0;
		tmp_2	<=	32'd0;
		tmp_3	<=	32'd0;
		tmp_4	<=	32'd0;
		tmp_5	<=	32'd0;
		tmp_6	<=	32'd0;
		tmp_7	<=	32'd0;
		tmp_8	<=	32'd0;
		tmp_9	<=	32'd0;
		tmp_a	<=	32'd0;
		tmp_b	<=	32'd0;
		tmp_c	<=	32'd0;
		tmp_d	<=	32'd0;
        tmp_e	<=	32'd0;
        tmp_f	<=	32'd0;
    end
else if(count_enable)
	begin  
		case(count)
			4'h0:	tmp_0	<=	next_input_data;
			4'h1:	tmp_1	<=	next_input_data;
			4'h2:	tmp_2	<=	next_input_data;
			4'h3:	tmp_3	<=	next_input_data;
			4'h4:	tmp_4	<=	next_input_data;
			4'h5:	tmp_5	<=	next_input_data;
			4'h6:	tmp_6	<=	next_input_data;
			4'h7:	tmp_7	<=	next_input_data;
			4'h8:	tmp_8	<=	next_input_data;
			4'h9:	tmp_9	<=	next_input_data;
			4'ha:	tmp_a	<=	next_input_data;
			4'hb:	tmp_b	<=	next_input_data;
			4'hc:	tmp_c	<=	next_input_data;
			4'hd:	tmp_d	<=	next_input_data;
			4'he:	tmp_e	<=	next_input_data;
			4'hf:	tmp_f	<=	next_input_data;
		endcase
	end
else
	begin
		tmp_0	<=	tmp_0;
	    tmp_1	<=  tmp_1;
	    tmp_2	<=  tmp_2;
	    tmp_3	<=  tmp_3;
	    tmp_4	<=  tmp_4;
	    tmp_5	<=  tmp_5;
	    tmp_6	<=  tmp_6;
	    tmp_7	<=  tmp_7;
	    tmp_8	<=  tmp_8;
	    tmp_9	<=  tmp_9;
	    tmp_a	<=  tmp_a;
	    tmp_b	<=  tmp_b;
	    tmp_c	<=  tmp_c;
	    tmp_d	<=  tmp_d;
	    tmp_e	<=  tmp_e;
	    tmp_f	<=  tmp_f;
	end

always@(*)
if(padding_1_block_finished_out)
	begin
		if(current_state == `FINISH_GENERAL)
			msg_padded_out	=	{data_concatenation[511:64], msg_length};
		else if(special_zero)
			msg_padded_out	=	{448'd0, msg_length};		
		else if(special_80)
			msg_padded_out	=	{32'h8000_0000, 416'd0, msg_length};
		else
			msg_padded_out	=	data_concatenation;
	end
else
		msg_padded_out	=	'd0;
		
always@(posedge clk_in)
	if(!reset_n_in)
		count	<=	'd0;
	else if(count_enable)
		count	<=	count + 1'b1;			
	else
		count	<=	'd0;
		
reg		is_last_block;		
always@(posedge clk_in)
	if(!reset_n_in)
		is_last_block 	<=	1'b0;
	else if(reg_is_last_word)	
		is_last_block	<=	1'b1;
	else if(count == 'd15)
		is_last_block	<=	1'b0;
	else
		is_last_block	<=	is_last_block;
		
always@(posedge clk_in)
	if(!reset_n_in)
		block_count	<=	'd0;
	else if(count == 'd15 && !is_last_block)
		block_count	<=	block_count + 1'b1;
	else
		block_count	<=	block_count;
	
always@(posedge clk_in)
	if(!reset_n_in)
		bit_count	<=	'd0;
	else if(reg_is_last_word)
		bit_count	<=	{count, 5'd0} +  {4'd0,reg_last_word_byte,3'd0}+9'b0_0000_1000;
	else
		bit_count	<=	bit_count;
							

always@(posedge clk_in)					
	if(!reset_n_in)
		current_state	<=	`IDLE;
	else
		current_state	<=	next_state;
		
always@(*)
	begin	
		next_state	=	`IDLE;
		case(current_state)
			`IDLE:
					if(padding_en_in)
						next_state	=	`DIRECT_PASS;
					else
						next_state	=	`IDLE;
			`DIRECT_PASS:
					if(!reg_is_last_word && count == 'd15)	
						next_state	=	`CASE1;
					else if(reg_is_last_word && (count <'d13 || (count == 'd13 && reg_last_word_byte != 2'b11)))
						begin
							case(last_word_byte_in)
								2'b00:	next_state = `CASE2_2B00;
								2'b01:	next_state = `CASE2_2B01;
								2'b10:	next_state = `CASE2_2B10;
								2'b11:	next_state = `CASE2_2B11;
							endcase
						end
					else if(reg_is_last_word && ((count == 'd13 && reg_last_word_byte == 2'b11) ||
												 count == 'd14 ||
												 (count == 'd15 && reg_last_word_byte == 2'b00) ||
												 (count == 'd15 && reg_last_word_byte == 2'b01) ||
												 (count == 'd15 && reg_last_word_byte == 2'b10))
							)
						begin
							case(reg_last_word_byte)
								2'b00:	next_state = `CASE3_2B00;
								2'b01:	next_state = `CASE3_2B01;
								2'b10:	next_state = `CASE3_2B10;
								2'b11:	next_state = `CASE3_2B11;
							endcase
						end								
					else if(reg_is_last_word && count == 'd15 && reg_last_word_byte == 2'b11)
						next_state	=	`CASE4_2B11_S;
					else
						next_state = `DIRECT_PASS;
			`CASE1:
				next_state = `IDLE;
			
			`CASE2_2B00:
				if(count == 4'd15)
					next_state = `FINISH_GENERAL;
				else
					next_state = `CASE2_2B00;
					
			`CASE2_2B01:
				if(count == 4'd15)
					next_state = `FINISH_GENERAL;
				else
					next_state = `CASE2_2B01;
			`CASE2_2B10:
				if(count == 4'd15)
					next_state = `FINISH_GENERAL;
				else
					next_state = `CASE2_2B10;					
			`CASE2_2B11:
				next_state	=	`CASE2_2B11_S;
			
			`CASE2_2B11_S:
				if(count == 'd15)
					next_state = `FINISH_GENERAL;
				else
					next_state = `CASE2_2B11_S;					
			
			`CASE3_2B00:
				if(count == 'd15)
					next_state = `SPECIAL_ALL_ZERO;
				else
					next_state = `CASE3_2B00;					
			`CASE3_2B01:
				if(count == 'd15)
					next_state = `SPECIAL_ALL_ZERO;
				else
					next_state = `CASE3_2B01;
			`CASE3_2B10:
				if(count == 'd15)
					next_state = `SPECIAL_ALL_ZERO;
				else
					next_state = `CASE3_2B10;
					
			`CASE3_2B11:
					next_state = `CASE3_2B11_S;
					
			`CASE3_2B11_S:
				if(count == 'd15)
					next_state = `SPECIAL_ALL_ZERO;
				else
					next_state = `CASE3_2B11_S;	
					
			`CASE4_2B11_S:
					next_state = `SPECIAL_80_ZERO;
			
			`FINISH_GENERAL:
					next_state = `IDLE;
			
					
			`SPECIAL_ALL_ZERO:
					if(padding_en_in)
						next_state	=	`IDLE;
					else
						next_state	=	`SPECIAL_ALL_ZERO;
						
			`SPECIAL_80_ZERO:
					if(padding_en_in)
						next_state	=	`IDLE;
					else
						next_state	=	`SPECIAL_80_ZERO;
			default:
					next_state	=	`IDLE;
		endcase
	end

always@(posedge clk_in)	
	if(count == 'd15)
		padding_1_block_finished_out	<=	1'b1;
	else if(
				(current_state == `SPECIAL_ALL_ZERO && padding_en_in) ||
				(current_state == `SPECIAL_80_ZERO && padding_en_in) 
			)
		padding_1_block_finished_out	<=	1'b1;
	else
		padding_1_block_finished_out	<=	1'b0;	

	
	
always@(posedge clk_in)
	if(!reset_n_in)
		begin
			count_enable					<=	1'b0;
			next_input_data					<=	'd0;
			padding_all_finished_out		<=	1'b0;
			special_zero					<=	1'b0;
			special_80						<=	1'b0;
		end
	else
		begin			
			if(current_state == `SPECIAL_ALL_ZERO)	
				special_zero	<=	1'b1;
			else
				special_zero	<=	1'b0;
				
			if(current_state == `SPECIAL_80_ZERO)	
				special_80	<=	1'b1;
			else
				special_80	<=	1'b0;			
				
			if(current_state == `DIRECT_PASS  && msg_valid_in == 1'b1)
				count_enable	<=	1'b1;
			else if(count == 'd15
					)
				count_enable	<=	1'b0;
			else
				count_enable	<=	count_enable;

				
			case(current_state)
				`DIRECT_PASS:
								if(msg_valid_in && is_last_word_in && last_word_byte_in ==  2'b00)
									next_input_data	<=	{msg_in[31:24],8'h80, 16'h0};
								else if(msg_valid_in && is_last_word_in && last_word_byte_in ==  2'b01)
									next_input_data	<=	{msg_in[31:16],8'h80, 8'h0};
								else if(msg_valid_in && is_last_word_in && last_word_byte_in ==  2'b10)
									next_input_data	<=	{msg_in[31:8],8'h80};
								else
									next_input_data	<=	msg_in;
				`CASE2_2B11_S:	next_input_data	<=	32'h8000_0000;
				`CASE3_2B11_S:  next_input_data	<=	32'h8000_0000;
				default:		next_input_data	<=	msg_valid_in == 1'b1 ? msg_in : 'd0;
			endcase	
						
			if(
				(current_state == `CASE2_2B00 && next_state == `FINISH_GENERAL) ||
				(current_state == `CASE2_2B01 && next_state == `FINISH_GENERAL) ||
				(current_state == `CASE2_2B10 && next_state == `FINISH_GENERAL) ||
				(current_state == `CASE2_2B11_S && next_state == `FINISH_GENERAL) ||
				(current_state == `SPECIAL_ALL_ZERO && next_state == `IDLE) ||
				(current_state == `SPECIAL_80_ZERO && next_state == `IDLE)
			)
				padding_all_finished_out	<=	1'b1;
			else if(SM3_en_in == 1'b0)
				padding_all_finished_out	<=	1'b0;
			else
				padding_all_finished_out	<=	padding_all_finished_out;			
						
		end
		

		
endmodule

/*
 * File: message_expansion.v
 * Project: rtl
 * File Created: Sunday, 5th August 2018 9:03:57 am
 * Author: Chen Rui (raymond.rui.chen@qq.com>)
 * -----
 * Last Modified: Sunday, 5th August 2018 9:12:44 am
 * Modified By: Chen Rui (raymond.rui.chen@qq.com>)
 * -----
 * Copyright (c) 2018 - Chen Rui
 * All rights reserved.
 */
module msg_expansion
	(
		clk_in,
		reset_n_in,
		message_in,
		start_in,
		index_j_in,
		word_p_out,
		word_out,
		msg_exp_finished_out
	);
	
`define	IDLE	2'b00
`define	WORKING	2'b01

input				clk_in;
input				reset_n_in;	
input	[511 : 0]	message_in;			
input				start_in;		
input	[5:0]		index_j_in;		

output	reg[31 : 0]	word_p_out;			
output	reg[31 : 0]	word_out;			
output				msg_exp_finished_out;

reg					working_en;
reg					msg_exp_finished_out;
reg		[1:0]		current_state,
					next_state;
reg		[31 : 0]	w0;
reg		[31 : 0]	w1;
reg		[31 : 0]	w2;
reg		[31 : 0]	w3;
reg		[31 : 0]	w4;
reg		[31 : 0]	w5;
reg		[31 : 0]	w6;
reg		[31 : 0]	w7;
reg		[31 : 0]	w8;
reg		[31 : 0]	w9;
reg		[31 : 0]	w10;
reg		[31 : 0]	w11;
reg		[31 : 0]	w12;
reg		[31 : 0]	w13;
reg		[31 : 0]	w14;
reg		[31 : 0]	w15;

wire	[31 : 0]	tmp_shift_15;	
wire	[31 : 0]	tmp_shift_7;	
wire	[31 : 0]	data_for_p1;
wire	[31 : 0]	data_after_p1;
wire	[31 : 0]	word_update;

assign	tmp_shift_15	=	{w13[31-15:0], w13[31:31-15+1]};
assign	tmp_shift_7		=	{w3[31-7:0],   w3[31:31-7+1]};
assign	data_for_p1		=	w0 ^ w7 ^ tmp_shift_15;
assign	data_after_p1	=	data_for_p1 ^ {data_for_p1[31-15:0], data_for_p1[31:31-15+1]} ^ {data_for_p1[31-23:0], data_for_p1[31:31-23+1]};
assign	word_update		=	data_after_p1 ^ tmp_shift_7 ^ w10;


always@(posedge clk_in)
	if(!reset_n_in)
		{w0, w1,  w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15}	<=	512'd0;
	else if(start_in)
		{w0, w1,  w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15}	<=	message_in;
	else if(working_en)
		{w0, w1,  w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15}	<=	{w1,  w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, word_update};
	else
		{w0, w1,  w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15}	<=	{w0, w1,  w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15};	


always@(posedge clk_in)
	if(!reset_n_in)
		begin
			word_p_out	<=	32'd0;
			word_out	<=	32'd0;
		end
	else if(working_en)
		begin
			word_p_out	<=	w0 ^ w4;
			word_out	<=	w0;
		end
	else
		begin
			word_p_out	<=	word_p_out;
			word_out	<=	word_out;
		end
	
	
	
always@(posedge clk_in)					
	if(!reset_n_in)
		current_state	<=	`IDLE;
	else
		current_state	<=	next_state;
		
always@(*)
	begin	
		next_state	=	`IDLE;
		case(current_state)
			`IDLE:
					if(start_in)
						next_state = `WORKING;
					else
						next_state = `IDLE;

			`WORKING:
					if(index_j_in == 'd63)
						next_state = `IDLE;
					else
						next_state = `WORKING;
			default:
					next_state = `IDLE;
		endcase
	end
	
always@(posedge clk_in)
	if(!reset_n_in)
		begin
			working_en				<=	1'b0;
			msg_exp_finished_out	<=	1'b0;
		end
	else 
		begin
			
			if(next_state == `WORKING)
				working_en <= 1'b1;
			else
				working_en	<=	1'b0;	
			
			if(current_state == `WORKING && next_state == `IDLE)
				msg_exp_finished_out	<=	'd1;
			else
				msg_exp_finished_out	<=	'd0;
		end
		
		
endmodule
	
/*
 * File: compression_function.v
 * Project: rtl
 * File Created: Sunday, 5th August 2018 9:03:57 am
 * Author: Chen Rui (raymond.rui.chen@qq.com>)
 * -----
 * Last Modified: Sunday, 5th August 2018 9:12:25 am
 * Modified By: Chen Rui (raymond.rui.chen@qq.com>)
 * -----
 * Copyright (c) 2018 - Chen Rui
 * All rights reserved.
 */
module compression_function
	(
		clk_in,
		reset_n_in,
		start_in,
		index_j_in,
		is_1st_msg_block_in,
		word_expanded_p_in,
		word_expanded_in,		
		data_after_cf_out
	);
	
`define	IDLE	2'b00
`define	LOAD	2'b01
`define	WORKING	2'b10	
	
input					clk_in;
input					reset_n_in;
input					start_in;
input	[5  : 0]		index_j_in;
input	[31 : 0]		word_expanded_p_in;
input	[31 : 0]		word_expanded_in;
input					is_1st_msg_block_in;

output	reg[255: 0]		data_after_cf_out;
reg						msg_cf_finished;

reg		[31 : 0]	reg_a;
reg		[31 : 0]	reg_b;
reg		[31 : 0]	reg_c;
reg		[31 : 0]	reg_d;
reg		[31 : 0]	reg_e;
reg		[31 : 0]	reg_f;
reg		[31 : 0]	reg_g;
reg		[31 : 0]	reg_h;

wire	[31 : 0]	tmp_for_ss1_0;
wire	[31 : 0]	TJ;
wire	[5  : 0]	J_mod;
wire	[31 : 0]	tmp_for_ss1_1;
wire	[31 : 0]	tmp_for_ss1_2;
wire	[31 : 0]	ss1;
wire	[31 : 0]	ss2;
wire	[31 : 0]	tmp_for_tt1_0;
wire	[31 : 0]	tmp_for_tt1_1;
wire	[31 : 0]	tt1;
wire	[31 : 0]	tmp_for_tt2_0;
wire	[31 : 0]	tmp_for_tt2_1;
wire	[31 : 0]	tt2;
wire	[31 : 0]	tt2_after_p0;


wire	[255:0]		data;

reg		[1:0]		current_state,
					next_state;
reg					working_en;	
reg					load_en;		
wire	[31:0]		tmp_a,
					tmp_b,
					tmp_c,
					tmp_d,
					tmp_e,
					tmp_f,
					tmp_g,
					tmp_h;
reg					reg_is_1st_msg_block2;
reg					reg_is_1st_msg_block;


always@(posedge clk_in)					
	if(!reset_n_in)
		reg_is_1st_msg_block2	<= 1'b0;
	else
		reg_is_1st_msg_block2	<=	is_1st_msg_block_in;
		
		
always@(posedge clk_in)					
	if(!reset_n_in)
		reg_is_1st_msg_block	<= 1'b0;
	else
		reg_is_1st_msg_block	<=	reg_is_1st_msg_block2;
				

assign	tmp_for_ss1_0	=	{reg_a[31-12:0], reg_a[31:31-12+1]} + reg_e;
assign	TJ				=	index_j_in < 16 ? 32'h79cc4519  : 32'h7a879d8a;
assign	J_mod			=	index_j_in < 6'd32 ? index_j_in	:  index_j_in - 6'd32;
assign	tmp_for_ss1_2	=	tmp_for_ss1_0 + tmp_for_ss1_1;
assign	ss1				=	{tmp_for_ss1_2[31 - 7 : 0], tmp_for_ss1_2[31 : 31 - 7 + 1]};
assign	ss2				=	ss1 ^ {reg_a[31 - 12 : 0], reg_a[31 : 31 - 12 + 1]};
assign	tmp_for_tt1_0	=	index_j_in < 16 ? reg_a ^ reg_b ^ reg_c : (reg_a & reg_b | reg_a & reg_c | reg_b & reg_c);
assign	tmp_for_tt1_1	=	reg_d + ss2 + word_expanded_p_in;
assign	tt1				=	tmp_for_tt1_0 + tmp_for_tt1_1;
assign	tmp_for_tt2_0	=	index_j_in < 16 ? reg_e ^ reg_f ^ reg_g : (reg_e & reg_f | ~reg_e & reg_g);
assign	tmp_for_tt2_1	=	reg_h + ss1 + word_expanded_in;
assign	tt2				=	tmp_for_tt2_0 + tmp_for_tt2_1;
assign	tt2_after_p0	=	tt2 ^ {tt2[31-9:0], tt2[31:31-9+1]} ^ {tt2[31-17:0], tt2[31:31-17+1]};


assign	is_1st_data_block	=	load_en == 1'b1 && index_j_in == 'd0;
assign	data	=	is_1st_data_block 
					? data_after_cf_out 
					: {reg_a,reg_b,reg_c,reg_d,reg_e,reg_f,reg_g,reg_h};
					
assign	{tmp_a, tmp_b, tmp_c, tmp_d, tmp_e, tmp_f, tmp_g, tmp_h} = data;


always@(posedge clk_in)
	if(!reset_n_in)
		begin
			reg_a	<=	'd0;
			reg_b	<=	'd0;
			reg_c	<=	'd0;
			reg_d	<=	'd0;
			reg_e	<=	'd0;
			reg_f	<=	'd0;
			reg_g	<=	'd0;
			reg_h	<=	'd0;
		end
	else if(load_en && reg_is_1st_msg_block)
		begin
			reg_a	<=	32'h7380166f;
			reg_b	<=	32'h4914b2b9;
			reg_c	<=	32'h172442d7;
			reg_d	<=	32'hda8a0600;
			reg_e	<=	32'ha96f30bc;
			reg_f	<=	32'h163138aa;
			reg_g	<=	32'he38dee4d;
			reg_h	<=	32'hb0fb0e4e;		
		end
	else if(load_en &&!reg_is_1st_msg_block)
		begin
			reg_a	<=	tmp_a;
			reg_b	<=	tmp_b;
			reg_c	<=	tmp_c;
			reg_d	<=	tmp_d;
			reg_e	<=	tmp_e;
			reg_f	<=	tmp_f;
			reg_g	<=	tmp_g;
			reg_h	<=	tmp_h;
		end	
	else if(working_en)
		begin
			reg_d	<=	reg_c;
			reg_c	<=	{reg_b[31 - 9 : 0], reg_b[31 : 31 - 9 + 1]};
			reg_b 	<=	reg_a;
			reg_a	<=	tt1;
			reg_h	<=	reg_g;
			reg_g	<=	{reg_f[31 - 19 : 0], reg_f[31 : 31 - 19 + 1]};
			reg_f	<=	reg_e;
			reg_e	<=	tt2_after_p0;
		end
	else
		begin
			reg_a	<=	reg_a;
			reg_b	<=	reg_b;
			reg_c	<=	reg_c;
			reg_d	<=	reg_d;
			reg_e	<=	reg_e;
			reg_f	<=	reg_f;
			reg_g	<=	reg_g;
			reg_h	<=	reg_h;
		end			


always@(posedge clk_in)
	if(!reset_n_in)
		data_after_cf_out	<=	'd0;
	else if(is_1st_data_block && reg_is_1st_msg_block)
		data_after_cf_out	<=	{32'h7380166f, 32'h4914b2b9, 32'h172442d7, 32'hda8a0600, 32'ha96f30bc, 32'h163138aa, 32'he38dee4d, 32'hb0fb0e4e};
	else if(msg_cf_finished == 1'b1)
		data_after_cf_out	<=	{reg_a,reg_b,reg_c,reg_d,reg_e,reg_f,reg_g,reg_h} ^ data_after_cf_out;
	else
		data_after_cf_out	<=	data_after_cf_out;
		

barrel_shifter u_barrel_shifter
	(
		.data_in(TJ),
		.shift_number_in(J_mod[4:0]),
		.data_after_shift_out(tmp_for_ss1_1)
	);


always@(posedge clk_in)					
	if(!reset_n_in)
		current_state	<=	`IDLE;
	else
		current_state	<=	next_state;
		
always@(*)
	begin	
		next_state	=	`IDLE;
		case(current_state)
			`IDLE:
					if(start_in)
						next_state = `LOAD;
					else
						next_state = `IDLE;
			`LOAD:
					next_state = `WORKING;
					
			`WORKING:
					if(index_j_in == 'd63)
						next_state = `IDLE;
					else
						next_state = `WORKING;
			default:
					next_state = `IDLE;
		endcase
	end
	
always@(posedge clk_in)
	if(!reset_n_in)
		begin
			working_en				<=	1'b0;
			msg_cf_finished	<=	1'b0;
		end
	else 
		begin
			
			if(current_state == `IDLE && next_state == `LOAD)
				load_en	<=	1'b1;
			else
				load_en	<=	1'b0;
			
			if(next_state == `WORKING)
				working_en <= 1'b1;
			else
				working_en	<=	1'b0;	
			
			if(current_state == `WORKING && next_state == `IDLE)
				msg_cf_finished	<=	'd1;
			else
				msg_cf_finished	<=	'd0;
		end


		
endmodule
	
/*
 * File: barrel_shifter.v
 * Project: rtl
 * File Created: Sunday, 5th August 2018 9:03:57 am
 * Author: Chen Rui (raymond.rui.chen@qq.com>)
 * -----
 * Last Modified: Sunday, 5th August 2018 9:12:40 am
 * Modified By: Chen Rui (raymond.rui.chen@qq.com>)
 * -----
 * Copyright (c) 2018 - Chen Rui
 * All rights reserved.
 */
module barrel_shifter
	(
		data_in,
		shift_number_in,
		data_after_shift_out
	);

input	[31 : 0]			data_in;
input	[4 : 0]				shift_number_in;
output	reg	[31 : 0]		data_after_shift_out;

always@(*)
	case(shift_number_in)
		5'b0_0000:	data_after_shift_out	<=	 data_in;
		5'b0_0001:	data_after_shift_out	<=	{data_in[31 - 1  : 0], data_in[31 : 31  - 1  + 1]};
		5'b0_0010:	data_after_shift_out	<=	{data_in[31 - 2  : 0], data_in[31 : 31  - 2  + 1]};
		5'b0_0011:	data_after_shift_out	<=	{data_in[31 - 3  : 0], data_in[31 : 31  - 3  + 1]};
		5'b0_0100:	data_after_shift_out	<=	{data_in[31 - 4  : 0], data_in[31 : 31  - 4  + 1]};
		5'b0_0101:	data_after_shift_out	<=	{data_in[31 - 5  : 0], data_in[31 : 31  - 5  + 1]};
		5'b0_0110:	data_after_shift_out	<=	{data_in[31 - 6  : 0], data_in[31 : 31  - 6  + 1]};
		5'b0_0111:	data_after_shift_out	<=	{data_in[31 - 7  : 0], data_in[31 : 31  - 7  + 1]};
		5'b0_1000:	data_after_shift_out	<=	{data_in[31 - 8  : 0], data_in[31 : 31  - 8  + 1]};
		5'b0_1001:	data_after_shift_out	<=	{data_in[31 - 9  : 0], data_in[31 : 31  - 9  + 1]};
		5'b0_1010:	data_after_shift_out	<=	{data_in[31 - 10 : 0], data_in[31 : 31  - 10 + 1]};
		5'b0_1011:	data_after_shift_out	<=	{data_in[31 - 11 : 0], data_in[31 : 31  - 11 + 1]};
		5'b0_1100:	data_after_shift_out	<=	{data_in[31 - 12 : 0], data_in[31 : 31  - 12 + 1]};
		5'b0_1101:	data_after_shift_out	<=	{data_in[31 - 13 : 0], data_in[31 : 31  - 13 + 1]};
		5'b0_1110:	data_after_shift_out	<=	{data_in[31 - 14 : 0], data_in[31 : 31  - 14 + 1]};
		5'b0_1111:	data_after_shift_out	<=	{data_in[31 - 15 : 0], data_in[31 : 31  - 15 + 1]};
		5'b1_0000:	data_after_shift_out	<=	{data_in[31 - 16 : 0], data_in[31 : 31  - 16 + 1]};
		5'b1_0001:	data_after_shift_out	<=	{data_in[31 - 17 : 0], data_in[31 : 31  - 17 + 1]};
		5'b1_0010:	data_after_shift_out	<=	{data_in[31 - 18 : 0], data_in[31 : 31  - 18 + 1]};
		5'b1_0011:	data_after_shift_out	<=	{data_in[31 - 19 : 0], data_in[31 : 31  - 19 + 1]};
		5'b1_0100:	data_after_shift_out	<=	{data_in[31 - 20 : 0], data_in[31 : 31  - 20 + 1]};
		5'b1_0101:	data_after_shift_out	<=	{data_in[31 - 21 : 0], data_in[31 : 31  - 21 + 1]};
		5'b1_0110:	data_after_shift_out	<=	{data_in[31 - 22 : 0], data_in[31 : 31  - 22 + 1]};
		5'b1_0111:	data_after_shift_out	<=	{data_in[31 - 23 : 0], data_in[31 : 31  - 23 + 1]};
		5'b1_1000:	data_after_shift_out	<=	{data_in[31 - 24 : 0], data_in[31 : 31  - 24 + 1]};
		5'b1_1001:	data_after_shift_out	<=	{data_in[31 - 25 : 0], data_in[31 : 31  - 25 + 1]};
		5'b1_1010:	data_after_shift_out	<=	{data_in[31 - 26 : 0], data_in[31 : 31  - 26 + 1]};
		5'b1_1011:	data_after_shift_out	<=	{data_in[31 - 27 : 0], data_in[31 : 31  - 27 + 1]};
		5'b1_1100:	data_after_shift_out	<=	{data_in[31 - 28 : 0], data_in[31 : 31  - 28 + 1]};
		5'b1_1101:	data_after_shift_out	<=	{data_in[31 - 29 : 0], data_in[31 : 31  - 29 + 1]};
		5'b1_1110:	data_after_shift_out	<=	{data_in[31 - 30 : 0], data_in[31 : 31  - 30 + 1]};
		5'b1_1111:	data_after_shift_out	<=	{data_in[31 - 31 : 0], data_in[31 : 31  - 31 + 1]}; 
	endcase

endmodule	