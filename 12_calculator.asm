;	// Open81 - A free firmware for the Timex TS1000 / Sinclair ZX81
;	// Copyright (C) 1981 Nine Tiles Networks Ltd.

;	// This program is free software; you can redistribute it and/or
;	// modify it under the terms of the GNU General Public License
;	// as published by the Free Software Foundation; either version 2
;	// of the License, or (at your option) any later version.

;	// This program is distributed in the hope that it will be useful,
;	// but WITHOUT ANY WARRANTY; without even the implied warranty of
;	// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;	// GNU General Public License for more details.

;	// You should have received a copy of the GNU General Public License
;	// along with this program; if not, write to the Free Software
;	// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

;	// section headings and labels from
;	// The Complete Timex TS1000 / Sinclair ZX81 ROM disassembly
;	// by Dr. Ian Logan & Dr. Frank O'Hara

	org $1915;	// addresses prior to this point are identical to version 2

;	// THE 'CALCULATOR' TABLES

table_con:
	defb $00, $00, $00, $00, $00;	// 0
	defb $81, $00, $00, $00, $00;	// 1
	defb $80, $00, $00, $00, $00;	// 1/2
	defb $81, $49, $0f, $da, $a2;	// PI/2
	defb $84, $20, $00, $00, $00;	// 10

table_adr:
	defw fp_jump_true;
	defw fp_exchange;
	defw fp_delete;
	defw fp_subtract;
	defw fp_multiply;
	defw fp_division;
	defw fp_to_power;
	defw fp_or;
	defw fp_no_and_no;
	defw fp_comparison;
	defw fp_comparison;
	defw fp_comparison;
	defw fp_comparison;
	defw fp_comparison;
	defw fp_comparison;
	defw fp_addition;
	defw fp_str_and_no;
	defw fp_comparison;
	defw fp_comparison;
	defw fp_comparison;
	defw fp_comparison;
	defw fp_comparison;
	defw fp_comparison;
	defw fp_strs_add;
	defw fp_negate;
	defw fp_code;
	defw fp_val;
	defw fp_len;
	defw fp_sin;
	defw fp_cos;
	defw fp_tan;
	defw fp_asn;
	defw fp_acs;
	defw fp_atn;
	defw fp_ln;
	defw fp_exp;
	defw fp_int;
	defw fp_sqr;
	defw fp_sgn;
	defw fp_abs;
	defw fp_peek;
	defw fp_usr;
	defw fp_strS;
	defw fp_chrS;
	defw fp_not;
	defw fp_move_fp;
	defw fp_n_mod_m;
	defw fp_jump;
	defw fp_stk_data;
	defw fp_dec_jr_nz;
	defw fp_less_0;
	defw fp_greater_0;
	defw fp_end_calc;
	defw fp_get_argt;
	defw fp_truncate;
	defw fp_calc_2;
	defw fp_e_to_fp;

tbl_offs equ $ - table_adr;
	defw fp_series_xx;
	defw fp_stk_con_xx;
	defw fp_st_mem_xx;
	defw fp_get_mem_xx;

;	// THE 'CALCULATOR ' SUBROUTINE

calculate:
	call stk_pntrs;						// HL to last value on calculator stack

gen_ent_1:
	ld a, b;							// offset or parameter
	ld (breg),a;						// to breg

gen_ent_2:
	exx;								// store subroutine
	ex (sp), hl;						// return address
	exx;								// in HL'

re_entry:
	ld (stkend), de;					// set to stack end
	exx;								// use alternate register set
	ld a, (hl);							// get literal
	inc hl;								// HL' points to next literal

scan_ent:
	push hl;							// stack it
	and a;								// test A
	jp p, first_38;						// jump with 0 to 56
	ld d, a;							// literal to D
	and %01100000;						// preserve bit 5 and 6
	rrca;								// shift
	rrca;								// right
	rrca;								// into
	rrca;								// bit 1 and 2
	add a, tbl_offs;					// offsets
	ld l, a;							// L holds doubled offset
	ld a, d;							// get parameter
	and %00011111;						// from bits 0 to 4
	jr ent_table;						// address routine

first_38:
	cp 24;								// unary operation?
	jr nc, double_a;					// jump if so
	exx;								// main register set
	call stk_pntrs_2;					// get pointers to operands
	exx;								// alternate register set

double_a:
	rlca;								// two bytes per entry
	ld l, a;							// so double offset

ent_table:
	ld de, table_adr;;					// base address
	ld h, 0;							// offset now in HL
	add hl, de;							// calculate address
	ld e, (hl);							// get address
	inc hl;								// of routine
	ld d, (hl);							// in DE
	ld hl, re_entry;					// stack re_entry
	ex (sp), hl;						// address
	push de;							// stack routine address
	exx;								// main register set
	ld bc, (stkend_h);					// breg to B

;	// THE 'DELETE' SUBROUTINE

fp_delete:
	ret;								// end of subroutineindirect jump to subroutine

;	// THE 'SINGLE OPERATION' SUBROUTINE

fp_calc_2:
	pop af;								// drop re-entry address
	ld a, (breg);						// offset to A
	exx;								// alternate register set
	jr scan_ent;						// immediate jump

;	// THE 'TEST 5_SPACES' SUBROUTINE

test_5_sp:
	push de;							// stack DE
	push hl;							// stack HL
	ld bc, 5;							// 5 bytes
	call test_room;						// test for space
	pop hl;								// unstack HL
	pop de;								// unstack DE
	ret;								// end of subroutine

;	// THE 'MOVE A FLOATING_POINT NUMBER' SUBROUTINE

fp_move_fp:
	call test_5_sp;						// test for space
	ldir;								// copy five bytes
	ret;								// end of subroutine

;	// THE 'STACK LITERALS' SUBROUTINE

fp_stk_data:
	ld h, d;							// DE
	ld l, e;							// to HL

stk_const:
	call test_5_sp;						// test for space
	exx;								// alternate register set
	push hl;							// stack pointer to next literal
	exx;								// main register set
	ex (sp), hl;						// swap result and next literal pointers
	ld a, (hl);							// first literal to A
	and %11000000;						// divide by 64
	rlca;								// to give values
	rlca;								// 0 to 3
	ld c, a;							// value to C
	inc c;								// incremented (1 to 4)
	ld a, (hl);							// get literal again
	and %00111111;						// modulo 64
	jr nz, form_exp;					// jump if remainder not zero
	inc hl;								// else get next literal
	ld a, (hl);							// and leave unreduced

form_exp:
	add a, 80;							// add 80 to get exponent
	ld (de), a;							// put it on calculator stack
	ld a, 5;							// get number
	sub c;								// of literals
	inc hl;								// next literal
	inc de;								// next position in calculator stack
	ldir;								// copy literals
	ex (sp), hl;						// restore result and next literal pointers
	exx;								// alternate register set
	pop hl;								// unstack next literal to HL'
	exx;								// main register set
	ld b, a;							// zero bytes to B
	xor a;								// LD A, 0

stk_zeros:
	dec b;								// reduce count
	ret z;								// return if done
	ld (de), a;							// stack a zero
	inc de;								// next position in calculator stack
	jr stk_zeros;						// loop until done

;	// THE 'MEMORY LOCATION' SUBROUTINE

loc_mem:
	ld c, a;							// parameter to C
	rlca;								// multiply
	rlca;								// by
	add a, c;							// five
	ld c, a;							// result
	ld b, 0;							// to BC
	add hl, bc;							// get base address
	ret;								// end of subroutine

;	// THE 'GET FROM MEMORY AREA' SUBROUTINE

fp_get_mem_xx:
	ld hl, (mem);						// get pointer to memory area

fp_get_mem_xx_2:
	push de;							// stack result pointer
	call loc_mem;						// get base address
	call fp_move_fp;					// move five bytes
	pop hl;								// unstack result pointer
	ret;								// end of subroutine

;	// THE 'STACK A CONSTANT' SUBROUTINE

fp_stk_con_xx:
	ld hl, table_con;					// address of table of constants
	jr fp_get_mem_xx_2;					// indirect exit to stack constant

;	// THE 'STORE IN MEMORY AREA' SUBROUTINE

fp_st_mem_xx:
	push hl;							// stack result pointer
	ex de, hl;							// source to DE
	ld hl, (mem);						// pointer to memory area to HL
	call loc_mem;						// get base address
	ex de, hl;							// exchange pointers
	ld c, 5;							// five bytes
	ldir;								// copy
	ex de, hl;							// exchange pointers
	pop hl;								// unstack result pointer
	ret;								// end of subroutine

;	// THE 'EXCHANGE' SUBROUTINE

fp_exchange:
	ld b, 5;							// five bytes

swap_byte:
	ld a, (de);							// get each byte of second
	ld c, a;							//
	ld a, (hl);							// and first
	ld (de), a;							// first number to (DE)
	ld (hl), c;							// second number to (HL)
	inc hl;								// consider next
	inc de;								// pair of bytes
	djnz swap_byte;						// exchange five bytes
	ret;								// end of subroutine

;	// THE 'SERIES GENERATOR ' SUBROUTINE
fp_series_xx:
	ld b, a;							// parameter to B
	call gen_ent_1;						// enter calc and set counter
	defb duplicate;
	defb addition;
	defb st_mem_0;
	defb delete;
	defb stk_zero;
	defb st_mem_2

g_loop:
	defb duplicate;
	defb get_mem_0;
	defb multiply;
	defb get_mem_2;
	defb st_mem_1;
	defb subtract;
	defb end_calc;
	call fp_stk_data;					// b(r), 2 * b(r) * z - b(r - 1), a(r + 1)
	call gen_ent_2;						// re-enter calc without disturbing breg
	defb addition;
	defb exchange;
	defb st_mem_2;
	defb delete;
	defb dec_jr_nz, g_loop - $ - 1;
	defb get_mem_1;
	defb subtract;
	defb end_calc;
	ret;								// end of subroutine

;	// THE 'ABSOLUTE MAGNITUDE' FUNCTION

fp_abs:
	inc hl;
	res 7, (hl);
	dec hl;
	ret;

;	// THE 'UNARY MINUS' OPERATION

fp_negate:
	ld a, (hl);							// get first byte
	and a;								// zero?
	ret z;
	inc hl;								// next byte
	ld a, (hl);
	xor $80;
	ld (hl), a;							// store second byte
	dec hl;								// point to first byte
	ret;								// end of subroutine

;	// THE 'SIGNUM' FUNCTION

fp_sgn:
	inc hl;
	ld a, (hl);
	dec hl;
	dec (hl);
	inc (hl);
	scf;
	call nz, fp_0_div_1;
	inc hl;
	rlca;
	rr (hl);
	dec hl;
	ret;

;	// THE 'PEEK' FUNCTION

fp_peek:
	call find_int;						// get address in BC
	ld a, (bc);							// get byte
	jp stack_a;							// indirect exit

;	// THE'USR' FUNCTION

fp_usr:
	call find_int;						// get address in BC
	ld hl, stack_bc;					// stack
	push hl;							// routine address
	push bc;							// stack address
	ret;								// end of subroutine

;	// THE 'GREATER THAN ZERO' OPERATION

fp_greater_0:
	ld a, (hl);
	and a;
	ret z;
	ld a, $ff;							// sign byte
	jr sign_to_c;						// immediate jump

;	// THE 'NOT' FUNCTION

fp_not:
	ld a, (hl);
	neg;
	ccf;
	jr fp_0_div_1;

;	// THE 'LESS THAN ZERO' OPERATION

fp_less_0:
	xor a;								// LD A, 0

sign_to_c:
	inc hl;								// point to sign
	xor (hl);							// carry reset if positive, set if negative
	dec hl;								// restore result pointer
	rlca;								// opposite effect from fp_greater_0

fp_0_div_1:
	push hl;							// stack result pointer
	ld b, $05;

fp_zero:
	ld (hl), $00;
	inc hl;
	djnz fp_zero;
	pop hl;
	ret nc;
	ld (hl), $81;
	ret;								// end of subroutine

;	// THE 'OR ' OPERATION

fp_or:
	ld a, (de);
	and a;
	ret z;
	scf;
	jr fp_0_div_1;						// immediate jump

;	// THE 'NUMBER AND NUMBER' OPERATION

fp_no_and_no:
	ld a, (de);
	and a;
	ret nz;
	jr fp_0_div_1;						// immediate jump

;	// THE 'string and number' operation

fp_str_and_no:
	ld a, (de);							// get first byte
	and a;								// test for zero
	ret nz;								// return if not zero
	dec de;								// point to fifth string byte
	xor a;								// LD A, 0
	ld (de), a;							// zero high byte of length
	dec de;								// point to fourth string byte
	ld (de), a;							// zero low byte of length
	inc de;								// restore
	inc de;								// pointer
	ret;								// end of subroutine

;	// THE 'COMPARISON' OPERATIONS

fp_comparison:
	ld a, b;							// offset to A
	bit 2, a;							// >= 4?
	jr nz, ex_or_not;					// jump if not
	dec a;								// reduce range

ex_or_not:
	rrca;								// set carry for >= and <
	jr nc, nu_or_str;					// jump if carry flag not set
	push af;							// stack AF
	push hl;							// stack HL
	call fp_exchange;					// exchange
	pop de;								// unstack DE
	ex de, hl;							// swap HL with DE
	pop af;								// unstack AF

nu_or_str:
	rrca;								// update carry flag
	push af;							// stack it
	bit 2, a;							// string comparison?
	jr nz, strings;						// jump if so
	call fp_subtract;					// subtraction
	jr end_tests;						// end of subroutine

strings:
	call stk_fetch;						// length and start address of second string
	push de;							// stack
	push bc;							// them
	call stk_fetch;						// first string
	pop hl;								// unstack second string length

byte_comp:
	ld a, h;							// is second
	or l;								// string null?
	ex (sp), hl;						// swap HL with (SP)
	ld a, b;							// 
	jr nz, sec_plus;					// jump if not null
	or c;								// both null?

secnd_low:
	pop bc;								// unstack BC
	jr z, both_null;					// jump if so
	pop af;								// restore carry flag
	ccf;								// and complement
	jr str_test;						// immediate jump

both_null:
	pop af;								// restore carry flag
	jr str_test;						// immediate jump

sec_plus:
	or c;								// first less?
	jr z, frst_less;					// jump if so
	ld a, (de);							// compare
	sub (hl);							// next byte
	jr c, frst_less;					// jump if first byte less
	jr nz, secnd_low;					// jump if second byte less
	dec bc;								// bytes equal
	inc de;								// reduce
	inc hl;								// lengths
	ex (sp), hl;						// and jump
	dec hl;								// to compare
	jr byte_comp;						// next byte

frst_less:
	pop bc;								// unstack BC
	pop af;								// unstack AF
	and a;								// clear carry flag

str_test:
	push af;							// stack carry flag
	rst fp_calc;						// x
	defb stk_zero;						// x, 0
	defb end_calc;						// exit calculator

end_tests:
	pop af;								// unstack carry flag
	push af;							// restack carry flag
	call c, fp_not;						// jump if set
	call nc, fp_greater_0;				// jump if not set
	pop af;								// unstack carry flag
	rrca;								// rotate into carry
	call nc, fp_not;					// jump if not set
	ret;								// end of subroutine

;	// THE 'STRING CONCATENATION' OPERATION

fp_strs_add:
	call stk_fetch;						// get parameters of
	push de;							// second string
	push bc;							// and stack them
	call stk_fetch;						// get parameters of fisrt string
	pop hl;								// unstack length to HL
	push hl;							// and restack
	push de;							// stack paramters
	push bc;							// of fisrt string
	add hl, bc;							// find total length
	ld b, h;							// and store
	ld c, l;							// in BC
	rst bc_spaces;						// make space
	call stk_store;						// parameters to calculator stack
	pop bc;								// unstack first
	pop hl;								// string parameters
	call l_enter_1;						// else copy to workspace

other_str:
	pop bc;								// unstack second
	pop hl;								// string parameters
	call l_enter_1;						// else copy to workspace

;	// THE 'STK_PNTRS' SUBROUTINE

stk_pntrs:
	ld hl, (stkend);					// stack end to HL

stk_pntrs_2:
	ld e, l	;							// DE points to second
	ld d, h;							// operand
	dec hl;								// make
	dec hl;								// HL
	dec hl;								// point
	dec hl;								// to first
	dec hl;								// operand
	ret;								// end of subroutine

;	// THE 'CHR$' FUNCTION

fp_chrS:
	call fp_to_a;						// last value to A
	jr c, report_b2;					// error if greater than 255
	jr nz, report_b2;					// or negative
	ld bc, 1;							// make one space
	rst bc_spaces;
	ld (de), a;							// value to workspace
	jr fp_strS_1;						// exit

report_b2:
	rst error_1;
	defb integer_out_of_range;

;	// THE 'VAL' FUNCTION

fp_val:
	ld hl, (ch_add);					// get current value of ch-add
;	rst get_ch;							// shorter but slower
	push hl;							// stack it
	call stk_fetch;						// get parameters
	push de;							// stack start address
	inc bc;								// increase length by one
	rst bc_spaces;						// make space
	pop hl;								// unstack start address
	ld (ch_add), de;					// pointer to ch_add
	push de;							// stack it
	ldir;								// copy the string to the workspace
	ex de, hl;							// swap pointers
	dec hl;								// last byte of string
	ld (hl), ctrl_newline;				// replace with carriage return
	res 7, (iy + _flags);				// reset syntax flag
	call class_6;						// check syntax
	call check_2;						// end of expression?
	pop hl;								// unstack start address
	ld (ch_add), hl;					// start address to ch_add
	set 7, (iy + _flags);				// set line execution flag
	call scanning;						// use string as next expression
	pop hl;								// get last value
	ld (ch_add), hl;					// and restore ch_add
	jr stk_pntrs;						// exit and reset pointers

;	// THE 'STR$' FUNCTION

fp_strS:
	ld bc, 1;							// make one space
	rst bc_spaces;
	ld (hl), ctrl_newline;				// put newline
	ld hl, (s_posn);					// get column/line
	push hl;							// stack it
	ld l, $ff;
	ld (s_posn), hl;
	ld hl, (df_cc);
	push hl;							// stack it
	ld (df_cc), de;
	push de;
	call print_fp;						// print last value
	pop de;
	ld hl, (df_cc);
	and a;								// calculate
	sbc hl, de;							// length
	ld b, h;							// store in 
	ld c, l;							// BC
	pop hl;
	ld (df_cc), hl;
	pop hl;
	ld (s_posn), hl;

fp_strS_1:
	call stk_store;						// parameters to calculator stack
	ex de, hl;							// reset pointers
	ret;								// end of subroutine

;	// THE 'CODE' FUNCTION

fp_code:
	call stk_fetch;						// get string parameters
	ld a, b;							// test for
	or c;								// zero
	jr z, stk_code;						// jump with null string
	ld a, (de);							// code of first character to A

stk_code:
	jp stack_a;							// exit and return value

;	// THE 'LEN' FUNCTION

fp_len:
	call stk_fetch;						// get string parameters
	jp stack_bc;						// exit and return length

;	// THE 'DECREASE THE COUNTER' SUBROUTINE

fp_dec_jr_nz:
	exx;								// alternate register set
	push hl;							// stack literal pointer
	ld hl, breg;						// get breg (counter)
	dec (hl);							// reduce it
	pop hl;								// unstack literal pointer
	jr nz, jump_2;						// jump if not zero
	inc hl;								// next literal
	exx;								// main register set
	ret;								// end of subroutine

;	// THE 'JUMP' SUBROUTINE

fp_jump:
	exx;								// alternate register set

jump_2:
	ld e, (hl);
	xor a;
	bit 7, e;
	jr z, new_addr;
	cpl;

new_addr:
	ld d, a;
	add hl, de;
	exx;
	ret;

;	// THE 'JUMP ON TRUE' SUBROUTINE

fp_jump_true:
	ld a, (de);
	and a;
	jr nz, fp_jump;
	exx;
	inc hl;
	exx;
	ret;

;	// THE 'MODULUS' SUBROUTINE

fp_n_mod_m:
	rst fp_calc;
	defb st_mem_1;
	defb delete;
	defb duplicate;
	defb get_mem_1;
	defb division;
	defb int;
	defb get_mem_1;
	defb exchange;
	defb st_mem_1;
	defb multiply;
	defb subtract;
	defb get_mem_1;
	defb end_calc;
	ret;

;	// THE 'INT' FUNCTION

fp_int:
	rst fp_calc;
	defb duplicate;
	defb less_0;
	defb jump_true, x_neg - $ - 1;
	defb truncate;
	defb end_calc;
	ret;

x_neg:
	defb duplicate;
	defb truncate;
	defb st_mem_0;
	defb subtract;
	defb get_mem_0;
	defb exchange;
	defb fn_not;
	defb jump_true, exit - $ - 1;
	defb stk_one;
	defb subtract;

exit:
	defb end_calc;
	ret;

;	// THE 'EXPONENTIAL' FUNCTION

fp_exp:
	rst fp_calc;
	defb stk_data;
	defb $f1, $38, $aa, $3b, $29;
	defb multiply;
	defb duplicate;
	defb int;
	defb st_mem_3;
	defb subtract;
	defb duplicate;
	defb addition;
	defb stk_one;
	defb subtract;
	defb $88;
	defb $13, $36;
	defb $58, $65, $66;
	defb $9d, $78, $65, $40;
	defb $a2, $60, $32, $c9;
	defb $e7, $21, $f7, $af, $24;
	defb $eb, $2f, $b0, $b0, $14;
	defb $ee, $7e, $bb, $94, $58;
	defb $f1, $3a, $7e, $f8, $cf;
	defb get_mem_3;
	defb end_calc;
	call fp_to_a;
	jr nz, n_negtv;
	jr c, report6_2;
	add a, (hl);
	jr nc, result_ok;

report6_2:
	rst error_1 ;
	defb arithmetic_overflow;

n_negtv:
	jr c, rslt_zero;					// zero if n < -255
	sub (hl);							// subtract abs(n)
	jr nc, rslt_zero;					// zero if e < zero
	neg;								// -e to +e

result_ok:
	ld (hl), a;							// store exponent
	ret;								// exit

rslt_zero:
	rst fp_calc;
	defb delete;
	defb stk_zero;
	defb end_calc;
	ret;

;	// THE 'NATURAL LOGARITHM' FUNCTION

fp_ln:
	rst fp_calc;
	defb duplicate;
	defb greater_0;
	defb jump_true, valid - $ - 1;
	defb end_calc;

report_a:
	rst error_1;
	defb invalid_argument;

valid:
	defb end_calc;
	ld a, (hl);							// exponent to A
	ld (hl), $80;						// x to x'
	call stack_a;
	rst fp_calc;
	defb stk_data;
	defb $38, $00;
	defb subtract;
	defb exchange;
	defb duplicate;
	defb stk_data;
	defb $f0, $4c, $cc, $cc, $cd;
	defb subtract;
	defb greater_0;
	defb jump_true, gre_8 - $ - 1;
	defb exchange;
	defb stk_one;
	defb subtract;
	defb exchange;
	defb end_calc;
	inc (hl);
	rst fp_calc;

gre_8:
	defb exchange;
	defb stk_data;
	defb $f0, $31, $72, $17, $f8;
	defb multiply;
	defb exchange;
	defb stk_half;
	defb subtract;
	defb stk_half;
	defb subtract;
	defb duplicate;
	defb stk_data;
	defb $32, $20;
	defb multiply;
	defb stk_half;
	defb subtract;
	defb $8c;
	defb $11, $ac;
	defb $14, $09;
	defb $56, $da, $a5;
	defb $59, $30, $c5;
	defb $5c, $90, $aa;
	defb $9e, $70, $6f, $61;
	defb $a1, $cb, $da, $96;
	defb $a4, $31, $9f, $b4;
	defb $e7, $a0, $fe, $5c, $fc;
	defb $ea, $1b, $43, $ca, $36;
	defb $ed, $a7, $9c, $7e, $5e;
	defb $f0, $6e, $23, $80, $93;
	defb multiply;
	defb addition;
	defb end_calc;
	ret;

;	// THE 'REDUCE ARGUMENT' SUBROUTINE

fp_get_argt:
	rst fp_calc;
	defb stk_data;
	defb $ee, $22, $f9, $83, $6e;
	defb multiply;
	defb duplicate;
	defb stk_half;
	defb addition;
	defb int;
	defb subtract;
	defb duplicate;
	defb addition;
	defb duplicate;
	defb addition;
	defb duplicate;
	defb abs;
	defb stk_one;
	defb subtract;
	defb duplicate;
	defb greater_0;
	defb st_mem_0;
	defb jump_true, zplus - $ - 1;
	defb delete;
	defb end_calc;
	ret;

zplus:
	defb stk_one;
	defb subtract;
	defb exchange;
	defb less_0;
	defb jump_true, yneg - $ - 1;
	defb negate;

yneg:
	defb end_calc;
	ret;

;	// THE 'COSINE' FUNCTION

fp_cos:
	rst fp_calc;
	defb get_argt;
	defb abs;
	defb stk_one;
	defb subtract;
	defb get_mem_0;
	defb jump_true, c_ent - $ - 1;
	defb negate;
	defb jump, c_ent - $ - 1;

;	// THE 'SINE' FUNCTION

fp_sin:
	rst fp_calc;
	defb get_argt;

c_ent:
	defb duplicate;
	defb duplicate;
	defb multiply;
	defb duplicate;
	defb addition;
	defb stk_one;
	defb subtract;
	defb $86;
	defb $14, $e6;
	defb $5c, $1f, $0b;
	defb $a3, $8f, $38, $ee;
	defb $e9, $15, $63, $bb, $23;
	defb $ee, $92, $0d, $cd, $ed;
	defb $f1, $23, $5d, $1b, $ea;
	defb multiply;
	defb end_calc;
	ret;

;	// THE 'TANGENT' FUNCTION

fp_tan:
	rst fp_calc;
	defb duplicate;
	defb sin;
	defb exchange;
	defb cos;
	defb division;
	defb end_calc;
	ret;

;	// THE 'ARCTANGENT' FUNCTION

fp_atn:
	ld a, (hl);
	cp $81;
	jr c, small;
	rst fp_calc;
	defb stk_one;
	defb negate;
	defb exchange;
	defb division;
	defb duplicate;
	defb less_0;
	defb stk_pi_div_2;
	defb exchange;
	defb jump_true, cases - $ - 1;
	defb negate;
	defb jump, cases - $ - 1;

small:
	rst fp_calc;
	defb stk_zero;

cases:
	defb exchange;
	defb duplicate;
	defb duplicate;
	defb multiply;
	defb duplicate;
	defb addition;
	defb stk_one;
	defb subtract;
	defb $8c;
	defb $10, $b2;
	defb $13, $0e;
	defb $55, $e4, $8d;
	defb $58, $39, $bc;
	defb $5b, $98, $fd;
	defb $9e, $00, $36, $75;
	defb $a0, $db, $e8, $b4;
	defb $63, $42, $c4;
	defb $e6, $b5, $09, $36, $be;
	defb $e9, $36, $73, $1b, $5d;
	defb $ec, $d8, $de, $63, $be;
	defb $f0, $61, $a1, $b3, $0c;
	defb multiply;
	defb addition;
	defb end_calc;
	ret;

;	// THE 'ARCSINE' FUNCTION

fp_asn:
	rst fp_calc;
	defb duplicate;
	defb duplicate;
	defb multiply;
	defb stk_one;
	defb subtract;
	defb negate;
	defb sqr;
	defb stk_one;
	defb addition;
	defb division;
	defb atn;
	defb duplicate;
	defb addition;
	defb end_calc;
	ret;

;	// THE 'ARCCOSINE' FUNCTION

fp_acs:
	rst fp_calc;
	defb asn;
	defb stk_pi_div_2;
	defb subtract;
	defb negate;
	defb end_calc;
	ret;

;	// THE 'SQUARE ROOT' FUNCTION

fp_sqr:
	rst fp_calc;						// x
	defb st_mem_0;						// store in mem-0
	defb end_calc;						// exit calculator
	ld a, (hl);							// value to A
	and a;								// test against zero
	ret z;								// return if so
	add a, 128;							// set carry if greater or equal to 128
	rra;								// divide by two
	ld (hl), a;							// replace value
	inc hl;								// next location
	ld a, (hl);							// get sign bit
	rla;								// rotate left
	jp c, report_a;						// error with negative number
	ld (hl), 127;						// mantissa starts at about one
	ld b, 5;							// set counter

fp_sqr_1:
	rst fp_calc;						// x
	defb duplicate;						// x, x
	defb get_mem_0;						// x, x, n
	defb exchange;						// x, n, x
	defb division;						// x, n / x
	defb addition;						// x + n / x
	defb end_calc;						// exit calculator
	dec (hl);							// halve value
	djnz fp_sqr_1;						// loop until found
	ret;								// end of subroutine

;	// THE 'EXPONENTIATION' OPERATION

fp_to_power:
	rst fp_calc;
	defb exchange;
	defb duplicate;
	defb fn_not;
	defb jump_true, xis0 - $ - 1;
	defb ln;
	defb multiply;
	defb end_calc;
	jp fp_exp;

xis0:
	defb delete;
	defb duplicate;
	defb fn_not;
	defb jump_true, one - $ - 1;
	defb stk_zero;
	defb exchange;
	defb greater_0;
	defb jump_true, last - $ - 1;
	defb end_calc;
	rst error_1;
	defb arithmetic_overflow;

one:
	defb delete;
	defb stk_one;

last:
	defb end_calc;
	ret;

spare:
;	defs font - $, $ff;					// 19 spare bytes
