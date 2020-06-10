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

e_format:
	cp capital_e;
	ret nz;
	ld (iy + _membot), $ff;
	rst next_ch;
	cp plus;
	jr z, sign_done;
	cp minus;
	jr nz, st_e_part;
	inc (iy + _membot);

sign_done:
	rst next_ch;

st_e_part:
	call int_to_fp;
	rst fp_calc;
	defb get_mem_0;
	defb jump_true, e_fp - $ - 1;
	defb negate;

e_fp:
	defb e_to_fp;
	defb end_calc;
	ret;

;	// THE 'STK-DIGIT' SUBROUTINE

stk_digit:
	cp zero;
	ret c;
	cp $26;
	ccf;
	ret c;
	sub zero;

;	// THE 'STACK-A' SUBROUTINE

stack_a:
	ld c, a;							// value to
	ld b, 0;							// BC

;	// THE 'STACK_BC' SUBROUTINE

stack_bc:
	ld iy, err_nr;
	push bc;
	rst fp_calc;
	defb stk_zero;
	defb end_calc;
	pop bc;
	ld (hl), $91;
	ld a, b;
	and a;
	jr nz, norml_fp;
	ld (hl), a;
	or c;
	ret z;
	ld b, c;
	ld c, (hl);
	ld (hl), $89;

norml_fp:
	dec (hl);
	sla c;
	rl b;
	jr nc, norml_fp;
	srl b;
	rr c;
	inc hl;
	ld (hl), b;
	inc hl;
	ld (hl), c;
	dec hl;
	dec hl;
	ret;

;	// THE 'INTEGER TO FLOATING-POINT' SUBROUTINE

int_to_fp:
	push af;
	rst fp_calc;
	defb stk_zero;
	defb end_calc;
	pop af;

nxt_dgt_2:
	call stk_digit;
	ret c;
	rst fp_calc;
	defb exchange;
	defb stk_ten;
	defb multiply;
	defb addition;
	defb end_calc;
	rst next_ch;
	jr nxt_dgt_2;

;	// THE 'E-FORMAT TO FLOATING-POINT' SUBROUTINE 
fp_e_to_fp:
	rst fp_calc;
	defb duplicate;
	defb less_0;
	defb st_mem_0;
	defb delete;
	defb abs;

e_vet:
	defb stk_one;
	defb subtract;
	defb duplicate;
	defb less_0;
	defb jump_true, end_e - $ - 1;
	defb duplicate;
	defb stk_data;
	defb $33, $40;
	defb subtract;
	defb duplicate;
	defb less_0;
	defb jump_true, e_one - $ - 1;
	defb exchange;
	defb delete;
	defb exchange;
	defb stk_data;
	defb $80, $48, $18, $96, $80;
	defb jump, e_m_d - $ - 1;

e_one:
	defb delete;
	defb exchange;
	defb stk_ten;

e_m_d:
	defb get_mem_0;
	defb jump_true, e_div - $ - 1;
	defb multiply;
	defb jump, e_exc - $ - 1;

e_div:
	defb division;

e_exc:
	defb exchange;
	defb jump, e_vet - $ - 1;

end_e:
	defb delete;
	defb end_calc;
	ret;

;	// THE 'FLOATING-POINT TO BC' SUBROUTINE

fp_to_bc:
	call stk_fetch;
	and a;
	jr nz, not_zero;
	ld b, a;
	ld c, a;
	push af;
	jr fbc_end;

not_zero:
	ld b, e;
	ld e, c;
	ld c, d;
	sub $91;
	ccf;
	bit 7, b;
	push af;
	set 7, b;
	jr c, fbc_end;
	inc a;
	neg;
	cp $08;
	jr c, shift_tst;
	ld e, c;
	ld c, b;
	ld b, $00;
	sub $08;

shift_tst:
	and a;
	ld d, a;
	ld a, e;
	rlca;
	jr z, in_place;

shift_bc:
	srl b;
	rr c;
	dec d;
	jr nz, shift_bc;

in_place:
	jr nc, fbc_end;
	inc bc;
	ld a, b;
	or c;
	jr nz, fbc_end;
	pop af;
	scf;
	push af;

fbc_end:
	push bc;
	rst fp_calc;
	defb end_calc;
	pop bc;
	pop af;
	ld a, c;
	ret;

;	// THE 'FLOATING-POINT TO A' SUBROUTINE

fp_to_a:
	call fp_to_bc;						// last value on calc stack to BC
	ret c;								// return if out of range
	push af;							// stack result and flags
	dec b;								// is B
	inc b;								// zero?
	jr z, fp_a_end;						// jump if not
	pop af;								// unstack result and flags
	scf;								// signal out of range
	ret;								// end of subroutine

fp_a_end:
	pop af;								// unstack result and flags
	ret;								// end of subroutine

;	// THE 'PRINT A FLOATING-POINT NUMBER' SUBROUTINE

print_fp:
	rst fp_calc;
	defb duplicate;
	defb less_0;
	defb jump_true, p_neg - $ - 1;
	defb duplicate;
	defb greater_0;
	defb jump_true, p_pos - $ - 1;
	defb delete;
	defb end_calc;
	ld a, zero;
	rst print_a;
	ret;

p_neg:
	defb abs;
	defb end_calc;
	ld a, minus;
	rst print_a;
	rst fp_calc;

p_pos:
	defb end_calc;
	ld a, (hl);
	call stack_a;
	rst fp_calc;
	defb stk_data;
	defb $78, $00, $80;
	defb subtract;
	defb stk_data;
	defb $ef, $1a, $20, $9a, $85;
	defb multiply;
	defb int;
	defb st_mem_1;
	defb stk_data;
	defb $34, $00;
	defb subtract;
	defb negate;
	defb e_to_fp;
	defb stk_half;
	defb addition;
	defb int;
	defb end_calc;
	ld hl, $406b;	// last byte mem2;
	ld (hl), $90;
	ld b, $0a;

nxt_dgt_3:
	inc hl;
	push hl;
	push bc;
	rst fp_calc;
	defb stk_ten;
	defb n_mod_m;
	defb exchange;
	defb end_calc;
	call fp_to_a;
	or $90;
	pop bc;
	pop hl;
	ld (hl), a;
	djnz nxt_dgt_3;
	inc hl;
	ld bc, $0008;
	push hl;

get_first:
	dec hl;
	ld a, (hl);
	cp $90;
	jr z, get_first;
	sbc hl, bc;
	push hl;
	ld a, (hl);
	add a, $6b;
	push af;

round_up:
	pop af;
	inc hl;
	ld a, (hl);
	adc a, $00;
	daa;
	push af;
	and $0f;
	ld (hl), a;
	set 7, (hl);
	jr z, round_up;
	pop af;
	pop hl;
	ld b, $06;

markers:
	ld (hl), $80;
	dec hl;
	djnz markers;
	rst fp_calc;
	defb delete;
	defb get_mem_1;
	defb end_calc;
	call fp_to_a;
	jr z, signd_exp;
	neg;

signd_exp:
	ld e, a;
	inc e;
	inc e;
	pop hl;

get_fst_2:
	dec hl;
	dec e;
	ld a, (hl);
	and $0f;
	jr z, get_fst_2;
	ld a, e;
	sub $05;
	cp $08;
	jp p, e_needed;
	cp $f6;
	jp m, e_needed;
	add a, $06;
	jr z, out_zero;
	jp m, exp_minus;
	ld b, a;

out_b_chs:
	call out_next;
	djnz out_b_chs;
	jr test_int;

e_needed:
	ld b, e;
	call out_next;
	call test_int;
	ld a, capital_e;
	rst print_a;
	ld a, b;
	and a;
	jp p, plus_sign;
	neg;
	ld b, a;
	ld a, minus;
	jr out_sign;

plus_sign:
	ld a, plus;

out_sign:
	rst print_a;
	ld a, b;
	ld b, $ff;

ten_more:
	inc b;
	sub $0a;
	jr nc, ten_more;
	add a, $0a;
	ld c, a;
	ld a, b;
	and a;
	jr z, byte_two;
	call out_code;

byte_two:
	ld a, c;
	call out_code;
	ret;

exp_minus:
	neg;
	ld b, a;
	ld a, period;
	rst print_a;

out_zeros:
	ld a, zero;
	rst print_a;
	djnz out_zeros;
	jr test_done;

out_zero:
	ld a, zero;
	rst print_a;

;	// THE 'TEST-INT' SUBROUTINE

test_int:
	dec (hl);
	inc (hl);
	ret pe;
	ld a, period;
	rst print_a;

;	// THE 'TEST-DONE' SUBROUTINE

test_done:
	dec (hl);
	inc (hl);
	ret pe;
	call out_next;
	jr test_done;

;	// THE 'OUT_NEXT' SUBROUTINE

out_next:
	ld a, (hl);
	and $0f;
	call out_code;
	dec hl;
	ret;

;	// THE 'PREPARE TO ADD' SUBROUTINE

prep_add:
	ld a, (hl);							// exponent to A
	and a;								// zero?
	ret z;								// return if so
	ld (hl), 0;							// make zero for positive
	inc hl;								// address sign byte
	bit 7, (hl);						// set zero flag for positive
	set 7, (hl);						// restore true numeric bit
	dec hl;								// step back
	ret z;								// return with positive
	push bc;							// stack exponent
	ld bc, 5;							// five bytes
	add hl, bc;							// address byte after last byte
	ld b, c;							// count to B
	ld c, a;							// exponent to C
	scf;								// set carry flag for negative

neg_byte:
	dec hl;								// each byte in turn
	ld a, (hl);							// get it
	cpl;								// complement carry flag
	adc a, 0;							// add in carry for negation
	ld (hl), a;							// store it
	djnz neg_byte;						// loop until done
	ld a, c;							// exponent to A
	pop bc;								// unstack exponent
	ret;								// end of subroutine

;	// THE 'FETCH TWO NUMBERS' SUBROUTINE

fetch_two:
	push hl;							// stack HL
	push af;							// and AF
	ld c, (hl);							// m1 to C
	inc hl;								// next
	ld b, (hl);							// m2 to B
	ld (hl), a;							// sign tp HL
	inc hl;								// next
	ld a, c;							// m1 to A
	ld c, (hl);							// m3 to C
	push bc;							// stack m2 and m3
	inc hl;								// next
	ld c, (hl);							// m4 to C
	inc hl;								// next
	ld b, (hl);							// m5 to B
	ex de, hl;							// HL points to n1
	ld d, a;							// m1 to D
	ld e, (hl);							// n1 to E
	push de;							// stacl m1 and n1
	inc hl;								// next
	ld d, (hl);							// n2 to D
	inc hl;								// next
	ld e, (hl);							// n3 to E
	push de;							// stack n2 and n3
	exx;								// alternate register set
	pop de;								// n2 and n3 to DE'
	pop hl;								// m1 and n1 to HL'
	pop bc;								// m2 and m3 to BC'
	exx;								// main register set
	inc hl;								// next
	ld d, (hl);							// n4 to D
	inc hl;								// next
	ld e, (hl);							// n5 to E
	pop af;								// unstack AF
	pop hl;								// and HL
	ret;								// end of subroutine

;	// THE 'SHIFT ADDEND' SUBROUTINE

shift_fp:
	and a;								// no exponent difference?
	ret z;								// return if so
	cp 33;								// greater than 32?
	jr nc, addend_0;					// jump if so
	push bc;							// stack BC
	ld b, a;							// exponent difference to B

one_shift:
	exx;								// alternate register set
	sra l;								// shift right arithmetic L'
	rr d;								// rotate right
	rr e;								// with carry DE'
	exx;								// main register set
	rr d;								// rotate right
	rr e;								// with carry DE
	djnz one_shift;						// shift all five bytes right until done
	pop bc;								// unstack BC
	ret nc;								// return if no carry
	call add_back;						// get carry
	ret nz;								// return if nothing to add

addend_0:
	exx;								// alternate register set
	xor a;								// LD A, 0

zeros_4_5:
	ld l, 0;							// clear L'
	ld d, a;							// clear
	ld e, l;							// DE'
	exx;								// main register set
	ld de, 0;							// clear DE
	ret;								// end of subroutine

;	// THE 'ADD_BACK' SUBROUTINE

add_back:
	inc e;								// add carry to rightmost byte
	ret nz;								// return if no overflow
	inc d;								// next
	ret nz;								// return if no overflow
	exx;								// alternate register set
	inc e;								// next
	jr nz, all_added;					// jump if no overflow
	inc d;								// next

all_added:
	exx;								// main register set
	ret;								// end of subroutine

;	// THE 'SUBTRACTION' OPERATION

fp_subtract:
	ld a, (de);
	and a;
	ret z;
	inc de;
	ld a, (de);
	xor $80;
	ld (de), a;
	dec de;

;	// THE 'ADDITION' OPERATION

fp_addition:
	exx;								// alternate register set
	push hl;							// stack next literal address
	exx;								// main register set
	push de;							// stack pointer to addend
	push hl;							// and pointer to augend
	call prep_add;						// prepare augend
	ld b, a;							// exponent to B
	ex de, hl;							// swap pointers
	call prep_add;						// prepare addend
	ld c, a;							// exponent to C
	cp b;								// first exponent smaller?
	jr nc, shift_len;					// jump if so
	ld a, b;							// swap
	ld b, c;							// exponents
	ex de, hl;							// swap pointers

shift_len:
	push af;							// stack larger exponent
	sub b;								// get difference in B
	call fetch_two;						// get two numbers from calculator stack
	call shift_fp;						// shift addend right
	pop af;								// unstack larger exponent
	pop hl;								// unstack pointer to result
	ld (hl), a;							// store exponent
	push hl;							// stack pointer to result
	ld l, b;							// m4 and m5
	ld h, c;							// to HL
	add hl, de;							// add two right bytes
	exx;								// alternate register set
	ex de, hl;							// n2 and n3 to HL'
	adc hl, bc;							// add left bytes with carry
	ex de, hl;							// result to DE'
	ld a, h;							// add H'
	adc a, l;							// add L'
	ld l, a;							// result to L'
	rra;								// test for
	xor l;								// left overflow
	exx;								// main register set
	ex de, hl;							// result in DE and DE'
	pop hl;								// unstack pointer to exponent
	rra;								// test for shift
	jr nc, test_neg;					// jump if no carry
	ld a, 1;							// single right shift
	call shift_fp;						// do it
	inc (hl);							// exponent plus one
	jr z, add_rep_6;					// jump with overflow

test_neg:
	exx;								// swap register set
	ld a, l;							// sign bit to A
	and %10000000;						// set sign
	exx;								// alternate register set
	inc hl;								// point to second byte
	ld (hl), a;							// store sign
	dec hl;								// point to first byte
	jr z, go_nc_mlt;					// ump with zero else twos complement
	ld a, e;							// first byte to A
	neg;								// negate it
	ccf;								// complement carry flag
	ld e, a;							// store in E
	ld a, d;							// next byte to A
	cpl;								// ones complement
	adc a, 0;							// add carry
	ld d, a;							// store in D
	exx;								// swap register set
	ld a, e;							// next byte to A
	cpl;								// ones complement
	adc a, 0;							// add carry
	ld e, a;							// store in E
	ld a, d;							// next byte
	cpl;								// ones complement
	adc a, 0;							// add carry
	jr nc, end_compl;					// jump if no carry
	rra;								// lse 0.5 to mantissa, e = e + 1
	exx;								// alternate register set
	inc (hl);							// increase (HL)

add_rep_6:
	jp z, report_6;						// jump if overflow
	exx;								// swap register set
	
end_compl:
	ld d, a;							// last byte to D
	exx;								// swap register set

go_nc_mlt:
	xor a;								// LD A, 0
	jp test_norm;						// immediate jump

;	// THE 'PREPARE TO MULTIPLY OR DIVIDE' SUBROUTINE

prep_m_d:
	scf;
	dec (hl);
	inc (hl);
	ret z;
	inc hl;
	xor (hl);
	set 7, (hl);
	dec hl;
	ret;

;	// THE 'MULTIPLICATION' OPERATION

fp_multiply:
	xor a;								// LD A, 0
	call prep_m_d;						// prepare first number for multiplication
	ret c;								// return if zero
	exx;								// alternate register set
	push hl;							// stack next literal address
	exx;								// main register set
	push de;							// stack pointer to number to be multiplied
	ex de, hl;							// swap pointers
	call prep_m_d;						// prepare second number for multiplication
	ex de, hl;							// swap pointers
	jr c, zero_rslt;					// jump if second number is zero
	push hl;							// stack pointer to result
	call fetch_two;						// get two numbers from calculator stack
	ld a, b;							// m5 to A
	and a;								// prepare for subtraction
	sbc hl, hl;							// LD HL, 0
	exx;								// alternate register set
	push hl;							// stack m1 and n1
	sbc hl, hl;							// LD HL', 0
	exx;								// main register set
	ld b, 33;							// shift count
	jr strt_mlt;						// immediate jump

mlt_loop:
	jr nc, no_add;						// jump if no carry
	add hl, de;							// add number in DE to result
	exx;								// alternate register set
	adc hl, de;							// add number in DE' to result
	exx;								// main register set

no_add:
	exx;								// alternate register set
	rr h;								// shift HL'
	rr l;								// right
	exx;								// main register set
	rr h;								// shift HL
	rr l;								// right

strt_mlt:
	exx;								// alternate register set
	rr b;								// shift BC'
	rr c;								// right
	exx;								// main register set
	rr c;								// shift C right
	rra;								// shift A right with carry
	djnz mlt_loop;						// loop until done
	ex de, hl;							// HL to DE
	exx;								// alternate register set
	ex de, hl;							// HL' to DE'
	exx;								// main register set
	pop bc;								// unstack exponents
	pop hl;								// unstack pointer to exponent byte
	ld a, b;							// sum of exponents
	add a, c;							// to A
	jr nz, make_expt;					// jump if not zero
	and a;								// else clear carry flag

make_expt:
	dec a;								// prepare to increase
	ccf;								// exponent by 128

divn_expt:
	rla;								// rotate left with carry
	ccf;								// complement carry flag
	rra;								// rotate right with carry
	jp p, oflw1_clr;					// jump if sign flag cleared
	jr nc, report_6;					// jump if overflow
	and a;								// clear carry flag

oflw1_clr:
	inc a;								// increase A
	jr nz, oflw2_clr;					// jump if not zero
	jr c, oflw2_clr;					// jump if carry set
	exx;								// alternate register set
	bit 7, d;							// test bit 7 of D'
	exx;								// main register set
	jr nz, report_6;					// jump if overflow

oflw2_clr:
	ld (hl), a;							// set exponent byte
	exx;								// alternate register set
	ld a, b;							// LD A, B'
	exx;								// main register set

test_norm:
	jr nc, normalize;					// jump if no carry
	ld a, (hl);							// result to A
	and a;								// test for zero

near_zero:
	ld a, 128;							// or near zero
	jr z, skip_zero;					// jump with zero

zero_rslt:
	xor a;								// LD A, 0

skip_zero:
	exx;								// alternate register set
	and d;								// 2** - 128 if normal, else zero
	call zeros_4_5;						// set exponent to one
	rlca;								// unless zero
	ld (hl), a;							// restore exponent byte
	jr c, oflow_clr;					// jump if 2** - 128
	inc hl;								// second byte of result
	ld (hl), a;							// store zero
	dec hl;								// first byte of result
	jr oflow_clr;						// immediate jump

normalize:
	ld b, 32;							// left shift count

shift_one:
	exx;								// alternate register set
	bit 7, d;							// test bit 7 of D'
	exx;								// main register set
	jr nz, norml_now;					// jump if already normalized
	rlca;								// fifth byte to A
	rl e;								// rotate DE
	rl d;								// left
	exx;								// alternate register set
	rl e;								// rotate DE'
	rl d;								// left
	exx;								// main register set
	dec (hl);							// reduce exponent
	jr z, near_zero;					// jump if zero
	djnz shift_one;						// loop until done
	jr zero_rslt;						// immediate jump

norml_now:
	rla;								// rotate A left
	jr nc, oflow_clr;					// jump if no carry
	call add_back;						// else add back
	jr nz, oflow_clr;					// jump if no overflow
	exx;								// alternate register set
	ld d, 128;							// mantissa to 0.5
	exx;								// main register set
	inc (hl);							// increase result
	jr z, report_6;						// jump with overflow

oflow_clr:
	push hl;							// stack pointer
	inc hl;								// point to sign byte
	exx;								// alternate register set
	push de;							// stack DE'
	exx;								// main register set
	pop bc;								// DE' to BC
	ld a, b;							// sign to A
	rla;								// store sign
	rl (hl);							// in bit 7
	rra;								// of
	ld (hl), a;							// store first byte
	inc hl;								// next
	ld (hl), c;							// store second byte
	inc hl;								// next
	ld (hl), d;							// store third byte
	inc hl;								// next
	ld (hl), e;							// store fourth byte
	pop hl;								// unstack pointer to result
	pop de;								// unstack pointer to second number
	exx;								// alternate register set
	pop hl;								// next literal address to HL'
	exx;								// main register set
	ret;								// end of subroutine

report_6:
	rst error_1 ;
	defb arithmetic_overflow;

;	// THE 'DIVISION' OPERATION

fp_division:
	ex de, hl;							// swap pointers
	xor a;								// LD A, 0
	call prep_m_d;						// prepare number to divide by
	jr c, report_6;						// error if divide by zero
	ex de, hl;							// swap pointers
	call prep_m_d;						// prepare number to be divided
	ret c;								// return if already zero
	exx;								// alternate register set
	push hl;							// stack next literal address
	exx;								// main register set
	push de;							// stack pointer to number to divide by
	push hl;							// stack pointer to number to be divided
	call fetch_two;						// get two numbers from calculator stack
	exx;								// alternate register set
	push hl;							// stack m1 and n1
	ld l, c;							// to BC'
	ld h, b;							// number to be divided
	exx;								// main register set
	ld l, b;							// and
	ld h, c;							// BC
	xor a;								// LD A, 0
	ld b, $df;							// set count
	jr div_start;						// immediate jump

div_loop:
	rla;								// perform 32-bit shift into BC'CA taking
	rl c;								// one from carry, if set, at each shift
	exx;								// alternate register set
	rl c;								// rotate BC'
	rl b;								// left
	exx;								// main register set

div_34th:
	add hl, hl;							// move remains of dividend in HL'HL 
	exx;								// alternate register set
	adc hl, hl;							// and retrieve lost bit
	exx;								// main register set
	jr c, subn_only;					// jump if carry set

div_start:
	sbc hl, de;							// subtract divisor from dividend
	exx;								// alternate register set
	sbc hl, de;							// (32-bit arithmetic
	exx;								// main register set
	jr nc, no_rstore;					// jump if no carry
	add hl, de;							// restore HL
	exx;								// alternate register set
	adc hl, de;							// restore HL'
	exx;								// main register set
	and a;								// clear carry flag
	jr count_one;						// immediate jump

subn_only:
	and a;								// prepare for subtraction
	sbc hl, de;							// subtract divisor from dividend
	exx;								// alternate register set
	sbc hl, de;							// (32-bit arithmetic)
	exx;								// main register set

no_rstore:
	scf;								// set carry flag

count_one:
	inc b;								// increase count
	jp m, div_loop;						// loop until done
	push af;							// stack carry flag (33rd bit)
	jr z, div_34th;						// trial subtract for 34th bit if required
	ld e, a;							// transfer
	ld d, c;							// mantissa of result ...
	exx;								// alternate register set
	ld e, c;							// ... from BC'CA
	ld d, b;							// to DE'DE
	pop af;								// 34th bit
	rr b;								// to B'
	pop af;								// 33rd bit
	rr b;								// to B'
	exx;								// main register set
	pop bc;								// unstack exponent bytes
	pop hl;								// unstack pointer to result
	ld a, b;							// difference
	sub c;								// to A
	jp divn_expt;						// immediate jump


;	// THE 'INTEGER TRUNCATION TOWARDS ZERO' SUBROUTINE

fp_truncate:
	ld a, (hl);							// exponent to A
	cp 129;								// greater than 128?
	jr nc, x_large;						// jump if so
	ld (hl), 0;							// zero exponent
	ld a, 32;							// count
	jr nil_bytes;						// immediate jump

x_large:
	sub 160;							// subtract 160 from exponent
	ret p;								// return if positive
	neg;								// else negate

nil_bytes:
	push de;							// stack stack end
	ex de, hl;							// HL points to byte after fifth byte
	dec hl;								// point to fifth byte
	ld b, a;							// number of bits to zero to B
	srl b;								// divide by B
	srl b;								// to get
	srl b;								// byte count 
	jr z, bits_zero;					// jump if zero

byte_zero:
	ld (hl), 0;							// zero byte
	dec hl;								// next location
	djnz byte_zero;						// loop until done

bits_zero:
	and %00000111;						// A mod 8 = number of bits to zero
	jr z, ix_end;						// jump if zero
	ld b, a;							// count to B
	ld a, 255;							// set mask

less_mask:
	sla a;								// shift left and insert zero in bit 0
	djnz less_mask;						// loop until done
	and (hl);							// discard unwanted bits
	ld (hl), a;							// store value in (HL)

ix_end:
	ex de, hl;							// swap pointers
	pop de;								// stack end to DE
	ret;								// end of subroutine
