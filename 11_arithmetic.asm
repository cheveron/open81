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
	ld c, a;
	ld b, 0;

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
	call fp_to_bc;
	ret c;
	push af;
	dec b;
	inc b;
	jr z, fp_a_end;
	pop af;
	scf;
	ret;

fp_a_end:
	pop af;
	ret;

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
	ld a, (hl);
	ld (hl), 0;
	and a;
	ret z;
	inc hl;
	bit 7, (hl);
	set 7, (hl);
	dec hl;
	ret z;
	push bc;
	ld bc, $0005;
	add hl, bc;
	ld b, c;
	ld c, a;
	scf;

neg_byte:
	dec hl;
	ld a, (hl);
	cpl;
	adc a, 0;
	ld (hl), a;
	djnz neg_byte;
	ld a, c;
	pop bc;
	ret;

;	// THE 'FETCH TWO NUMBERS' SUBROUTINE

fetch_two:
	push hl;
	push af;
	ld c, (hl);
	inc hl;
	ld b, (hl);
	ld (hl), a;
	inc hl;
	ld a, c;
	ld c, (hl);
	push bc;
	inc hl;
	ld c, (hl);
	inc hl;
	ld b, (hl);
	ex de, hl;
	ld d, a;
	ld e, (hl);
	push de;
	inc hl;
	ld d, (hl);
	inc hl;
	ld e, (hl);
	push de;
	exx;
	pop de;
	pop hl;
	pop bc;
	exx;
	inc hl;
	ld d, (hl);
	inc hl;
	ld e, (hl);
	pop af;
	pop hl;
	ret;

;	// THE 'SHIFT ADDEND' SUBROUTINE

shift_fp:
	and a;
	ret z;
	cp 33;
	jr nc, addend_0;
	push bc;
	ld b, a;

one_shift:
	exx;
	sra l;
	rr d;
	rr e;
	exx;
	rr d;
	rr e;
	djnz one_shift;
	pop bc;
	ret nc;
	call add_back;
	ret nz;

addend_0:
	exx;
	xor a;

zeros_4_5:
	ld l, 0;
	ld d, a;
	ld e, l;
	exx;
	ld de, $0000;
	ret;

;	// THE 'ADD_BACK' SUBROUTINE

add_back:
	inc e;
	ret nz;
	inc d;
	ret nz;
	exx;
	inc e;
	jr nz, all_added;
	inc d;

all_added:
	exx;
	ret;

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
	exx;
	push hl;
	exx;
	push de;
	push hl;
	call prep_add;
	ld b, a;
	ex de, hl;
	call prep_add;
	ld c, a;
	cp b;
	jr nc, shift_len;
	ld a, b;
	ld b, c;
	ex de, hl;

shift_len:
	push af;
	sub b;
	call fetch_two;
	call shift_fp;
	pop af;
	pop hl;
	ld (hl), a;
	push hl;
	ld l, b;
	ld h, c;
	add hl, de;
	exx;
	ex de, hl;
	adc hl, bc;
	ex de, hl;
	ld a, h;
	adc a, l;
	ld l, a;
	rra ;
	xor l;
	exx;
	ex de, hl;
	pop hl;
	rra ;
	jr nc, test_neg;
	ld a, 1;
	call shift_fp;
	inc (hl);
	jr z, add_rep_6;

test_neg:
	exx;
	ld a, l;
	and %10000000;
	exx;
	inc hl;
	ld (hl), a;
	dec hl;
	jr z, go_nc_mlt;
	ld a, e;
	neg;
	ccf;
	ld e, a;
	ld a, d;
	cpl ;
	adc a, 0;
	ld d, a;
	exx;
	ld a, e;
	cpl ;
	adc a, 0;
	ld e, a;
	ld a, d;
	cpl ;
	adc a, 0;
	jr nc, end_compl;
	rra ;
	exx;
	inc (hl);

add_rep_6:
	jp z, report_6;
	exx;
	
end_compl:
	ld d, a;
	exx;

go_nc_mlt:
	xor a;
	jr test_norm;

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
	xor a;
	call prep_m_d;
	ret c;
	exx;
	push hl;
	exx;
	push de;
	ex de, hl;
	call prep_m_d;
	ex de, hl;
	jr c, zero_rslt;
	push hl;
	call fetch_two;
	ld a, b;
	and a;
	sbc hl, hl;
	exx;
	push hl;
	sbc hl, hl;
	exx;
	ld b, 33;
	jr strt_mlt;

mlt_loop:
	jr nc, no_add;
	add hl, de;
	exx;
	adc hl, de;
	exx;

no_add:
	exx;
	rr h;
	rr l;
	exx;
	rr h;
	rr l;

strt_mlt:
	exx;
	rr b;
	rr c;
	exx;
	rr c;
	rra;
	djnz mlt_loop;
	ex de, hl;
	exx;
	ex de, hl;
	exx;
	pop bc;
	pop hl;
	ld a, b;
	add a, c;
	jr nz, make_expt;
	and a;

make_expt:
	dec a;
	ccf;

divn_expt:
	rla ;
	ccf;
	rra ;
	jp p, oflw1_clr;
	jr nc, report_6;
	and a;

oflw1_clr:
	inc a;
	jr nz, oflw2_clr;
	jr c, oflw2_clr;
	exx;
	bit 7, d;
	exx;
	jr nz, report_6;

oflw2_clr:
	ld (hl), a;
	exx;
	ld a, b;
	exx;

test_norm:
	jr nc, normalize;
	ld a, (hl);
	and a;

near_zero:
	ld a, 128;
	jr z, skip_zero;

zero_rslt:
	xor a;

skip_zero:
	exx;
	and d;
	call zeros_4_5;
	rlca;
	ld (hl), a;
	jr c, oflow_clr;
	inc hl;
	ld (hl), a;
	dec hl;
	jr oflow_clr;

normalize:
	ld b, 32;

shift_one:
	exx;
	bit 7, d;
	exx;
	jr nz, norml_now;
	rlca;
	rl e;
	rl d;
	exx;
	rl e;
	rl d;
	exx;
	dec (hl);
	jr z, near_zero;
	djnz shift_one;
	jr zero_rslt;

norml_now:
	rla ;
	jr nc, oflow_clr;
	call add_back;
	jr nz, oflow_clr;
	exx;
	ld d, 128;
	exx;
	inc (hl);
	jr z, report_6;

oflow_clr:
	push hl;
	inc hl;
	exx;
	push de;
	exx;
	pop bc;
	ld a, b;
	rla;
	rl (hl);
	rra;
	ld (hl), a;
	inc hl;
	ld (hl), c;
	inc hl;
	ld (hl), d;
	inc hl;
	ld (hl), e;
	pop hl;
	pop de;
	exx;
	pop hl;
	exx;
	ret;

report_6:
	rst error_1 ;
	defb arithmetic_overflow;

;	// THE 'DIVISION' OPERATION

fp_division:
	ex de, hl;
	xor a;
	call prep_m_d;
	jr c, report_6;
	ex de, hl;
	call prep_m_d;
	ret c;
	exx;
	push hl;
	exx;
	push de;
	push hl;
	call fetch_two;
	exx;
	push hl;
	ld h, b;
	ld l, c;
	exx;
	ld h, c;
	ld l, b;
	xor a;
	ld b, $df;
	jr div_start;

div_loop:
	rla ;
	rl c;
	exx;
	rl c;
	rl b;
	exx;
	add hl, hl;
	exx;
	adc hl, hl;
	exx;
	jr c, subn_only;

div_start:
	sbc hl, de;
	exx;
	sbc hl, de;
	exx;
	jr nc, no_rstore;
	add hl, de;
	exx;
	adc hl, de;
	exx;
	and a;
	jr count_one;

subn_only:
	and a;
	sbc hl, de;
	exx;
	sbc hl, de;
	exx;

no_rstore:
	scf;

count_one:
	inc b;
	jp m, div_loop;
	push af;
	jr z, div_start;
	ld e, a;
	ld d, c;
	exx;
	ld e, c;
	ld d, b;
	pop af;
	rr b;
	pop af;
	rr b;
	exx;
	pop bc;
	pop hl;
	ld a, b;
	sub c;
	jp divn_expt;

;	// THE 'INTEGER TRUNCATION TOWARDS ZERO' SUBROUTINE

fp_truncate:
	ld a, (hl);
	cp 129;
	jr nc, x_large;
	ld (hl), 0;
	ld a, 32;
	jr nil_bytes;

x_large:
	sub 160;
	ret p;
	neg;

nil_bytes:
	push de;
	ex de, hl;
	dec hl;
	ld b, a;
	srl b;
	srl b;
	srl b;
	jr z, bits_zero;

byte_zero:
	ld (hl), 0;
	dec hl;
	djnz byte_zero;

bits_zero:
	and %00000111;
	jr z, ix_end;
	ld b, a;
	ld a, 255;

less_mask:
	sla a;
	djnz less_mask;
	and (hl);
	ld (hl), a;

ix_end:
	ex de, hl;
	pop de;
	ret;
