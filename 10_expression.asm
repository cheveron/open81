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

; THE 'SCANNING' SUBROUTINE

scanning:
	rst  get_ch;
	ld b, $00;
	push bc;

s_rnd:
	cp $40;
	jr nz, s_pi;
	call syntax_z;
	jr z, s_rnd_end;
	ld bc, (seed);
	call stack_bc;
	rst fp_calc;
	defb stk_one;
	defb addition;
	defb stk_data;
	defb $37;
	defb $16;
	defb multiply;
	defb stk_data;
	defb $80;
	defb $41;
	defb $00, $00, $80;
	defb n_mod_m;
	defb delete;
	defb stk_one;
	defb subtract;
	defb duplicate;
	defb end_calc;
	call fp_to_bc;
	ld (seed), bc;
	ld a, (hl);
	and a;
	jr z, s_rnd_end;
	sub 16;
	ld (hl), a;

s_rnd_end:
	jr s_pi_end;
	
s_pi:
	cp $42;
	jr nz, s_inkeys;
	call syntax_z;
	jr z, s_pi_end;
	rst fp_calc;
	defb stk_pi_div_2;
	defb end_calc;
	inc (hl);

s_pi_end:
	rst next_ch;
	jp s_numeric;

s_inkeys:
	cp tk_inkeys;
	jr nz, s_alphnum;
	call keyboard;
	ld b, h;
	ld c, l;
	ld d, c;
	inc d;
	call nz, decode;
	ld a, d;
	adc a, d;
	ld b, d;
	ld c, a;
	ex de, hl;
	jr s_string;

s_alphnum:
	call alphanum;
	jr c, s_let_num;
	cp period;
	jp z, s_decimal;
	ld bc, $09d8;
	cp minus;
	jr z, s_push_p0;
	cp open_bracket;
	jr nz, s_quote;
	call ch_add_inc;
	call scanning;
	cp close_bracket;
	jr nz, s_rprt_c1;
	call ch_add_inc;
	jr s_cont_1;

s_quote:
	cp double_quote;
	jr nz, s_funct;
	call ch_add_inc;
	push hl;
	jr s_q_end;

s_q_next;
	call ch_add_inc;

s_q_end:
	cp $0b;
	jr nz, s_n_lerr;
	pop de;
	and a;
	sbc hl, de;
	ld b, h;
	ld c, l;

s_string:
	ld hl, flags;
	res 6, (hl);
	bit 7, (hl);
	call nz, stk_store;
	rst next_ch;

s_cont_1:
	jp s_cont_3;

s_n_lerr:
	cp ctrl_newline;
	jr nz, s_q_next;

s_rprt_c1:
	jp report_c;

s_funct:
	sub $c4;
	jr c, s_rprt_c1;
	ld bc, $04ec;
	cp $13;
	jr z, s_push_p0;
	jr nc, s_rprt_c1;
	ld b, $10;
	add a, $d9;
	ld c, a;
	cp $dc;
	jr nc, s_no_to_s;
	res 6, c;

s_no_to_s:
	cp $ea;
	jr c, s_push_p0;
	res 7, c;

s_push_p0:
	push bc;
	rst next_ch;
	jp s_rnd;

s_let_num:
	cp $26;
	jr c, s_decimal;
	call look_vars;
	jp c, report_2;
	call z, stk_var;
	ld a, (flags);
	cp $c0;
	jr c, s_cont_2;
	inc hl;
	ld de, (stkend);
	call fp_move_fp;
	ex de, hl;
	ld (stkend), hl;
	jr s_cont_2;

s_decimal:
	call syntax_z;
	jr nz, s_stk_dec;
	call dec_to_fp;
	rst get_ch;
	ld bc, $0006;
	call make_room;
	inc hl;
	ld (hl), ctrl_number;
	inc hl;
	ex de, hl;
	ld hl, (stkend);
	ld c, 5;
	and a;
	sbc hl, bc;
	ld (stkend), hl;
	ldir;
	ex de, hl;
	dec hl;
	call cursor_so;
	jr s_numeric;

s_stk_dec:
	rst next_ch;
	cp ctrl_number;
	jr nz, s_stk_dec;
	inc hl;
	ld de, (stkend);
	call fp_move_fp;
	ld (stkend), de;
	ld (ch_add), hl;

s_numeric:
	set 6, (iy + _flags)

s_cont_2:
	rst get_ch;

s_cont_3:
	cp open_bracket;
	jr nz, s_opertr;
	bit 6, (iy + _flags);
	jr nz, s_loop;
	call slicing;
	rst next_ch;
	jr s_cont_3;

s_opertr:
	ld bc, $00c3;
	cp close_angle;
	jr c, s_loop;
	sub $16;
	jr nc, s_high_op;
	add a, $0d;
	jr s_end_op;

s_high_op:
	cp $03;
	jr c, s_end_op;
	sub $c2;
	jr c, s_loop;
	cp $06;
	jr nc, s_loop;
	add a, $03;

s_end_op:
	add a, c;
	ld c, a;
	ld hl, $104c;
	add hl, bc;
	ld b, (hl);

s_loop:
	pop de;
	ld a, d;
	cp b;
	jr c, s_tighter;
	and a;
	jp z, jp_get_ch;
	push bc;
	push de;
	call syntax_z;
	jr z, s_syntest;
	ld a, e;
	and $3f;
	ld b, a;
	rst fp_calc;
	defb calc_2;
	defb end_calc;
	jr s_runtest;

s_syntest:
	ld a, e;
	xor (iy + $01) ; flags;
	and %01000000;

s_rport_c2:
	jp nz, report_c;

s_runtest:
	pop de;
	ld hl, flags;
	set 6, (hl);
	bit 7, e;
	jr nz, s_endloop;
	res 6, (hl);

s_endloop:
	pop bc;
	jr s_loop;

s_tighter:
	push de;
	ld a, c;
	bit 6, (iy + _flags);
	jr nz, s_next;
	and %00111111;
	add a, 8;
	ld c, a;
	cp str_and_no;
	jr nz, s_not_and;
	set 6, c;
	jr s_next;

s_not_and:
	jr c, s_rport_c2;
	cp strs_add;
	jr z, s_next;
	set 7, c;

s_next:
	push bc;
	rst next_ch;
	jp s_rnd;

;	// THE PRIORITY TABLE

pri_tbl:
	defb $06;	// -
	defb $08;	// *
	defb $08;	// /
	defb $0a;	// **
	defb $02;	// OR
	defb $03;	// AND
	defb $05;	// <=
	defb $05;	// >=
	defb $05;	// <>
	defb $05;	// >
	defb $05;	// <
	defb $05;	// =
	defb $06;	// +

;	// THE 'LOOK-VARS' SUBROUTINE

look_vars:
	set 6, (iy + _flags);
	rst get_ch;
	call alpha;
	jp nc, report_c;
	push hl;
	ld c, a;
	rst next_ch;
	push hl;
	res 5, c;
	cp open_bracket;
	jr z, v_run_syn;
	set 6, c;
	cp dollar;
	jr z, v_str_var;
	set 5, c;

v_char:
	call alphanum;
	jr nc, v_run_syn;
	res 6, c;
	rst next_ch;
	jr v_char;

v_str_var:
	rst next_ch;
	res 6, (iy + _flags);

v_run_syn:
	ld b, c;
	call syntax_z;
	jr nz, v_run;
	ld a, c;
	and $e0;
	set 7, a;
	ld c, a;
	jr v_syntax;

v_run:
	ld hl, (vars);
	
v_each:
	ld a, (hl);
	and $7f;
	jr z, v_80_byte;
	cp c;
	jr nz, v_next;
	rla;
	add a, a;
	jp p, v_found_2;
	jr c, v_found_2;
	pop de;
	push de;
	push hl;

v_matches:
	inc hl;
	
v_spaces:
	ld a, (de);
	inc de;
	and a;	// test space;
	jr z, v_spaces;
	cp (hl);
	jr z, v_matches;
	or %10000000;
	cp (hl);
	jr nz, v_get_ptr;
	ld a, (de);
	call alphanum;
	jr nc, v_found_1;

v_get_ptr:
	pop hl

v_next:
	push bc;
	call next_one;
	ex de, hl;
	pop bc;
	jr v_each;


v_80_byte:
	set 7, b;

v_syntax:
	pop de;
	rst get_ch;
	cp open_bracket;
	jr z, v_pass;
	set 5, b;
	jr v_end;

v_found_1:
	pop de;

v_found_2:
	pop de;
	pop de;
	push hl;
	rst get_ch;
	
v_pass:
	call alphanum;
	jr nc, v_end;
	rst next_ch;
	jr v_pass;

v_end:
	pop hl;
	rl b;
	bit 6, b;
	ret;

;	// THE 'STK-VAR' SUBROUTINE

stk_var:
	xor a;
	ld b, a;
	bit 7, c;
	jr nz, sv_count;
	bit 7, (hl);
	jr nz, sv_arrays;
	inc a;

sv_simpleS:
	inc hl;
	ld c, (hl);
	inc hl;
	ld b, (hl);
	inc hl;
	ex de, hl;
	call stk_store;
	rst get_ch;
	jp sv_slice_query;

sv_arrays:
	inc hl;
	inc hl;
	inc hl;
	ld b, (hl);
	bit 6, c;
	jr z, sv_ptr;
	dec b;
	jr z, sv_simpleS;
	ex de, hl;
	rst get_ch;
	cp open_bracket;
	jr nz, report_3;
	ex de, hl;

sv_ptr:
	ex de, hl;
	jr sv_count;

sv_comma:
	push hl;
	rst get_ch;
	pop hl;
	cp comma;
	jr z, sv_loop;
	bit 7, c;
	jr z, report_3;
	bit 6, c;
	jr nz, sv_close;
	cp close_bracket;
	jr nz, sv_rpt_c;
	rst next_ch;
	ret;

sv_close:
	cp close_bracket;
	jr z, sv_dim;
	cp tk_to;
	jr nz, sv_rpt_c;

sv_ch_add:
	rst get_ch;
	dec hl;
	ld (ch_add), hl;
	jr sv_slice;

sv_count:
	ld hl, $0000;

sv_loop:
	push hl;
	rst next_ch;
	pop hl ;
	ld a, c;
	cp %11000000;
	jr nz, sv_mult;
	rst get_ch;
	cp close_bracket;
	jr z, sv_dim;
	cp tk_to;
	jr z, sv_ch_add;

sv_mult:
	push bc;
	push hl;
	call de_inc_to_de;
	ex (sp), hl;
	ex de, hl;
	call int_exp1;
	jr c, report_3;
	dec bc;
	call hl_hl_x_de;
	add hl, bc;
	pop de;
	pop bc;
	djnz sv_comma;
	bit 7, c;

sv_rpt_c:
	jr nz, sl_rpt_c;
	push hl;
	bit 6, c;
	jr nz, sv_elemS;
	ld b, d;
	ld c, e;
	rst get_ch;
	cp close_bracket;
	jr z, sv_number;

report_3:
	rst error_1;
	defb subscript_out_of_range;

sv_number:
	rst next_ch;
	pop hl;
	ld de, $0005;
	call hl_hl_x_de;
	add hl, bc;
	ret;

sv_elemS:
	call de_inc_to_de;
	ex (sp), hl;
	call hl_hl_x_de;
	pop bc;
	add hl, bc;
	inc hl;
	ld b, d;
	ld c, e;
	ex de, hl;
	call stk_st_0;
	rst get_ch;
	cp close_bracket;
	jr z, sv_dim;
	cp comma;
	jr nz, report_3;

sv_slice:
	call slicing;

sv_dim:
	rst next_ch;

sv_slice_query:
	cp open_bracket;
	jr z, sv_slice;
	res 6, (iy + _flags);
	ret;

;	// THE 'SLICING' SUBROUTINE

slicing:
	call syntax_z;
	call nz, stk_fetch;
	rst next_ch;
	cp close_bracket;
	jr z, sl_store;
	push de;
	xor a;
	push af;
	push bc;
	ld de, $0001;
	rst get_ch;
	pop hl;
	cp tk_to;
	jr z, sl_second;
	pop af;
	call int_exp2;
	push af;
	ld d, b;
	ld e, c;
	push hl;
	rst get_ch;
	pop hl;
	cp tk_to;
	jr z, sl_second;
	cp close_bracket;

sl_rpt_c:
	jp nz, report_c;
	ld h, d;
	ld l, e;
	jr sl_define;

sl_second:
	push hl;
	rst next_ch;
	pop hl;
	cp close_bracket;
	jr z, sl_define;
	pop af;
	call int_exp2;
	push af;
	rst get_ch;
	ld h, b;
	ld l, c;
	cp close_bracket;
	jr nz, sl_rpt_c;

;	// THE 'NEW' PARAMETERS ARE NOW DEFINED

sl_define:
	pop af;
	ex (sp), hl;
	add hl, de;
	dec hl;
	ex (sp), hl;
	and a;
	sbc hl, de;
	ld bc, $0000;
	jr c, sl_over;
	inc hl;
	and a;
	jp m, report_3;
	ld b, h;
	ld c, l;

sl_over:
	pop de;
	res 6, (iy + _flags);

sl_store:
	call syntax_z;
	ret z;

;	// THE 'STKSTORE' SUBROUTINE

stk_st_0:
	xor a;

stk_store:
	push bc;
	call test_5_sp;
	pop bc;
	ld hl, (stkend);
	ld (hl), a;
	inc hl;
	ld (hl), e;
	inc hl;
	ld (hl), d;
	inc hl;
	ld (hl), c;
	inc hl;
	ld (hl), b;
	inc hl;
	ld (stkend), hl;
	res 6, (iy + _flags);
	ret;

;	// THE 'INT:EXP' SUBROUTINE

int_exp1:
	xor a;
	
int_exp2:
	push de;
	push hl;
	push af;
	call class_6;
	pop af;
	call syntax_z;
	jr z, i_restore;
	push af;
	call find_int;
	pop de;
	ld a, b;
	or c;
	scf;
	jr z, i_carry;
	pop hl;
	push hl;
	and a;
	sbc hl, bc;

i_carry:
	ld a, d;
	sbc a, 0;

i_restore:
	pop hl;
	pop de;
	ret;

;	// THE 'DE, (DE+1)' SUBROUTINE

de_inc_to_de:
	ex de, hl;
	inc hl;
	ld e, (hl);
	inc hl;
	ld d, (hl);
	ret;

;	// THE 'HL=HL*DE' SUBROUTINE

hl_hl_x_de:
	call syntax_z;
	ret z;
	push bc;
	ld b, 16;
	ld a, h;
	ld c, l;
	ld hl, $0000;

hl_loop:
	add hl, hl;
	jr c, hl_over;
	rl c;
	rla;
	jr nc, hl_again;
	add hl, de;

hl_over:
	jp c, report_4;

hl_again:
	djnz hl_loop;
	pop bc;
	ret;

;	// THE 'LET' COMMAND ROUTINE

let:
	ld hl, (dest);
	bit 1, (iy + _flagx);
	jr z, l_exists;
	ld bc, $0005;

l_each_ch:
	inc bc;

l_no_sp:
	inc hl;
	ld a, (hl);
	and a ; test space;
	jr z, l_no_sp;
	call alphanum;
	jr c, l_each_ch;
	cp dollar;
	jp z, l_news;
	rst bc_spaces;
	push de;
	ld hl, (dest);
	dec de;
	ld a, c;
	sub 6;
	ld b, a;
	ld a, $40;
	jr z, l_single;

l_char:
	inc hl;
	ld a, (hl);
	and a ; test space;
	jr z, l_char;
	inc de;
	ld (de), a;
	djnz l_char;
	or %10000000;
	ld (de), a;
	ld a, %10000000;

l_single:
	ld hl, (dest);
	xor (hl);
	pop hl;
	call l_clear;

l_numeric:
	push hl;
	rst fp_calc;
	defb delete;
	defb end_calc;
	pop hl;
	ld bc, $0005;
	and a;
	sbc hl, bc;
	jr l_enter;

l_exists:
	bit 6, (iy + _flags);
	jr z, l_deletes;
	ld de, $0006;
	add hl, de;
	jr l_numeric;

l_deletes:
	ld hl, (dest);
	ld bc, (strlen);
	bit 0, (iy + _flagx);
	jr nz, l_adds;
	ld a, b;
	or c;
	ret z;
	push hl;
	rst bc_spaces;
	push de;
	push bc;
	ld d, h;
	ld e, l;
	inc hl;
	ld (hl), $00;	// space;
	lddr;
	push hl;
	call stk_fetch;
	pop hl;
	ex (sp), hl;
	and a;
	sbc hl, bc;
	add hl, bc;
	jr nc, l_length;
	ld b, h;
	ld c, l;

l_length:
	ex (sp), hl;
	ex de, hl;
	ld a, b;
	or c;
	jr z, l_in_w_s;
	ldir;

l_in_w_s:
	pop bc;
	pop de;
	pop hl;

l_enter:
	ex de, hl;

l_enter_1:
	ld a, b;
	or c;
	ret z;
	push de;
	ldir;
	pop hl;
	ret;

l_adds:
	dec hl;
	dec hl;
	dec hl;
	ld a, (hl);
	push hl;
	push bc;
	call l_string;
	pop bc;
	pop hl;
	inc bc;
	inc bc;
	inc bc;
	jp reclaim_2;

l_news:
	ld a, $60;
	ld hl, (dest);
	xor (hl);

l_string:
	push af;
	call stk_fetch;
	ex de, hl;
	add hl, bc;
	push hl;
	inc bc;
	inc bc;
	inc bc;
	rst bc_spaces;
	ex de, hl;
	pop hl;
	dec bc;
	dec bc;
	push bc;
	lddr;
	ex de, hl;
	pop bc;
	dec bc;
	ld (hl), b;
	dec hl;
	ld (hl), c;
	pop af;

l_clear:
	push af;
	call reclaim_3;
	pop af;
	dec hl;
	ld (hl), a;
	ld hl, (stkbot);
	ld (e_line), hl;
	dec hl;
	ld (hl), $80;
	ret;

;	// THE 'STK_FETCH' SUBROUTINE

stk_fetch:
	ld hl, (stkend);
	dec hl;
	ld b, (hl);
	dec hl;
	ld c, (hl);
	dec hl;
	ld d, (hl);
	dec hl;
	ld e, (hl);
	dec hl;
	ld a, (hl);
	ld (stkend), hl;
	ret;

;	// THE 'DIM' COMMAND ROUTINE

dim:
	call look_vars;

d_rport_c:
	jp nz, report_c;
	call syntax_z;
	jr nz, d_run;
	res 6, c;
	call stk_var;
	call check_end;

d_run:
	jr c, d_letter;
	push bc;
	call next_one;
	call reclaim_2;
	pop bc;

d_letter:
	set 7, c;
	ld b, 0;
	push bc;
	ld hl, $0001;
	bit 6, c;
	jr nz, d_size;
	ld l, 5;

d_size:
	ex de, hl;

d_no_loop:
	rst next_ch;
	ld h, $40;
	call int_exp1;
	jp c, report_3;
	pop hl;
	push bc;
	inc h;
	push hl;
	ld h, b;
	ld l, c;
	call hl_hl_x_de;
	ex de, hl;
	rst get_ch;
	cp comma;
	jr z, d_no_loop;
	cp close_bracket;
	jr nz, d_rport_c;
	rst next_ch;
	pop bc;
	ld a, c;
	ld l, b;
	ld h, 0;
	inc hl;
	inc hl;
	add hl, hl;
	add hl, de;
	jp c, report_4;
	push de;
	push bc;
	push hl;
	ld b, h;
	ld c, l;
	ld hl, (e_line);
	dec hl;
	call make_room;
	inc hl;
	ld (hl), a;
	pop bc;
	dec bc;
	dec bc;
	dec bc;
	inc hl;
	ld (hl), c;
	inc hl;
	ld (hl), b;
	pop af;
	inc hl;
	ld (hl), a ;
	ld h, d;
	ld l, e;
	dec de;
	ld (hl), 0;
	pop bc;
	lddr;

;	// THE 'DIMENSION_SIZES' ARE NOW ENTERED

dim_sizes:
	pop bc;
	ld (hl), b;
	dec hl;
	ld (hl), c;
	dec hl;
	dec a;
	jr nz, dim_sizes;
	ret;

;	// THE 'RESERVE' SUBROUTINE

reserve:
	ld hl, (stkbot);
	dec hl;
	call make_room;
	inc hl;
	inc hl;
	pop bc;
	ld (e_line), bc;
	pop bc;
	ex de, hl;
	inc hl;
	ret;

;	// THE 'CLEAR' COMMAND ROUTINE

clear:
	ld hl, (vars);
	ld (hl), $80;
	inc hl;
	ld (e_line), hl;

;	// THE 'X-TEMP' SUBROUTINE

x_temp:
	ld hl, (e_line);

;	// THE 'SETSTK-B' SUBROUTINE

set_stk_b:
	ld (stkbot), hl;

set_stk_e:
	ld (stkend), hl;
	ret;
	
;	// THE 'CURSOR-IN' SUBROUTINE

cursor_in:
	ld hl, (e_line);
	ld (hl), $7f;
	inc hl;
	ld (hl), $76;
	inc hl;
	ld (iy + _df_size), $02;
	jr set_stk_b;

;	// THE 'SET-MEM' SUBROUTINE

set_mem:
	ld hl, $405d ; membot;
	ld (mem), hl;
	ld hl, (stkbot);
	jr set_stk_e;

;	// THE 'RECLAIM-3' SUBROUTINE

reclaim_3:
	ld de, (e_line);
	jp reclaim_1;

;	// THE 'ALPHA' SUBROUTINE

alpha:
	cp capital_a;
	jr alpha_2;

;	// THE 'ALPHANUM' SUBROUTINE

alphanum:
	cp zero;

alpha_2:
	ccf;
	ret nc;
	cp $40 ; test 'Z' + 1;
	ret;

;	// THE 'DECIMAL TO FLOATING_POINT' SUBROUTINE

dec_to_fp:
	call int_to_fp;
	cp period;
	jr nz, e_format;
	rst fp_calc;
	defb stk_one;
	defb st_mem_0;
	defb delete;
	defb end_calc;

nxt_dgt_1:
	rst next_ch;
	call stk_digit;
	jr c, e_format;
	rst fp_calc;
	defb get_mem_0;
	defb stk_ten;
	defb multiply;
	defb st_mem_0;
	defb division;
	defb addition;
	defb end_calc;
	jr nxt_dgt_1;
