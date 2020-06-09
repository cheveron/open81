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

;	// THE 'KEYBOARD DECODE' SUBROUTINE

decode:
	ld d, $00;
	sra b;
	sbc a, a;
	or $26;
	ld l, $05;
	sub l;

key_line:
	add a, l;
	scf;
	rr c;
	jr c, key_line;
	inc c;
	ret nz;
	ld c, b;
	dec l;
	ld l, $01;
	jr nz, key_line;
	ld hl, $007d;
	ld e, a;
	add hl, de;
	scf;
	ret;

;	// THE 'PRINTING' SUBROUTINE

lead_sp:
	ld a, e;
	and a;
	ret m;
	jr print_ch;

out_digit:
	xor a;

digit_inc:
	add hl, bc;
	inc a;
	jr c, digit_inc;
	sbc hl, bc;
	dec a;
	jr z, lead_sp;

out_code:
	ld e, zero;
	add a, e;

out_ch:
	and a;
	jr z, print_sp;

print_ch:
	res 0, (iy + _flags);

print_sp:
	exx;
	push hl;
	bit 1, (iy + _flags);
	jr nz, lprint_a;
	call enter_ch;
	jr print_exx;

lprint_a:
	call lprint_ch;

print_exx:
	pop hl;
	exx;
	ret;

enter_ch:
	ld d, a;
	ld bc, (s_posn);
	ld a, c;
	cp $21;
	jr z, test_low;

test_n_l:
	ld a, $76;
	cp d;
	jr z, write_n_l;
	ld hl, (df_cc);
	cp (hl);
	ld a, d;
	jr nz, write_ch;
	dec c;
	jr nz, expand_1;
	inc hl;
	ld (df_cc), hl;
	ld c, $21;
	dec b;
	ld (s_posn), bc;

test_low:
	ld a, b;
	cp (iy + _df_size);
	jr z, report_5;
	and a;
	jr nz, test_n_l;

report_5:
	ld l, insufficient_room;
	jp error_3;

expand_1:
	call one_space;
	ex de, hl;

write_ch:
	ld (hl), a;
	inc hl;
	ld (df_cc), hl;
	dec (iy + _s_posn_lo);
	ret;

write_n_l:
	ld c, $21;
	dec b;
	set 0, (iy + _flags);
	jp loc_addr;

;	// THE 'LPRINT-CH' SUBROUTINE

lprint_ch:
	cp $76;
	jr z, copy_buff;
	ld c, a;
	ld a, (pr_cc);
	and $7f;
	cp $5c;
	ld l, a;
	ld h, $40;
	call z, copy_buff;
	ld (hl), c;
	inc l;
	ld (iy + _pr_cc), l;
	ret;
	
;	// THE 'COPY' COMMAND ROUTINE

copy:
	ld d, $16;
	ld hl, (d_file);
	inc hl;
	jr copy_x_d;

copy_buff:
	ld d, $01;
	ld hl, prbuff;

copy_x_d:
	call set_fast;
	push bc;

copy_loop:
	push hl;
	xor a;
	ld e, a;

copy_time:
	out ($fb), a;
	pop hl;

copy_brk:
	call break_1;
	jr c, copy_cont;
	rra;
	out ($fb), a;

report_d2:
	rst error_1;
	defb $0c;

copy_cont:
	in a, ($fb);
	add a, a;
	jp m, copy_end;
	jr nc, copy_brk;
	push hl;
	push de;
	ld a, d;
	cp $02;
	sbc a, a;
	and e;
	rlca;
	and e;
	ld d, a;

copy_next:
	ld c, (hl);
	ld a, c;
	inc hl;
	cp $76;
	jr z, copy_n_l;
	push hl;
	sla a;
	add a, a;
	add a, a;
	ld h, $0f;
	rl h;
	add a, e;
	ld l, a;
	rl c;
	sbc a, a;
	xor (hl);
	ld c, a;
	ld b, $08;

copy_bits:
	ld a, d;
	rlc c;
	rra;
	ld h, a;

copy_wait:
	in a, ($fb);
	rra;
	jr nc, copy_wait;
	ld a, h;
	out ($fb), a;
	djnz copy_bits;
	pop hl;
	jr copy_next;

copy_n_l:
	in a, ($fb);
	rra;
	jr nc, copy_n_l;
	ld a, d;
	rrca;
	out ($fb), a;
	pop de;
	inc e;
	bit 3, e;
	jr z, copy_time;
	pop bc;
	dec d;
	jr nz, copy_loop;
	ld a, $04;
	out ($fb), a;

copy_end:
	call slow_fast;
	pop bc;

;	// THE 'CLEAR PRINTER BUFFER' SUBROUTINE

clear_prb:
	ld hl, $405c;
	ld (hl), $76;
	ld b, $20;

prb_bytes:
	dec hl;
	ld (hl), $00;
	djnz prb_bytes;
	ld a, l;
	set 7, a;
	ld (pr_cc), a;
	ret;

;	// THE 'PRINT AT' SUBROUTINE

print_at:
	ld a, $17;
	sub b;
	jr c, wrong_val;

test_val:
	cp (iy + _df_size);
	jp c, report_5;
	inc a;
	ld b, a;
	ld a, $1f;
	sub c;

wrong_val:
	jp c, report_b;
	add a, $02;
	ld c, a;

set_field:
	bit 1, (iy + _flags);
	jr z, loc_addr;
	ld a, $5d;
	sub c;
	ld (pr_cc), a;
	ret;

;	// THE 'LOC_ADDR' SUBROUTINE

loc_addr:
	ld (s_posn), bc;
	ld hl, (vars);
	ld d, c;
	ld a, $22;
	sub c;
	ld c, a;
	ld a, $76;
	inc b;

look_back:
	dec hl;
	cp (hl);
	jr nz, look_back;
	djnz look_back;
	inc hl;
	cpir;
	dec hl;
	ld (df_cc), hl;
	scf;
	ret po;
	dec d;
	ret z;
	push bc;
	call make_room;
	pop bc;
	ld b, c;
	ld h, d;
	ld l, e;

expand_2:
	ld (hl), $00;
	dec hl;
	djnz expand_2;
	ex de, hl;
	inc hl;
	ld (df_cc), hl;
	ret;

;	// THE 'EXPAND TOKENS' SUBROUTINE

tokens:
	 push af;
	call token_add;
	jr nc, all_chars;
	bit 0, (iy + _flags);
	jr nz, all_chars;
	xor a;
	rst print_a;

all_chars:
	ld a, (bc);
	and $3f;
	rst print_a;
	ld a, (bc);
	inc bc;
	add a, a;
	jr nc, all_chars;
	pop bc;
	bit 7, b;
	ret z;
	cp comma;
	jr z, trail_sp;
	cp $38;
	ret c;

trail_sp:
	xor a;
	set 0, (iy + _flags);
	jp print_sp;

token_add:
	push hl;
	ld hl, $0111;
	bit 7, a;
	jr z, test_high;
	and $3f;

test_high:
	cp $43;
	jr nc, found;
	ld b, a;
	inc b;

words:
	bit 7, (hl);
	inc hl;
	jr z, words;
	djnz words;
	bit 6, a;
	jr nz, comp_flag;
	cp $18;

comp_flag:
	ccf;

found:
	ld b, h;
	ld c, l;
	pop hl;
	ret nc;
	ld a, (bc);
	add a, $e4;
	ret;
