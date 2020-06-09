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

;	// THE 'NEW' COMMAND ROUTINE

new:
	call set_fast;
	ld bc, (ramtop);
	dec bc;

;	// THE 'RAM-CHECK' ROUTINE

ram_check:
	ld h, b;
	ld l, c;
	ld a, $3f;

ram_fill:
	ld (hl),  $02;
	dec hl;
	cp h;
	jr nz, ram_fill;

ram_read:
	and a;
	sbc hl, bc;
	add hl, bc;
	inc hl;
	jr nc, set_top;
	dec (hl);
	jr z, set_top;
	dec (hl);
	jr z, ram_read;

set_top:
	ld (ramtop), hl;

;	// THE 'INITIALISATION' ROUTINE

initial:
	ld hl, (ramtop);
	dec hl;
	ld (hl), $3e;
	dec hl;
	ld sp, hl;
	dec hl;
	dec hl;
	ld (err_sp), hl;
	ld a, $1e;
	ld i, a;
	im 1;
	ld iy, err_nr;
	ld (iy + _cdflag), $40;
	ld hl, program;
	ld (d_file), hl;
	ld b, $19;

line:
	ld (hl), $76;
	inc hl;
	djnz line;
	ld (vars), hl;
	call clear;

n_l_only:
	call cursor_in;
	call slow_fast;

;	// PRODUCE THE BASIC LISTING

upper:
	call cls;
	ld hl, (e_ppc);
	ld de, (s_top);
	and a;
	sbc hl, de;
	ex de, hl;
	jr nc, addr_top;
	add hl, de;
	ld (s_top), hl;

addr_top:
	call line_addr;
	jr z, list_top;
	ex de, hl;

list_top:
	call list_prog;
	dec (iy + _breg);
	jr nz, lower;
	ld hl, (e_ppc);
	call line_addr;
	ld hl, (ch_add);
	scf;
	sbc hl, de;
	ld hl, s_top;
	jr nc, inc_line;
	ex de, hl;
	ld a, (hl);
	inc hl;
	ldi;
	ld (de), a;
	jr upper

down_key:
	ld hl, e_ppc;

inc_line:
	ld e, (hl);
	inc hl;
	ld d, (hl);
	push hl;
	ex de, hl;
	inc hl;
	call line_addr;
	call line_no;
	pop hl;

key_input:
	bit 5, (iy + _flagx);
	jr nz, lower;
	ld (hl), d;
	dec hl;
	ld (hl), e;
	jr upper;

;	// COPY THE EDIT-LINE

edit_inp:
	call cursor_in;

lower:
	ld hl, (e_line);

each_char:
	ld a, (hl);
	cp $7e;
	jr nz, end_line;
	ld bc, $0006;
	call reclaim_2;
	jr each_char;

end_line:
	cp $76;
	inc hl;
	jr nz, each_char;

edit_line:
	call cursor;

edit_room:
	call line_ends;
	ld hl, (e_line);
	ld (iy + _err_nr), $ff;
	call copy_line;
	bit 7, (iy + _err_nr);
	jr nz, display_6;
	ld a, (df_size);
	cp $18;
	jr nc, display_6;
	inc a;
	ld (df_size), a;
	ld b, a;
	ld c, $01;
	call loc_addr;
	ld d, h;
	ld e, l;
	ld a, (hl);

free_line:
	dec hl;
	cp (hl);
	jr nz, free_line;
	inc hl;
	ex de, hl;
	ld a, (ramtop_hi);
	cp $4d;
	call c, reclaim_1;
	jr edit_room;

;	// WAITING FOR A KEY

display_6:
	ld hl, $0000;
	ld (x_ptr), hl;
	ld hl, cdflag;
	bit 7, (hl);
	call z, display_1;

slow_disp:
	bit 0, (hl);
	jr z, slow_disp;
	ld bc, (last_k);
	call d_bounce;
	call decode;
	jr nc, lower;

;	// MODE SORTING

mode_sort;
	ld a, (mode);
	dec a;
	jp m, fetch_2;
	jr nz, fetch_1;
	ld (mode), a;
	dec e;
	ld a, e;
	sub $27;
	jr c, func_base;
	ld e, a;

func_base:
	ld hl, $00cc;
	jr table_add;

fetch_1:
	ld a, (hl);
	cp $76;
	jr z, k_l_key;
	cp $40;
	set 7, a;
	jr c, enter;
	ld hl, $00c7;

table_add:
	add hl, de;
	jr fetch_3;

fetch_2:
	ld a, (hl);
	bit 2, (iy + _flags);
	jr nz, test_curs;
	add a, $c0;
	cp $e6;
	jr nc, test_curs;

fetch_3:
	ld a, (hl);

test_curs:
	cp $f0;
	jp pe, key_sort;

enter:
	ld e, a;
	call cursor;
	ld a, e;
	call add_char;

back_next:
	jp lower;

;	// THE 'ADD-CHAR' SUBROUTINE

add_char;
	call one_space;
	ld (de), a;
	ret;

;	// SORTING THE CURSOR KEYS

k_l_key:
	ld a, $78;

key_sort:
	ld e, a;
	ld hl, $0482;
	add hl, de;
	add hl, de;
	ld c, (hl);
	inc hl;
	ld b, (hl);
	push bc;

;	// CHOOSING K v. L MODE

cursor:
	ld hl,  (e_line);
	bit 5, (iy + _flagx);
	jr nz, l_mode

k_mode:
	res 2, (iy + _flags);

test_char;
	ld a, (hl);
	cp $7f;
	ret z;
	inc hl;
	call number;
	jr z, test_char;
	cp $26;
	jr c, test_char;
	cp $de;
	jr z, k_mode;
	
l_mode: set 2, (iy + _flags);
	jr test_char

;	// THE 'CLEAR-ONE' SUBROUTINE

clear_one;
	ld bc, $0001;
	jp reclaim_2

;	// THE 'CURSOR' KEYTABLE
;
	defw up_key;
	defw down_key;
	defw left_key;
	defw right_key;
	defw function;
	defw edit_key;
	defw n_l_key;
	defw rubout;
	defw function;
	defw function

;	// THE 'CURSOR LEFT' ROUTINE


left_key;
	call left_edge;
	ld a, (hl);
	ld (hl), $7f;
	inc hl;
	jr get_code

;	// THE 'CURSOR RIGHT' ROUTINE

right_key;
	inc hl;
	ld a, (hl);
	cp $76;
	jr z, ended_2;
	ld (hl), $7f;
	dec hl

get_code;
	ld (hl), a

ended_1;
	jr back_next

;	// THE 'RUBOUT' ROUTINE

rubout;
	call left_edge;
	call clear_one;
	jr ended_1

;	// THE 'LEFT_EDGE' SUBROUTINE

left_edge:
	dec hl;
	ld de, (e_line);
	ld a, (de);
	cp $7f;
	ret nz;
	pop de;

ended_2:
	jr ended_1;

;	// THE 'CURSOR UP' ROUTINE

up_key:
	ld hl, (e_ppc);
	call line_addr;
	ex de, hl;
	call line_no;
	ld hl, e_ppc_hi;
	jp key_input;

;	// THE 'FUNCTION KEY' ROUTINE

function:
	ld a, e;
	and $07;
	ld (mode), a;
	jr ended_2;

;	// THE 'COLLECT LINE NUMBER' SUBROUTINE

zero_de:
	ex de, hl;
	ld de, $04c2;

line_no:
	ld a, (hl);
	and $c0;
	jr nz, zero_de;
	ld d, (hl);
	inc hl;
	ld e, (hl);
	ret;

;	// THE 'EDIT KEY' ROUTINE

edit_key:
	call line_ends;
	ld hl, edit_inp;
	push hl;
	bit 5, (iy + _flagx);
	ret nz;
	ld hl, (e_line);
	ld (df_cc), hl;
	ld hl, $1821;
	ld (s_posn), hl;
	ld hl, (e_ppc);
	call line_addr;
	call line_no;
	ld a, d;
	or e;
	ret z;
	dec hl;
	call out_no;
	inc hl;
	ld c, (hl);
	inc hl;
	ld b, (hl);
	inc hl;
	ld de, (df_cc);
	ld a, $7f;
	ld (de), a;
	inc de;
	push hl;
	ld hl, $001d;
	add hl, de;
	add hl, bc;
	sbc hl, sp;
	pop hl;
	ret nc;
	ldir;
	ex de, hl;
	pop de;
	call set_stk_b;
	jr ended_2;

;	// THE 'NEWLINE KEY' ROUTINE

n_l_key:
	call line_ends;
	ld hl, lower;
	bit 5, (iy + _flagx);
	jr nz, now_scan;
	ld hl, (e_line);
	ld a, (hl);
	cp $ff;
	jr z, stk_upper;
	call clear_prb;
	call cls;

stk_upper:
	ld hl, upper;

now_scan:
	push hl;
	call line_scan;
	pop hl;
	call cursor;
	call clear_one;
	call e_line_no;
	jr nz, n_l_inp;
	ld a, b;
	or c;
	jp nz, n_l_line;
	dec bc;
	dec bc;
	ld (ppc), bc;
	ld (iy + _df_size), $02;
	ld de, (d_file);
	jr test_null;

n_l_inp:
	cp $76;
	jr z, n_l_null;
	ld bc, (t_addr);
	call loc_addr;
	ld de, (nxtlin);
	ld (iy + _df_size), $02;

test_null:
	rst get_ch;
	cp $76;

n_l_null:
	jp z, n_l_only;
	ld (iy + _flags), $80;
	ex de, hl;

next_line:
	ld (nxtlin), hl;
	ex de, hl;
	call temp_ptr;
	call line_run;
	res 1, (iy + _flags);
	ld a, $c0;
	ld (iy + _x_ptr_hi), a;
	call x_temp;
	res 5, (iy + _flagx);
	bit 7, (iy + _err_nr);
	jr z, stop_line;
	ld hl, (nxtlin);
	and (hl);
	jr nz, stop_line;
	ld d, (hl);
	inc hl;
	ld e, (hl);
	ld (ppc), de;
	inc hl;
	ld e, (hl);
	inc hl;
	ld d, (hl);
	inc hl;
	ex de, hl;
	add hl, de;
	call break_1;
	jr c, next_line;
	ld hl, err_nr;
	bit 7, (hl);
	jr z, stop_line;
	ld (hl), $0c;

stop_line:
	bit 7, (iy + _pr_cc);
	call z, copy_buff;
	ld bc, $0121;
	call loc_addr;
	ld a, (err_nr);
	ld bc, (ppc);
	inc a;
	jr z, report;
	cp $09;
	jr nz, continue;
	inc bc;

continue:
	ld (oldppc), bc;
	jr nz, report;
	dec bc;

report:
	call out_code;
	ld a, $18;
	rst print_a;
	call out_num;
	call cursor_in;
	jp display_6;

n_l_line:
	ld (e_ppc), bc;
	ld hl, (ch_add);
	ex de, hl;
	ld hl, n_l_only;
	push hl;
	ld hl, (stkbot);
	sbc hl, de;
	push hl;
	push bc;
	call set_fast;
	call cls;
	pop hl;
	call line_addr;
	jr nz, copy_over;
	call next_one;
	call reclaim_2;

copy_over:
	pop bc;
	ld a, c;
	dec a;
	or b;
	ret z;
	push bc;
	inc bc;
	inc bc;
	inc bc;
	inc bc;
	dec hl;
	call make_room;
	call slow_fast;
	pop bc;
	push bc;
	inc de;
	ld hl, (stkbot);
	dec hl;
	lddr;
	ld hl, (e_ppc);
	ex de, hl;
	pop bc;
	ld (hl), b;
	dec hl;
	ld (hl), c;
	dec hl;
	ld (hl), e;
	dec hl;
	ld (hl), d;
	ret;

;	// THE 'LIST' COMMAND ROUTINE

llist:
	set 1, (iy + _flags);

list:
	call find_int;
	ld a, b;
	and $3f;
	ld h, a;
	ld l, c;
	ld (e_ppc), hl;
	call line_addr;

list_prog:
	ld e, $00;

until_end:
	call out_line;
	jr until_end;
	
;	// THE 'PRINT A BASIC LINE' SUBROUTINE

out_line:
	ld bc, (e_ppc);
	call cp_lines;
	ld d, $92;
	jr z, test_end;
	ld de, $0000;
	rl e;

test_end:
	ld (iy + _breg), e;
	ld a, (hl);
	cp $40;
	pop bc;
	ret nc;
	push bc;
	call out_no;
	inc hl;
	ld a, d;
	rst print_a;
	inc hl;
	inc hl;

copy_line:
	ld (ch_add), hl;
	set 0, (iy + _flags);

more_line:
	ld bc, (x_ptr);
	ld hl, (ch_add);
	and a;
	sbc hl, bc;
	jr nz, test_num;
	ld a, $b8;
	rst print_a;

test_num:
	ld hl, (ch_add);
	ld a, (hl);
	inc hl;
	call number;
	ld (ch_add), hl;
	jr z, more_line;
	cp $7f;
	jr z, out_curs;
	cp $76;
	jr z, out_ch;
	bit 6, a;
	jr z, not_token;
	call tokens;
	jr more_line;

not_token:
	rst print_a;
	jr more_line;

out_curs:
	ld a, (mode);
	ld b, $ab;
	and a;
	jr nz, flags_2;
	ld a, (flags);
	ld b, $b0;

flags_2:
	rra;
	rra;
	and $01;
	add a, b;
	call print_sp;
	jr more_line;

;	// THE 'NUMBER' SUBROUTINE

number:
	cp $7e;
	ret nz;
	inc hl;
	inc hl;
	inc hl;
	inc hl;
	inc hl;
	ret;
