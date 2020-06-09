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

;	// THE 'ONE-SPACE' SUBROUTINE

one_space:
	ld bc, $0001;

;	// THE 'MAKE-ROOM' SUBROUTINE

make_room:
	push hl;
	call test_room;
	pop hl;
	call pointers;
	ld hl, (stkend);
	ex de, hl;
	lddr;
	ret;

;	// THE 'CHANGE ALL POINTERS' SUBROUTINE

pointers:
	push af;
	push hl;
	ld hl, d_file;
	ld a, $09;

next_ptr:
	ld e, (hl);
	inc hl;
	ld d, (hl);
	ex (sp), hl;
	and a;
	sbc hl, de;
	add hl, de;
	ex (sp), hl;
	jr nc, ptr_done;
	push de;
	ex de, hl;
	add hl, bc;
	ex de, hl;
	ld (hl), d;
	dec hl;
	ld (hl), e;
	inc hl;
	pop de;

ptr_done:
	inc hl;
	dec a;
	jr nz, next_ptr;
	ex de, hl;
	pop de;
	pop af;
	and a;
	sbc hl, de;
	ld b, h;
	ld c, l;
	inc bc;
	add hl, de;
	ex de, hl;
	ret;

;	// THE 'LINE-ADDR' SUBROUTINE

line_addr:
	push hl;
	ld hl, program;
	ld d, h;
	ld e, l;

next_test:
	pop bc;
	call cp_lines;
	ret nc;
	push bc;
	call next_one;
	ex de, hl;
	jr next_test;

;	// THE 'COMPARE LINE NUMBERS' SUBROUTINE

cp_lines:
	ld a, (hl);
	cp b;
	ret nz;
	inc hl;
	ld a, (hl);
	dec hl;
	cp c;
	ret;

;	// THE 'NEXT LINE OR VARIABLE' SUBROUTINE

next_one:
	push hl;
	ld a, (hl);
	cp $40;
	jr c, lines;
	bit 5, a;
	jr z, bit_5_nil;
	add a, a;
	jp m, next_five;
	ccf;

next_five:
	ld bc, $0005;
	jr nc, next_lett;
	ld c, $11;

next_lett:
	rla;
	inc hl;
	ld a, (hl);
	jr nc, next_lett;
	jr next_add;

lines:
	inc hl;

bit_5_nil:
	inc hl;
	ld c, (hl);
	inc hl;
	ld b, (hl);
	inc hl;

next_add:
	add hl, bc;
	pop de;

;	// THE 'DIFFERENCE' SUBROUTINE

differ:
	and a;
	sbc hl, de;
	ld b, h;
	ld c, l;
	add hl, de;
	ex de, hl;
	ret;

;	// THE 'LINE ENDS' SUBROUTINE

line_ends:
	ld b, (iy + _df_size);
	push bc;
	call b_lines;
	pop bc;
	dec b;
	jr b_lines;

;	// THE 'CLS' COMMAND ROUTINE

cls:
	ld b, $18;

b_lines:
	res 1, (iy + _flags);
	ld c, $21;
	push bc;
	call loc_addr;
	pop bc;
	ld a, (ramtop_hi);
	cp $4d;
	jr c, collapsed;
	set 7, (iy + _s_posn_hi);

clear_loc:
	xor a;
	call print_sp;
	ld hl, (s_posn);
	ld a, l;
	or h;
	and $7e;
	jr nz, clear_loc;
	jp loc_addr;

collapsed:
	ld d, h;
	ld e, l;
	dec hl;
	ld c, b;
	ld b, $00;
	ldir;
	ld hl, (vars);

;	// THE 'RECLAIMING' SUBROUTINES

reclaim_1:
	call differ;

reclaim_2:
	push bc;
	ld a, b;
	cpl;
	ld b, a;
	ld a, c;
	cpl;
	ld c, a;
	inc bc;
	call pointers;
	ex de, hl;
	pop hl;
	add hl, de;
	push de;
	ldir;
	pop hl;
	ret;

;	// THE 'E-LINE NUMBER' SUBROUTINE

e_line_no:
	ld hl, (e_line);
	call temp_ptr;
	rst get_ch;
	bit 5, (iy + _flagx);
	ret nz;
	ld hl, $405d ; mem_0_1st;
	ld (stkend), hl;
	call int_to_fp;
	call fp_to_bc;
	jr c, no_number;
	ld hl, $d8f0;
	add hl, bc;

no_number:
	jp c, report_c;
	cp a;
	jp set_mem;

;	// THE 'REPORT & LINE NUMBER' PRINTING SUBROUTINES

out_num:
	push de;
	push hl;
	xor a;
	bit 7, b;
	jr nz, units;
	ld h, b;
	ld l, c;
	ld e, $ff;
	jr thousand;

out_no:
	push de;
	ld d, (hl);
	inc hl;
	ld e, (hl);
	push hl;
	ex de, hl;
	ld e, $00;

thousand:
	ld bc, $fc18;
	call out_digit;
	ld bc, $ff9c;
	call out_digit;
	ld c, $f6;
	call out_digit;
	ld a, l;

units:
	call out_code;
	pop hl;
	pop de;
	ret;




