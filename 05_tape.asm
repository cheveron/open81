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

;	// THE 'SET FAST MODE' SUBROUTINE

set_fast:
	bit 7, (iy + _cdflag);
	ret z;
	halt;
	out (nmigen), a;
	res 7, (iy + _cdflag);
	ret;

report_f:
	rst error_1;
	defb no_name_supplied;

;	// THE 'SAVE' COMMAND ROUTINE

save:
	call name;
	jr c, report_f;
	ex de, hl;
	ld de, $12cb;

header:
	call break_1;
	jr nc, break_2

delay_1:
	djnz delay_1;
	dec de;
	ld a, d;
	or e;
	jr nz, header;

out_name:
	call out_byte;
	bit 7, (hl);
	inc hl;
	jr z, out_name;
	ld hl, versn;

out_prog:
	call out_byte;
	call load_save;
	jr out_prog;

out_byte:
	ld e, (hl);
	scf;

each_bit:
	rl e;
	ret z;
	sbc a, a;
	and $05;
	add a, $04;
	ld c, a;

pulses:
	out ($ff), a;
	ld b, $23;

delay_2:
	djnz delay_2;
	call break_1;

break_2:
	jr nc, report_d;
	ld b, $1e;

delay_3:
	djnz delay_3;
	dec c;
	jr nz, pulses;

delay_4:
	and a;
	djnz delay_4;
	jr each_bit;

;	// THE 'LOAD' COMMAND ROUTINE

load:
	call name;
	rl d;
	rrc d;

next_prog:
	call in_byte;
	jr next_prog;

in_byte:
	ld c, $01;

next_bit:
	ld b, $00;

break_3:
	ld a, $7f;
	in a, ($fe);
	out ($ff), a;
	rra;
	jr nc, break_4;
	rla;
	rla;
	jr c, get_bit;
	djnz break_3;
	pop af;
	cp d;

restart:
	jp nc, initial;
	ld h, d;
	ld l, e;

in_name:
	call in_byte;
	bit 7, d;
	ld a, c;
	jr nz, matching;
	cp (hl);
	jr nz, next_prog;

matching:
	inc hl;
	rla;
	jr nc, in_name;
	inc (iy + _e_line_hi);
	ld hl, versn;

in_prog:
	ld d, b;
	call in_byte;
	ld (hl), c;
	call load_save;
	jr in_prog;

get_bit:
	push de;
	ld e, $94;

trailer:
	ld b, $1a;

counter:
	dec e;
	in a, ($fe);
	rla;
	bit 7, e;
	ld a, e;
	jr c, trailer;
	djnz counter;
	pop de;
	jr nz, bit_done;
	cp $56;
	jr nc, next_bit;

bit_done:
	ccf;
	rl c;
	jr nc, next_bit;
	ret;

break_4:
	ld a, d;
	and a;
	jr z, restart;

report_d:
	rst error_1;
	defb break_pressed;

;	// THE 'PROGRAM NAME' SUBROUTINE

name:
	call scanning;
	ld a, (flags);
	add a, a;
	jp m, report_c;
	pop hl;
	ret nc;
	push hl;
	call set_fast;
	call stk_fetch;
	ld h, d;
	ld l, e;
	dec c;
	ret m;
	add hl, bc;
	set 7, (hl);
	ret;
