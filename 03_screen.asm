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

;	// THE 'LOAD/SAVE UPDATE' SUBROUTINE

load_save:
	inc hl;
	ex de, hl;
	ld hl, (e_line);
	scf;
	sbc hl, de;
	ex de, hl;
	ret nc;
	pop hl;

;	// THE 'DISPLAY' ROUTINES

slow_fast:
	ld hl, cdflag;
	ld a, (hl);
	rla;
	xor (hl);
	rla;
	ret nc;
	ld a, $7f;
	ex af, af';
	ld b, $11;
	out ($fe), a;

loop_11:
	djnz loop_11;
	out (nmigen), a;
	ex af, af';
	rla;
	jr nc, no_slow;
	set 7, (hl);
	push af;
	push bc;
	push de;
	push hl;
	jr display_1;

no_slow:
	res 6, (hl);
	ret;

display_1:
	ld hl, (frames);
	dec hl;

display_p:
	ld a, $7f;
	and h;
	or l;
	ld a, h;
	jr nz, another;
	rla;
	jr over_nc;

another:
	ld b, (hl);
	scf;

over_nc:
	ld h, a;
	ld (frames), hl;
	ret nc;

display_2:
	call keyboard;
	ld bc, (last_k);
	ld (last_k), hl;
	ld a, b;
	add a, $02;
	sbc hl, bc;
	ld a, (debounce);
	or h;
	or l;
	ld e, b;
	ld b, $0b;
	ld hl, cdflag;
	res 0, (hl);
	jr nz, no_key;
	bit 7, (hl);
	set 0, (hl);
	ret z;
	dec b;
	nop;
	scf;

no_key:
	ld hl, debounce;
	ccf;
	rl b;

loop_b:
	djnz loop_b;
	ld b, (hl);
	ld a, e;
	cp $fe;
	sbc a, a;
	ld b, $1f;
	or (hl);
	and b;
	rra;
	ld (hl), a;
	out ($ff), a;
	ld hl, (d_file);
	set 7, h;
	call display_3;
	ld a, r;
	ld bc, $1901;
	ld a, $f5;
	call display_5;
	dec hl;
	call display_3;
	jp display_1;

display_3:
	pop ix;
	ld c, (iy + _margin);
	bit 7, (iy + _cdflag);
	jr z, display_4;
	ld a, c;
	neg;
	inc a;
	ex af, af';
	out ($fe), a;
	pop hl;
	pop de;
	pop bc;
	pop af;
	ret;

display_4:
	ld a, $fc;
	ld b, $01;
	call display_5;
	dec hl;
	ex (sp), hl;
	ex (sp), hl;
	jp (ix);

display_5:
	ld r, a;
	ld a, $dd;
	ei;
	jp (hl);
