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

;	// THE 'KEYBOARD SCANNING' SUBROUTINE

keyboard:
	ld hl, $ffff;
	ld bc, $fefe;
	in a, (c);
	or $01;

each_line:
	or $e0;
	ld d, a;
	cpl;
	cp $01;
	sbc a, a;
	or b;
	and l;
	ld l, a;
	ld a, h;
	and d;
	ld h, a;
	rlc b;
	in a, (c);
	jr c, each_line;
	rra;
	rl h;
	rla;
	rla;
	rla;
	sbc a, a;
	and $18;
	add a, $1f;
	ld (margin), a;
	ret;
