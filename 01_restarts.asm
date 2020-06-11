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

;	// THE 'START'

	org $0000
;start:
	out (nmigen), a;
	ld bc, $7fff;
	jp ram_check;

;	// THE 'ERROR' RESTART

;error_1:
	ld hl, (ch_add);
	ld (x_ptr), hl;
	jr error_2;

;	// THE 'PRINT A CHARACTER' RESTART

;print_a:
	and a;
	jp nz,  print_ch;
	jp print_sp;
	defb $ff;

;	// THE 'COLLECT CHARACTER' RESTART

jp_get_ch:
;get_ch:
	ld hl, (ch_add);
	ld a, (hl);

test_sp:
	and a;
	ret nz;
	nop;
	nop;

;	// THE 'COLLECT NEXT CHARACTER' RESTART

;next_ch:
	call ch_add_inc;
	jr test_sp;

ver_num:
	defb "250";							// version number (example: 2.5.0)

;	// THE 'FP-CALCULATOR' RESTART

;fp_calc:
	jp calculate;

;	// THE 'END-CALC' SUBROUTINE

fp_end_calc:
	pop af;
	exx;
	ex (sp), hl;
	exx;
	ret;

;	// THE 'MAKE BC SPACES' RESTART

;bc_spaces:
	push bc ;
	ld hl, (e_line) ;
	push hl ;
	jp reserve;

;	// THE 'INTERRUPT' RESTART

;interrupt:
	dec c;
	jp nz, scan_line;
	pop hl;
	dec b;
	ret z;
	set 3, c;

wait_int:
	ld r, a;
	ei;
	jp (hl);

scan_line:
	pop de;
	ret z;
	jr wait_int;

;	// THE 'INCREMENT CH-ADD' SUBROUTINE

ch_add_inc:
	ld hl, (ch_add);

cursor_so:
	inc hl;

temp_ptr:
	ld (ch_add), hl;
	ld a, (hl);
	cp $7f;
	ret nz;
	jr cursor_so;

;	// THE 'ERROR-2' ROUTINE

error_2:
	pop hl;
	ld l, (hl);

error_3:
	ld (iy + _err_nr), l;
	ld sp, (err_sp);
	call slow_fast;
	jp set_mem;
	defb $ff;

;	// THE 'NMI' ROUTINE

nmi:
	ex af, af';
	inc a;
	jp m, nmi_ret;
	jr z, nmi_cont;

nmi_ret:
	ex af, af';
	ret;

;	// THE 'PREPARE FOR 'SLOW' DISPLAY' ROUTINE

nmi_cont:
	ex af, af';
	push af;
	push bc;
	push de;
	push hl;
	ld hl, (d_file);
	set 7, h;
	halt;
	out (nmigen), a;
	jp (ix);
