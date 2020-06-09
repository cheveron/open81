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

;	// THE 'KEY TABLES'

k_unshifted:
	defb 'Z' - 27;
	defb 'X' - 27;
	defb 'C' - 27;
	defb 'V' - 27;
	defb 'A' - 27;
	defb 'S' - 27;
	defb 'D' - 27;
	defb 'F' - 27;
	defb 'G' - 27;
	defb 'Q' - 27;
	defb 'W' - 27;
	defb 'E' - 27;
	defb 'R' - 27;
	defb 'T' - 27;
	defb '1' - 20;
	defb '2' - 20;
	defb '3' - 20;
	defb '4' - 20;
	defb '5' - 20;
	defb '0' - 20;
	defb '9' - 20;
	defb '8' - 20;
	defb '7' - 20;
	defb '6' - 20;
	defb 'P' - 27;
	defb 'O' - 27;
	defb 'I' - 27;
	defb 'U' - 27;
	defb 'Y' - 27;
	defb ctrl_newline;
	defb 'L' - 27;
	defb 'K' - 27;
	defb 'J' - 27;
	defb 'H' - 27;
	defb ' ' - 32;
	defb '.' - 19;
	defb 'M' - 27;
	defb 'N' - 27;
	defb 'B' - 27

k_shifted:
	defb ':' - 44;
	defb ';' - 34;
	defb '?' - 48;
	defb '/' - 23;
	defb tk_stop, tk_lprint, tk_slow, tk_fast, tk_llist, tk_dquote;
	defb tk_or, tk_step, tk_l_eql, tk_neql, ctrl_edit, tk_and;
	defb tk_then, tk_to, ctrl_left, ctrl_rubout, ctrl_graphics;
	defb ctrl_right, ctrl_up, ctrl_down;
	defb '"' - 23;";"
	defb ')' - 24;
	defb '(' - 24;
	defb '$' - 23;
	defb tk_gr_eq;
	defb ctrl_function;
	defb '=' - 41;
	defb '+' - 22;
	defb '-' - 23;
	defb tk_dstar;
	defb $0c; £;
	defb ',' - 18;
	defb '>' - 44;
	defb '<' - 41;
	defb '*' - 19;

k_function:
	defb tk_ln, tk_exp, tk_at, ctrl_kl, tk_asn, tk_acs, tk_atn, tk_sgn;
	defb tk_abs, tk_sin, tk_cos, tk_tan, tk_int, tk_rnd, ctrl_kl, ctrl_kl;
	defb ctrl_kl, ctrl_kl, ctrl_kl, ctrl_kl, ctrl_kl, ctrl_kl, ctrl_kl;
	defb ctrl_kl, tk_tab, tk_peek, tk_code, tk_chrS, tk_strS, ctrl_kl;
	defb tk_usr, tk_len, tk_val, tk_sqr, ctrl_kl, ctrl_kl, tk_pi;
	defb tk_not, tk_inkeys;

k_graphic:
	defb $08, $0a, $09, $8a, $89, $81, $82, $07;
	defb $84, $06, $01, $02, $87, $04, $05;
	defb ctrl_rubout, ctrl_kl, $85, $03, $83, $8b;
	defb ')' + 104;
	defb '(' + 104;
	defb '$' + 105;
	defb $86, ctrl_kl;
	defb '>' + 84;
	defb '+' + 106;
	defb '-' + 105;
	defb $88;

k_token:
	defb $8f;							// ?
	defb $0b, $8b;						// ""
	defb $26, $b9;						// AT
	defb $39, $26, $a7;					// TAB
	defb $8f;							// ?
	defb $28, $34, $29, $aa;			// CODE
	defb $3b, $26, $b1;					// VAL
	defb $31, $2a, $b3;					// LEN
	defb $38, $2e, $b3;					// SIN
	defb $28, $34, $b8;					// COS
	defb $39, $26, $b3;					// TAN
	defb $26, $38, $b3;					// ASN
	defb $26, $28, $b8;					// ACS
	defb $26, $39, $b3;					// ATN
	defb $31, $b3;						// LN
	defb $2a, $3d, $b5;					// EXP
	defb $2e, $33, $b9;					// INT
	defb $38, $36, $b7;					// SQR
	defb $38, $2c, $b3;					// SGN
	defb $26, $27, $b8;					// ABS
	defb $35, $2a, $2a, $b0;			// PEEK
	defb $3a, $38, $b7;					// USR
	defb $38, $39, $37, $8d;			// STR$
	defb $28, $2d, $37, $8d;			// CHR$
	defb $33, $34, $b9;					// NOT
	defb $17, $97;						// **
	defb $34, $b7;						// OR
	defb $26, $33, $a9;					// AND
	defb $13, $94;						// <=
	defb $12, $94;						// >=
	defb $13, $92;						// <>
	defb $39, $2d, $2a, $b3;			// THEN
	defb $39, $b4;						// TO
	defb $38, $39, $2a, $b5;			// STEP
	defb $31, $35, $37, $2e, $33, $b9;	// LPRINT
	defb $31, $31, $2e, $38, $b9;		// LLIST
	defb $38, $39, $34, $b5;			// STOP
	defb $38, $31, $34, $bc;			// SLOW
	defb $2b, $26, $38, $b9;			// FAST
	defb $33, $2a, $bc;					// NEW
	defb $38, $28, $37, $34, $31, $b1;	// SCROLL
	defb $28, $34, $33, $b9;			// CONT
	defb $29, $2e, $b2;					// DIM
	defb $37, $2a, $b2;					// REM
	defb $2b, $34, $b7;					// FOR
	defb $2c, $34, $39, $b4;			// GOTO
	defb $2c, $34, $38, $3a, $a7;		// GOSUB
	defb $2e, $33, $35, $3a, $b9;		// INPUT
	defb $31, $34, $26, $a9;			// LOAD
	defb $31, $2e, $38, $b9;			// LIST
	defb $31, $2a, $b9;					// LET
	defb $35, $26, $3a, $38, $aa;		// PAUSE
	defb $33, $2a, $3d, $b9;			// NEXT
	defb $35, $34, $30, $aa;			// POKE
	defb $35, $37, $2e, $33, $b9;		// PRINT
	defb $35, $31, $34, $b9;			// PLOT
	defb $37, $3a, $b3;					// RUN
	defb $38, $26, $3b, $aa;			// SAVE
	defb $37, $26, $33, $a9;			// RAND
	defb $2e, $ab;						// IF
	defb $28, $31, $b8;					// CLS
	defb $3a, $33, $35, $31, $34, $b9;	// UNPLOT
	defb $28, $31, $2a, $26, $b7;		// CLEAR
	defb $37, $2a, $39, $3a, $37, $b3;	// RETURN
	defb $28, $34, $35, $be;			// COPY
	defb $37, $33, $a9;					// RND
	defb $2e, $33, $30, $2a, $3e, $8d;	// INKEY$
	defb $35, $ae;						// PI
