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
 
;	// THE 'FONT'
 
	org $1e00

font:
	defb $00, $00, $00, $00, $00, $00, $00, $00;	// 
	defb $f0, $f0, $f0, $f0, $00, $00, $00, $00;	// '
	defb $0f, $0f, $0f, $0f, $00, $00, $00, $00;	//  '
	defb $ff, $ff, $ff, $ff, $00, $00, $00, $00;	// ''
	defb $00, $00, $00, $00, $f0, $f0, $f0, $f0;	// .
	defb $f0, $f0, $f0, $f0, $f0, $f0, $f0, $f0;	// |
	defb $0f, $0f, $0f, $0f, $f0, $f0, $f0, $f0;	// .'
	defb $ff, $ff, $ff, $ff, $f0, $f0, $f0, $f0;	// |'
	defb $aa, $55, $aa, $55, $aa, $55, $aa, $55;	// ;;
	defb $00, $00, $00, $00, $aa, $55, $aa, $55;	// ,,
	defb $aa, $55, $aa, $55, $00, $00, $00, $00;	// ''
	defb $00, $24, $24, $00, $00, $00, $00, $00;	// "
	defb $00, $1c, $22, $78, $20, $20, $7e, $00;	// £
	defb $00, $08, $3e, $28, $3e, $0a, $3e, $08;	// $
	defb $00, $00, $00, $10, $00, $00, $10, $00;	// 
	defb $00, $3c, $42, $04, $08, $00, $08, $00;	// ?
	defb $00, $04, $08, $08, $08, $08, $04, $00;	// (
	defb $00, $20, $10, $10, $10, $10, $20, $00;	// )
	defb $00, $00, $10, $08, $04, $08, $10, $00;	// >
	defb $00, $00, $04, $08, $10, $08, $04, $00;	// <
	defb $00, $00, $00, $3e, $00, $3e, $00, $00;	// =
	defb $00, $00, $08, $08, $3e, $08, $08, $00;	// +
	defb $00, $00, $00, $00, $3e, $00, $00, $00;	// -
	defb $00, $00, $14, $08, $3e, $08, $14, $00;	// *
	defb $00, $00, $02, $04, $08, $10, $20, $00;	// /
	defb $00, $00, $10, $00, $00, $10, $10, $20;	// ;
	defb $00, $00, $00, $00, $00, $08, $08, $10;	// ,
	defb $00, $00, $00, $00, $00, $18, $18, $00;	// .
	defb $00, $3c, $46, $4a, $52, $62, $3c, $00;	// 0
	defb $00, $18, $28, $08, $08, $08, $3e, $00;	// 1
	defb $00, $3c, $42, $02, $3c, $40, $7e, $00;	// 2
	defb $00, $3c, $42, $0c, $02, $42, $3c, $00;	// 3
	defb $00, $08, $18, $28, $48, $7e, $08, $00;	// 4
	defb $00, $7e, $40, $7c, $02, $42, $3c, $00;	// 5
	defb $00, $3c, $40, $7c, $42, $42, $3c, $00;	// 6
	defb $00, $7e, $02, $04, $08, $10, $10, $00;	// 7
	defb $00, $3c, $42, $3c, $42, $42, $3c, $00;	// 8
	defb $00, $3c, $42, $42, $3e, $02, $3c, $00;	// 9
	defb $00, $3c, $42, $42, $7e, $42, $42, $00;	// A
	defb $00, $7c, $42, $7c, $42, $42, $7c, $00;	// B
	defb $00, $3c, $42, $40, $40, $42, $3c, $00;	// C
	defb $00, $78, $44, $42, $42, $44, $78, $00;	// D
	defb $00, $7e, $40, $7c, $40, $40, $7e, $00;	// E
	defb $00, $7e, $40, $7c, $40, $40, $40, $00;	// F
	defb $00, $3c, $42, $40, $4e, $42, $3c, $00;	// G
	defb $00, $42, $42, $7e, $42, $42, $42, $00;	// H
	defb $00, $3e, $08, $08, $08, $08, $3e, $00;	// I
	defb $00, $02, $02, $02, $42, $42, $3c, $00;	// J
	defb $00, $44, $48, $70, $48, $44, $42, $00;	// K
	defb $00, $40, $40, $40, $40, $40, $7e, $00;	// L
	defb $00, $42, $66, $5a, $42, $42, $42, $00;	// M
	defb $00, $42, $62, $52, $4a, $46, $42, $00;	// N
	defb $00, $3c, $42, $42, $42, $42, $3c, $00;	// O
	defb $00, $7c, $42, $42, $7c, $40, $40, $00;	// P
	defb $00, $3c, $42, $42, $52, $4a, $3c, $00;	// Q
	defb $00, $7c, $42, $42, $7c, $44, $42, $00;	// R
	defb $00, $3c, $40, $3c, $02, $42, $3c, $00;	// S
	defb $00, $fe, $10, $10, $10, $10, $10, $00;	// T
	defb $00, $42, $42, $42, $42, $42, $3c, $00;	// U
	defb $00, $42, $42, $42, $42, $24, $18, $00;	// V
	defb $00, $42, $42, $42, $42, $5a, $24, $00;	// W
	defb $00, $42, $24, $18, $18, $24, $42, $00;	// X
	defb $00, $82, $44, $28, $10, $10, $10, $00;	// Y
	defb $00, $7e, $04, $08, $10, $20, $7e, $00;	// Z
