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

;	// THE 'UNSTACK_Z' SUBROUTINE

unstack_z:
	call syntax_z;
	pop hl;
	ret z;
	jp (hl);

;	// THE 'LPRINT' COMMAND ROUTINE

lprint:
	set 1, (iy + _flags);

;	// THE 'PRINT' COMMAND ROUTINE

print:
	ld a, (hl);
	cp $76;
	jp z, print_end;

print_1:
	sub $1a;
	adc a, 0;
	jr z, spacing;
	cp $a7;
	jr nz, not_at;
	rst next_ch;
	call class_6;
	cp comma;
	jp nz, report_c;
	rst next_ch;
	call class_6;
	call syntax_on;
	rst fp_calc;
	defb exchange;
	defb end_calc;
	call stk_to_bc;
	call print_at;
	jr print_on;

not_at:
	cp $a8;
	jr nz, not_tab;
	rst next_ch;
	call class_6;
	call syntax_on;
	call stk_to_a;
	jp nz, report_b;
	and $1f;
	ld c, a;
	bit 1, (iy + _flags);
	jr z, tab_test;
	sub (iy + _pr_cc);
	set 7, a;
	add a, $3c;
	call nc, copy_buff;

tab_test:
	add a, (iy + _s_posn_lo);
	cp $21;
	ld a, (s_posn_hi);
	sbc a, 1;
	call test_val;
	set 0, (iy + _flags);
	jr print_on;

not_tab:
	call scanning;
	call print_stk;

print_on:
	rst get_ch;
	sub $1a;
	adc a, 0;
	jr z, spacing;
	call check_end;
	jp print_end;

spacing:
	call nc, field;
	rst next_ch;
	cp $76;
	ret z;
	jp print_1;

syntax_on:
	call syntax_z;
	ret nz;
	pop hl;
	jr print_on;

print_stk:
	call unstack_z;
	bit 6, (iy + _flags);
	call z, stk_fetch;
	jr z, pr_str_4;
	jp print_fp;

pr_str_1:
	ld a, $0b;

pr_str_2:
	rst print_a;

pr_str_3:
	ld de, (x_ptr);

pr_str_4:
	ld a, b;
	or c;
	dec bc;
	ret z;
	ld a, (de);
	inc de;
	ld (x_ptr), de;
	bit 6, a;
	jr z, pr_str_2;
	cp $c0;
	jr z, pr_str_1;
	push bc;
	call tokens;
	pop bc;
	jr pr_str_3;

print_end:
	call unstack_z;
	ld a, $76;
	rst print_a;
	ret;

field:
	call unstack_z;
	set 0, (iy + _flags);
	xor a;
	rst print_a;
	ld bc, (s_posn);
	ld a, c;
	bit 1, (iy + _flags);
	jr z, centre;
	ld a, $5d;
	sub (iy + _pr_cc);

centre:
	ld c, $11;
	cp c;
	jr nc, right;
	ld c, 1;

right:
	call set_field;
	ret;

;	// THE 'PLOT & UNPLOT' COMMAND ROUTINES

plot_unp:
	call stk_to_bc;
	ld (coords), bc;
	ld a, $2b;
	sub b;
	jp c, report_b;
	ld b, a;
	ld a, 1;
	sra b;
	jr nc, columns;
	ld a, 4;

columns:
	sra c;
	jr nc, find_addr;
	rlca;

find_addr:
	push af;
	call print_at;
	ld a, (hl);
	rlca;
	cp open_bracket;
	jr nc, table_ptr;
	rrca;
	jr nc, sq_saved;
	xor $8f;

sq_saved:
	ld b, a;

table_ptr:
	ld de, $0c9e;
	ld a, (t_addr);
	sub e;
	jp m, plot;
	pop af;
	cpl;
	and b;
	jr unplot;

plot:
	pop af;
	or b;

unplot:
	cp $08;
	jr c, plot_end;
	xor $8f;

plot_end:
	exx;
	rst print_a;
	exx;
	ret;

;	// THE 'stk_to_bc' subroutine

stk_to_bc:
	call stk_to_a;
	ld b, a;
	push bc;
	call stk_to_a;
	ld e, c;
	pop bc;
	ld d, c;
	ld c, a;
	ret;

;	// THE 'STK-TO-A' SUBROUTINE

stk_to_a:
	call fp_to_a;
	jp c, report_b;
	ld c, $01;
	ret z;
	ld c, $ff;
	ret;

;	// THE 'SCROLL' COMMAND ROUTINE

scroll:
	ld b, (iy + _df_size);
	ld c, $21;
	call loc_addr;
	call one_space;
	ld a, (hl);
	ld (de), a;
	inc (iy + _s_posn_hi);
	ld hl, (d_file);
	inc hl;
	ld d, h;
	ld e, l;
	cpir;
	jp reclaim_1;

;	// THE 'SYNTAX TABLES'

offst_tbl:
	defb p_lprint - $;
	defb p_llist - $;
	defb p_stop - $;
	defb p_slow - $;
	defb p_fast - $;
	defb p_new - $;
	defb p_scroll - $;
	defb p_cont - $;
	defb p_dim - $;
	defb p_rem - $;
	defb p_for - $;
	defb p_goto - $;
	defb p_gosub - $;
	defb p_input - $;
	defb p_load - $;
	defb p_list - $;
	defb p_let - $;
	defb p_pause - $;
	defb p_next - $;
	defb p_poke - $;
	defb p_print - $;
	defb p_plot - $;
	defb p_run - $;
	defb p_save - $;
	defb p_rand - $;
	defb p_if - $;
	defb p_cls - $;
	defb p_unplot - $;
	defb p_clear - $;
	defb p_return - $;
	defb p_copy - $;

p_let:
	defb var_rqd, equals, expr_num_str;

p_goto:
	defb num_exp, no_f_ops;
	defw goto;

p_if:
	defb num_exp, tk_then, var_syn;
	defw fn_if;

p_gosub:
	defb num_exp, no_f_ops;
	defw gosub;

p_stop:
	defb no_f_ops;
	defw stop;

p_return:
	defb no_f_ops;
	defw return;

p_for:
	defb chr_var, equals, num_exp, tk_to, num_exp, var_syn;
	defw for;

p_next:
	defb chr_var, no_f_ops;
	defw next;

p_print:
	defb var_syn;
	defw print;

p_input:
	defb var_rqd, no_f_ops;
	defw input;

p_dim:
	defb var_syn;
	defw dim;

p_rem:
	defb var_syn;
	defw rem;

p_new:
	defb no_f_ops;
	defw new;

p_run:
	defb num_exp_0;
	defw run;

p_list:
	defb num_exp_0;
	defw list;

p_poke:
	defb num_exp, comma, num_exp, no_f_ops;
	defw poke;

p_rand:
	defb num_exp_0;
	defw rand;

p_load:
	defb var_syn;
	defw load;

p_save:
	defb var_syn;
	defw save;

p_cont:
	defb no_f_ops;
	defw cont;

p_clear:
	defb no_f_ops;
	defw clear;

p_cls:
	defb no_f_ops;
	defw cls;

p_plot:
	defb num_exp, comma, num_exp, no_f_ops;
	defw plot_unp;

p_unplot:
	defb num_exp, comma, num_exp, no_f_ops;
	defw plot_unp;

p_scroll:
	defb no_f_ops;
	defw scroll;

p_pause:
	defb num_exp, no_f_ops;
	defw pause;

p_slow:
	defb no_f_ops;
	defw slow;

p_fast:
	defb no_f_ops;
	defw fast;

p_copy:
	defb no_f_ops;
	defw copy;

p_lprint:
	defb var_syn;
	defw lprint;

p_llist:
	defb num_exp_0;
	defw llist;

;	// THE 'LINE SCANNING' ROUTINE

line_scan:
	ld (iy + _flags), $01;
	call e_line_no;

line_run:
	call set_mem;
	ld hl, err_nr;
	ld (hl), $ff;
	ld hl, flagx;
	bit 5, (hl);
	jr z, line_null;
	cp $e3;
	ld a, (hl);
	jp nz, input_rep;
	call syntax_z;
	ret z;
	rst error_1;
	defb $0c;

;	// THE 'STOP' COMMAND ROUTINE

stop:
report_9:
	rst error_1;
	defb stop_command;

line_null:
	rst get_ch;
	ld b, 0;
	cp $76;
	ret z;
	ld c, a;
	rst next_ch;
	ld a, c;
	sub $e1 ; last command;
	jr c, report_c2;
	ld c, a;
	ld hl, offst_tbl;
	add hl, bc;
	ld c, (hl);
	add hl, bc;
	jr get_param;

scan_loop:
	ld hl, (t_addr);

get_param:
	ld a, (hl);
	inc hl;
	ld (t_addr), hl;
	ld bc, $0cf4;
	push bc;
	ld c, a;
	cp $0b;
	jr nc, separator;
	ld hl, $0d16;
	ld b, $00;
	add hl, bc;
	ld c, (hl);
	add hl, bc;
	push hl;
	rst get_ch ;
	ret;

separator:
	rst get_ch;
	cp c;
	jr nz, report_c2;
	rst next_ch;
	ret;

;	// THE 'COMMAND CLASS' TABLE

class_tbl:
	defb class_0 - $;
	defb class_1 - $;
	defb class_2 - $;
	defb class_3 - $;
	defb class_4 - $;
	defb class_5 - $;
	defb class_6 - $;

;	// THE 'CHECK-END' SUBROUTINE

check_end:
	call syntax_z;
	ret nz;
	pop bc;

check_2:
	ld a, (hl);
	cp $76;
	ret z;

report_c2:
	jr report_c;

;	// THE 'COMMAND CLASS 3' ROUTINE

class_3:
	cp $76;
	call no_to_stk;

;	// THE 'COMMAND CLASS 0' ROUTINE

class_0:
	cp a;

;	// THE 'COMMAND CLASS 5' ROUTINE

class_5:
	pop bc;
	call z, check_end;
	ex de, hl;
	ld hl, (t_addr);
	ld c, (hl);
	inc hl;
	ld b, (hl);
	ex de, hl;

class_end:
	push bc;
	ret;

;	// THE 'COMMAND CLASS 1' ROUTINE

class_1:
	call look_vars;

class_4_2:
	ld (iy + _flagx), $00;
	jr nc, set_stk;
	set 1, (iy + _flagx);
	jr nz, set_strln;

report_2:
	rst error_1;
	defb missing_variable;

set_stk:
	call z, stk_var;
	bit 6, (iy + _flags);
	jr nz, set_strln;
	xor a;
	call syntax_z;
	call nz, stk_fetch;
	ld hl, flagx;
	or (hl);
	ld (hl), a;
	ex de, hl;

set_strln:
	ld (strlen), bc;
	ld (dest), hl;

rem:
	ret;

;	// THE 'COMMAND CLASS 2' ROUTINE

class_2:
	pop bc;
	ld a, (flags);

input_rep:
	push af;
	call scanning;
	pop af;
	ld bc, $1321;
	ld d, (iy + _flags);
	xor d;
	and %01000000;
	jr nz, report_c;
	bit 7, d;
	jr nz, class_end;
	jr check_2;

;	// THE 'COMMAND CLASS 4' ROUTINE

class_4:
	call look_vars;
	push af;
	ld a, c;
	or %10011111;
	inc a;
	jr nz, report_c;
	pop af;
	jr class_4_2;

;	// THE 'COMMAND CLASS 6' ROUTINE

class_6:
	call scanning;
	bit 6, (iy + _flags);
	ret nz;

report_c:
	rst error_1;
	defb no_numeric_value;
	
;	// THE 'NO-TO-STK' SUBROUTINE

no_to_stk:
	jr nz, class_6;
	call syntax_z;
	ret z;
	rst fp_calc;
	defb stk_zero;
	defb end_calc;
	ret;

;	// THE 'SYNTAX-Z' SUBROUTINE

syntax_z:
	bit 7, (iy + $01) ; flags;
	ret 

;	// THE 'IF' COMMAND ROUTINE

fn_if:
	call syntax_z;
	jr z, if_end;
	rst fp_calc;
	defb delete;
	defb end_calc;
	ld a, (de);
	and a;
	ret z;

if_end:
	jp line_null;

;	// THE 'FOR' COMMAND ROUTINE

for:
	cp $e0;
	jr nz, use_one;
	rst next_ch;
	call class_6;
	call check_end;
	jr reorder;

use_one:
	call check_end;
	rst fp_calc;
	defb stk_one;
	defb end_calc;

reorder:
	rst fp_calc;
	defb st_mem_0;
	defb delete;
	defb exchange;
	defb get_mem_0;
	defb exchange;
	defb end_calc;
	call let;
	ld (mem), hl;
	dec hl;
	ld a, (hl);
	set 7, (hl);
	ld bc, $0006;
	add hl, bc;
	rlca;
	jr c, lmt_step;
	sla c;
	call make_room;
	inc hl;

lmt_step:
	push hl;
	rst fp_calc;
	defb delete;
	defb delete;
	defb end_calc;
	pop hl;
	ex de, hl;
	ld c, $0a;
	ldir;
	ld hl, (ppc);
	ex de, hl;
	inc de;
	ld (hl), e;
	inc hl;
	ld (hl), d;
	call next_loop;
	ret nc;
	bit 7, (iy + _ppc_hi);
	ret nz;
	ld b, (iy + _strlen);
	res 6, b;
	ld hl, (nxtlin);

nxtlin_no:
	ld a, (hl);
	and $c0;
	jr nz, for_end;
	push bc;
	call next_one;
	pop bc;
	inc hl;
	inc hl;
	inc hl;
	call cursor_so;
	rst get_ch;
	cp $f3;
	ex de, hl;
	jr nz,  nxtlin_no;
	ex de, hl;
	rst next_ch;
	ex de, hl;
	cp b;
	jr nz, nxtlin_no;

for_end:
	ld (nxtlin), hl;
	ret;

;	// THE 'NEXT' COMMAND ROUTINE

next:
	bit 1, (iy + _flagx);
	jp nz, report_2;
	ld hl, (dest);
	bit 7, (hl);
	jr z, report_1;
	inc hl;
	ld (mem), hl;
	rst fp_calc;
	defb get_mem_0;
	defb get_mem_2;
	defb addition;
	defb st_mem_0;
	defb delete;
	defb end_calc;
	call next_loop;
	ret c;
	ld hl, (mem);
	ld de, $000f;
	add hl, de;
	ld e, (hl);
	inc hl;
	ld d, (hl);
	ex de, hl;
	jr goto_2;

report_1:
	rst error_1;
	defb next_without_for;

;	// THE 'NEXT-LOOP' SUBROUTINE

next_loop:
	rst fp_calc;
	defb get_mem_1;
	defb get_mem_0;
	defb get_mem_2;
	defb less_0;
	defb jump_true, lmt_v_val - $ - 1;
	defb exchange;

lmt_v_val:
	defb subtract;
	defb greater_0;
	defb jump_true, imposs - $ - 1;
	defb end_calc;
	and a;
	ret;
	
imposs:
	defb end_calc;
	scf;
	ret;

;	// THE 'RAND' COMMAND ROUTINE

rand:
	call find_int;
	ld a, b;
	or c;
	jr nz, set_seed;
	ld bc, (frames);

set_seed:
	ld (seed), bc;
	ret;

;	// THE 'CONT' COMMAND ROUTINE

cont:
	ld hl, (oldppc);
	jr goto_2;

;	// THE 'GOTO' COMMAND ROUTINE

goto:
	call find_int;
	ld h, b;
	ld l, c;

goto_2:
	ld a, h;
	cp $f0;
	jr nc, report_b;
	call line_addr;
	ld (nxtlin), hl;
	ret;

;	// THE 'POKE' COMMAND ROUTINE

poke:
	call fp_to_a;
	jr c, report_b;
	jr z, poke_save;
	neg;

poke_save:
	push af;
	call find_int;
	pop af;
	bit 7, (iy + _err_nr);
	ret z;
	ld (bc), a;
	ret;

;	// THE 'FIND-INT' SUBROUTINE

find_int:
	call fp_to_bc;
	jr c, report_b;
	ret z;

report_b:
	rst error_1;
	defb integer_out_of_range;

;	// THE 'RUN' COMMAND ROUTINE

run:
	call goto;
	jp clear;

;	// THE 'GOSUB' COMMAND ROUTINE

gosub:
	ld hl, (ppc);
	inc hl;
	ex (sp), hl;
	push hl;
	ld (err_sp), sp;
	call goto;
	ld bc, $0006;

;	// THE 'TEST-ROOM' SUBROUTINE

test_room:
	ld hl, (stkend);
	add hl, bc;
	jr c, report_4;
	ex de, hl;
	ld hl, $0024;
	add hl, de;
	sbc hl, sp;
	ret c;

report_4:
	ld l, out_of_ram;
	jp error_3;

;	// THE 'RETURN' COMMAND ROUTINE

return:
	pop hl;
	ex (sp), hl;
	ld a, h;
	cp $3e;
	jr z, report_7;
	ld (err_sp), sp;
	jr goto_2;

report_7:
	ex (sp), hl;
	push hl;
	rst error_1;
	defb return_without_gosub;

;	// THE 'INPUT' COMMAND ROUTINE

input:
	bit 7, (iy + _ppc_hi);
	jr nz, report_8;
	call x_temp;
	ld hl, flagx;
	set 5, (hl);
	res 6, (hl);
	ld a, (flags);
	and $40;
	ld bc, $0002;
	jr nz, prompt;
	ld c, $04;

prompt:
	or (hl);
	ld (hl), a;
	rst bc_spaces;
	ld (hl), $76;
	ld a, c;
	rrca;
	rrca;
	jr c, enter_cur;
	ld a, $0b;
	ld (de), a;
	dec hl;
	ld (hl), a;

enter_cur:
	dec hl;
	ld (hl), $7f;
	ld hl, (s_posn);
	ld (t_addr), hl;
	pop hl;
	jp lower;

report_8:
	rst error_1;
	defb input_as_direct_command;

;	// THE 'FAST' COMMAND ROUTINE

fast:
	call set_fast;
	res 6, (iy + _cdflag);
	ret;

;	// THE 'SLOW' COMMAND ROUTINE

slow:
	set 6, (iy + _cdflag);
	jp slow_fast;

;	// THE 'PAUSE' COMMAND ROUTINE

pause:
	call find_int;
	call set_fast;
	ld h, b;
	ld l, c;
	call display_p;
	ld (iy + _frames_hi), $ff;
	call slow_fast;
	jr d_bounce;

;	// THE 'BREAK-1' SUBROUTINE

break_1:
	ld a, $7f;
	in a, ($fe);
	rra;

;	// THE 'DEBOUNCE' SUBROUTINE

d_bounce:
	res 0, (iy + _cdflag);
	ld a, $ff;
	ld (debounce), a;
	ret;
