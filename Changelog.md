# Version 2.5

This version applies the canonical bug-fixes to the Version 2 ROM while retaining all entry points prior to the `calculate` routine. Unlike some ZX81 ROMs, `IF 1/4<>0.25 THEN PRINT "BUG"` does not throw an error. It includes floating-point speed improvements from SE Basic IV, including the square root routine by Andy Wright from the [SAM Coupé ROM](https://github.com/cheveron/samrom).

`01_restart.asm`
* Added version number after `next_ch` restart.
* Call to `calculate` now goes to a different address.

`10_expresion.asm`
* Added `l_enter_1` entry point.
* Fixed bug in `nxt_dgt_1`.

`11_arithmetic.asm`
* Added comments from SE Basic IV.
* Fixed bug in `out_zeros`.
* Fixed bug in `count_one`.

`12_calculator.asm`
* Added comments from SE Basic IV.
* Now `table_cons` stored in full.
* Added `stk_pntrs_2` entry point.
* Removed redundant `skip_cons`.
* Simpler `fp_stk_con_xx`.
* Faster `fp_st_mem_xx`.
* Shorter `swap_byte`.
* Moved `fp_negate` for consistency with SE Basic IV.
* Shorter `fp_comparison`.
* Shorter `fp_strs_add`.
* Faster `skt_pntrs`.
* Shorter `fp_chrS`.
* Changed `fp_n_mod_m` to use `mem-1` for consistency with SE Basic IV.
* Shorter `valid`.
* Faster `fp_sqr`.
* Faster `xis0`.
* 19 `spare` bytes.

`13_font.asm`
* Added `org` to `font`.
