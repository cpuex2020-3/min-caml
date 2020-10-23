.text
.globl min_caml_print_int
min_caml_print_int:
	addi	sp, sp, -8
	sw	fp, 4(sp)
	mv	fp, sp
	mv	a1, a0
	la	a0, format_int
	sw	ra, 0(sp)
	andi	a2, sp, 7
	beqz	a2, L0
	addi	sp, sp, -4
	call	printf
	addi	sp, sp, 4
	j	L1
L0:
	call	printf
L1:
	lw	ra, 0(sp)
	mv	sp, fp
	lw	fp, 4(sp)
	addi	sp, sp, 8
	ret
.data
format_int:
	.asciz	"%d"
format_float:
	.asciz	"%lf"
.balign 8
float_0:
	.long	0x0
	.long	0x0
float_1:
	.long	0x0
	.long	0x3ff00000
.balign 16
.globl min_caml_fnegd
min_caml_fnegd:
	.long	0
	.long	0x80000000
	.long	0
	.long	0
.globl min_caml_hp
min_caml_hp:
	.long	0x0
