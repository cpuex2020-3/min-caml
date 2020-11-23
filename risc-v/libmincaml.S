	.text
	.balign 8
	.globl min_caml_print_newline
min_caml_print_newline:
	addi	sp, sp, -8
	sw	s0, 4(sp)
	mv	s0, sp
	mv	a1, a0
	li	a0, 10
	sw	ra, 0(sp)
	andi	a2, sp, 7
	beqz	a2, newline.0
	addi	sp, sp, -4
	call	putchar
	addi	sp, sp, 4
	j	newline.1
newline.0:
	call	putchar
newline.1:
	lw	ra, 0(sp)
	mv	sp, s0
	lw	s0, 4(sp)
	addi	sp, sp, 8
	ret

	.section	.rodata
	.align	2
.LC0:
	.string	"%d"
	.text
	.align	2
	.globl	min_caml_print_int
min_caml_print_int:
	addi	sp,sp,-36
	sw	ra,28(sp)
	sw	s0,24(sp)
	addi	s0,sp,36
	mv	a1, a0
	li	a4, 9
	lui	a0,%hi(.LC0)
	addi	a0,a0,%lo(.LC0)
	call	printf
	nop
	lw	ra,28(sp)
	lw	s0,24(sp)
	addi	sp,sp,36
	jr	ra
	.size	min_caml_print_int, .-min_caml_print_int

	.data
format_int:
	.string	"%d"
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
	.globl min_caml_hp
min_caml_hp:
	.long	0x0
