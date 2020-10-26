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

.globl min_caml_create_array
min_caml_create_array:
	addi	sp, sp, -12
	sw	s0, 8(sp)
	mv	s0, sp
	sw	a3, 4(sp)
	sw	a2,	0(sp)
	mv	a3, a0
	mv	a0, t0
	mv	a2, a3
	slli	a2, a2, 2
	add	t0, t0, a2
create_array_loop:
	bne	a3, zero, create_array_cont
create_array_exit:
	lw	a2, 0(sp)
	lw	a3, 4(sp)
	mv	sp, s0
	lw	s0, 8(sp)
	addi	sp, sp, 12
	ret
create_array_cont:
	addi	a3, a3, -1
	slli	t1, a3, 2
	add	a2, t1, a0
	sw	a1, 0(a2)
	j	create_array_loop

	.globl min_caml_sqrt
min_caml_sqrt:
	fsqrt.s	fa0, fa0
	ret
	.globl min_caml_abs_float
min_caml_abs_float:
	ret
	.globl min_caml_sin
min_caml_sin:
	ret
	.globl min_caml_cos
min_caml_cos:
	ret
	.globl min_caml_float_of_int
min_caml_float_of_int:
	fcvt.s.w	fa0, a0
	ret
	.globl min_caml_int_of_float
min_caml_int_of_float:
	fcvt.w.s	a0, fa0
	ret

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
.globl min_caml_fnegd
min_caml_fnegd:
	.long	0
	.long	0x80000000
	.long	0
	.long	0
.globl min_caml_hp
min_caml_hp:
	.long	0x0
