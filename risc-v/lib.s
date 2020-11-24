.data
l.198:	# 3.000000
	.word	0x40400000
l.196:	# 720.000000
	.word	0x44340000
l.194:	# 24.000000
	.word	0x41c00000
l.192:	# 2.000000
	.word	0x40000000
l.190:	# 1.000000
	.word	0x3f800000
l.187:	# 5040.000000
	.word	0x459d8000
l.185:	# 120.000000
	.word	0x42f00000
l.183:	# 6.000000
	.word	0x40c00000
l.181:	# 6.283184
	.word	0x40c90fd8
.text
	.globl min_caml_sin
min_caml_sin:
	la	a0, l.181
	flw	fa1, 0(a0)
	fdiv.s	fa2, fa0, fa1
	fsw	fa0, 0(s0)
	fsw	fa1, 8(s0)
	fsgnj.s	fa0, fa2, fa2
	sw	ra, 16(s0)
	addi	s0, s0, 20
	jal	min_caml_int_of_float
	addi	s0, s0, -20
	lw	ra, 16(s0)
	sw	ra, 16(s0)
	addi	s0, s0, 20
	jal	min_caml_float_of_int
	addi	s0, s0, -20
	lw	ra, 16(s0)
	flw	fa1, 8(s0)
	fmul.s	fa1, fa1, fa0
	flw	fa0, 0(s0)
	fsub.s	fa0, fa0, fa1
	fmul.s	fa1, fa0, fa0
	fmul.s	fa1, fa1, fa0
	la	a0, l.183
	flw	fa2, 0(a0)
	fdiv.s	fa1, fa1, fa2
	fsub.s	fa1, fa0, fa1
	fmul.s	fa2, fa0, fa0
	fmul.s	fa2, fa2, fa0
	fmul.s	fa2, fa2, fa0
	fmul.s	fa2, fa2, fa0
	la	a0, l.185
	flw	fa3, 0(a0)
	fdiv.s	fa2, fa2, fa3
	fadd.s	fa1, fa1, fa2
	fmul.s	fa2, fa0, fa0
	fmul.s	fa2, fa2, fa0
	fmul.s	fa2, fa2, fa0
	fmul.s	fa2, fa2, fa0
	fmul.s	fa2, fa2, fa0
	fmul.s	fa2, fa2, fa0
	la	a0, l.187
	flw	fa0, 0(a0)
	fdiv.s	fa2, fa2, fa0
	fsub.s	fa0, fa1, fa2
	ret
	.globl min_caml_cos
min_caml_cos:
	la	a0, l.181
	flw	fa1, 0(a0)
	fdiv.s	fa2, fa0, fa1
	fsw	fa0, 0(s0)
	fsw	fa1, 8(s0)
	fsgnj.s	fa0, fa2, fa2
	sw	ra, 16(s0)
	addi	s0, s0, 20
	jal	min_caml_int_of_float
	addi	s0, s0, -20
	lw	ra, 16(s0)
	sw	ra, 16(s0)
	addi	s0, s0, 20
	jal	min_caml_float_of_int
	addi	s0, s0, -20
	lw	ra, 16(s0)
	flw	fa1, 8(s0)
	fmul.s	fa1, fa1, fa0
	flw	fa0, 0(s0)
	fsub.s	fa0, fa0, fa1
	la	a0, l.190
	flw	fa1, 0(a0)
	fmul.s	fa2, fa0, fa0
	la	a0, l.192
	flw	fa3, 0(a0)
	fdiv.s	fa2, fa2, fa3
	fsub.s	fa1, fa1, fa2
	fmul.s	fa2, fa0, fa0
	fmul.s	fa2, fa2, fa0
	fmul.s	fa2, fa2, fa0
	la	a0, l.194
	flw	fa3, 0(a0)
	fdiv.s	fa2, fa2, fa3
	fadd.s	fa1, fa1, fa2
	fmul.s	fa2, fa0, fa0
	fmul.s	fa2, fa2, fa0
	fmul.s	fa2, fa2, fa0
	fmul.s	fa2, fa2, fa0
	fmul.s	fa2, fa2, fa0
	la	a0, l.196
	flw	fa0, 0(a0)
	fdiv.s	fa2, fa2, fa0
	fsub.s	fa0, fa1, fa2
	ret
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

	.globl min_caml_create_float_array
min_caml_create_float_array:
	addi	sp, sp, -12
	sw	s0, 8(sp)
	mv	s0, sp
	sw	a3, 4(sp)
	sw	a2,	0(sp)
	mv	a3, a0
	mv	a0, t0
	mv	a2, a3
	slli	a2, a2, 3
	add	t0, t0, a2
create_float_array_loop:
	bne	a3, zero, create_float_array_cont
create_float_array_exit:
	lw	a2, 0(sp)
	lw	a3, 4(sp)
	mv	sp, s0
	lw	s0, 8(sp)
	addi	sp, sp, 12
	ret
create_float_array_cont:
	addi	a3, a3, -1
	slli	t1, a3, 3
	add	a2, t1, a0
	fsw	fa0, 0(a2)
	j	create_float_array_loop
	ret

	.globl min_caml_sqrt
min_caml_sqrt:
	fsqrt.s	fa0, fa0
	ret
	.globl min_caml_abs_float
min_caml_abs_float:
	fsgnjx.s	fa0, fa0, fa0
	ret
	.globl min_caml_float_of_int
min_caml_float_of_int:
	fcvt.s.w	fa0, a0
	ret
	.globl min_caml_truncate
min_caml_truncate:
	.globl min_caml_int_of_float
min_caml_int_of_float:
	fcvt.w.s	a0, fa0
	ret
	.globl min_caml_floor
min_caml_floor:
	fcvt.w.s	a0, fa0
	ret
