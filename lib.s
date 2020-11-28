	.data
l.one:	# 1.000000
	.word	0x3f800000
l.ftoi_cmp: # 8388608.0
	.word	0x4b000000
	.text
	.globl min_caml_create_array
min_caml_create_array:
	mv	t5, a0
	slli	t4, t5, 2
create_array_loop:
	bne	t5, zero, create_array_cont
create_array_exit:
	add	t0, t0, t4
	ret
create_array_cont:
	addi	t5, t5, -1
	slli	t6, t5, 2
	add	t6, t6, t0
	sw	a1, 0(t6)
	j	create_array_loop

	.globl min_caml_create_float_array
min_caml_create_float_array:
	mv	t5, a0
	slli	t4, t5, 2
create_float_array_loop:
	bne	t5, zero, create_float_array_cont
create_float_array_exit:
	add	t0, t0, t4
	ret
create_float_array_cont:
	addi	t5, t5, -1
	slli	t6, t5, 2
	add	t6, t6, t0
	fsw	fa0, 0(t6)
	j	create_float_array_loop

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
	la	t6, l.ftoi_cmp
	flw	ft0, 0(t6)
	li	t4, 1258291200
	flt.s	t3, fa0, ft0
	beq	t3, zero, ftoi_else
	fadd.s	fa0, fa0, ft0
	fmv.x.s	a0, fa0
	sub	a0, a0, t4
	ret
ftoi_else:
	li	t5, 0
ftoi_cont:
	flt.s	t3, fa0, ft0
	bne	t3, zero, ftoi_sum
	fsub.s	fa0, fa0, ft0
	addi	t5, t5, 1
	j	ftoi_cont
ftoi_sum:
	fadd.s	fa0, fa0, ft0
	fmv.x.s	a0, fa0
	sub	a0, a0, t4
	li	t4, 8388608
ftoi_loop:
	bne	t5, zero, ftoi_sum_cont
	ret
ftoi_sum_cont:
	addi	t5, t5, -1
	add	a0, a0, t4
	j	ftoi_loop

	.globl min_caml_floor
min_caml_floor:
	la	t6, l.ftoi_cmp
	flw	ft0, 0(t6)
	fsgnjx.s	ft1, fa0, fa0
	fsgnj.s	ft2, fa0, fa0
	flt.s	t3, ft1, ft0
	beq	t3, zero, floor_ret
	sw	a0, 4(s0)
	sw	ra, 8(s0)
	addi	s0, s0, 12
	jal	min_caml_int_of_float
	jal	min_caml_float_of_int
	addi	s0, s0, -12
	lw	ra, 8(s0)
	lw	a0, 4(s0)
	flt.s	t3, ft2, fa0
	beq	t3, zero, floor_ret
	la	t6, l.one
	flw	ft2, 0(t6)
	fsub.s	fa0, fa0, ft2
floor_ret:
	ret
