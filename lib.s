	.data
l.one:	# 1.000000
	.word	0x3f800000
l.zero:	# 0.0
	.word	0x00000000
l.ten:	# 10.0
	.word	0x41200000
l.point5:	# 0.5
	.word	0x3f000000
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
	mv	a0, t0
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
	mv	a0, t0
	add	t0, t0, t4
	ret
create_float_array_cont:
	addi	t5, t5, -1
	slli	t6, t5, 2
	add	t6, t6, t0
	fsw	fa0, 0(t6)
	j	create_float_array_loop

	.globl min_caml_fsqr
min_caml_fsqr:
	fmul.s	fa0, fa0, fa0
	ret
	.globl min_caml_sqrt
min_caml_sqrt:
	fsqrt.s	fa0, fa0
	ret
	.globl min_caml_fabs
min_caml_fabs:
	fsgnjx.s	fa0, fa0, fa0
	ret
	.globl min_caml_fless
min_caml_fless:
	flt.s	a0, fa0, fa1
	ret
	.globl min_caml_fhalf
min_caml_fhalf:
	la	t2, l.point5
	flw	ft0, 0(t2)
	fmul.s	fa0, fa0, ft0
	ret
	.globl min_caml_float_of_int
min_caml_float_of_int:
	fcvt.s.w	fa0, a0
	ret
	.globl min_caml_truncate
min_caml_truncate:
	la	t2, l.zero
	flw	ft0, 0(t2)
	flt.s	t2, fa0, ft0
	fsgnjx.s	fa0, fa0, fa0
	sw	ra,	4(s0)
	sw	t2, 8(s0)
	addi	s0, s0, 12
	jal	min_caml_floor
	jal	min_caml_int_of_float
	addi	s0, s0, -12
	lw	t2, 8(s0)
	lw	ra,	4(s0)
	beq	t2, zero, truncate_end
	sub	a0, zero, a0
truncate_end:
	ret
	.globl min_caml_int_of_float
min_caml_int_of_float:
	la	t2, l.zero
	flw	ft0, 0(t2)
	flt.s	t2, fa0, ft0
	fsgnjx.s	ft1, fa0, fa0
	la	t6, l.ftoi_cmp
	flw	ft0, 0(t6)
	li	t4, 1258291200
	flt.s	t3, ft1, ft0
	beq	t3, zero, ftoi_else
	fadd.s	ft1, ft1, ft0
	fmv.w.s	a0, ft1
	sub	a0, a0, t4
	beq	t2, zero, ftoi_end
	sub	a0, zero, a0
ftoi_end:
	ret
ftoi_else:
	li	t5, 0
ftoi_cont:
	flt.s	t3, ft1, ft0
	bne	t3, zero, ftoi_sum
	fsub.s	ft1, ft1, ft0
	addi	t5, t5, 1
	j	ftoi_cont
ftoi_sum:
	fadd.s	ft1, ft1, ft0
	fmv.w.s	a0, ft1
	sub	a0, a0, t4
	li	t4, 8388608
ftoi_loop:
	bne	t5, zero, ftoi_sum_cont
	beq	t2, zero, ftoi_end
	sub	a0, zero, a0
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

	.globl min_caml_fiszero
min_caml_fiszero:
	la	t2, l.zero
	flw	ft0, 0(t2)
	feq.s	a0, fa0, ft0
	ret

	.globl min_caml_fispos
min_caml_fispos:
	la	t2, l.zero
	flw	ft0, 0(t2)
	flt.s	a0, ft0, fa0
	ret

	.globl min_caml_fisneg
min_caml_fisneg:
	la	t2, l.zero
	flw	ft0, 0(t2)
	flt.s	a0, fa0, ft0
	ret

	.globl min_caml_fneg
min_caml_fneg:
	fsgnjn.s	fa0, fa0, fa0
	ret
