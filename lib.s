	.text
	.globl min_caml_create_array
min_caml_create_array:
	addi	sp, sp, -12
	sw	s0, 8(sp)
	mv	s0, sp
	sw	t3, 4(sp)
	sw	t4,	0(sp)
	mv	t5, a0
	mv	a0, t0
	mv	t4, t5
	slli	t4, t4, 2
	add	t0, t0, t4
create_array_loop:
	bne	t5, zero, create_array_cont
create_array_exit:
	lw	t4, 0(sp)
	lw	t5, 4(sp)
	mv	sp, s0
	lw	s0, 8(sp)
	addi	sp, sp, 12
	ret
create_array_cont:
	addi	t5, t5, -1
	slli	t1, t5, 2
	add	t4, t1, a0
	sw	a1, 0(t4)
	j	create_array_loop

	.globl min_caml_create_float_array
min_caml_create_float_array:
	addi	sp, sp, -12
	sw	s0, 8(sp)
	mv	s0, sp
	sw	t3, 4(sp)
	sw	t4,	0(sp)
	mv	t5, a0
	mv	a0, t0
	mv	t4, t5
	slli	t4, t4, 3
	add	t0, t0, t4
create_float_array_loop:
	bne	t5, zero, create_float_array_cont
create_float_array_exit:
	lw	t4, 0(sp)
	lw	t5, 4(sp)
	mv	sp, s0
	lw	s0, 8(sp)
	addi	sp, sp, 12
	ret
create_float_array_cont:
	addi	t5, t5, -1
	slli	t1, t5, 3
	add	t4, t1, a0
	fsw	fa0, 0(t4)
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
	fcvt.w.s	a0, fa0
	ret
	.globl min_caml_floor
min_caml_floor:
	fcvt.w.s	a0, fa0
	ret
