	.data
l.dpi:	# 6.28318548203
	.word 0x40c90fdb
l.pi:	# 3.1415927410125732421875
	.word	0x40490fdb
l.qpi:	# 0.785398
	.word	0x3f490fd8
l.hpi:	# 1.570796
	.word	0x3fc90fd8
l.four:	# 4.000000
	.word	0x40800000
l.two:	# 2.000000
	.word	0x40000000
l.one:	# 1.000000
	.word	0x3f800000
l.sin_3:	# -0.16666668
	.word	0xbe2aaaac
l.sin_5:	# 0.008332824
	.word	0x3c088666
l.sin_7:	# -0.00019587841
	.word	0xb94d64b6
l.cos_2:	# -0.5
	.word	0xbf000000
l.cos_4:	# 0.04166368
	.word	0x3d2aa789
l.cos_6:	# -0.0013695068
	.word	0xbab38106
l.atan_kernel.3:	# -0.3333333
	.word	0xbeaaaaaa
l.atan_kernel.5:	# 0.2
	.word	0x3e4ccccd
l.atan_kernel.7:	# -0.142857142
	.word	0xbe124925
l.atan_kernel.9:	# 0.111111104
	.word	0x3de38e38
l.atan_kernel.11:	# -0.08976446
	.word	0xbdb7d66e
l.atan_kernel.13:	#  0.060035485
	.word	0x3d75e7c5
l.atan_cmp.1:	# 0.4375
	.word	0x3ee00000
l.atan_cmp.2:	# 2.4375
	.word	0x401c0000

	.text
reduction:
	la	t6, l.dpi
	flw	ft1, 0(t6)
	fsgnj.s	ft3, ft1, ft1
	la	t6, l.two
	flw	ft2, 0(t6)
red_cont:
	fle.s	t3, ft1, fa0
	beq	t3, zero, red_break
	fmul.s	ft1, ft1, ft2
	j	red_cont
red_break:
	fle.s	t3, ft3, fa0
	beq	t3, zero, red_ret
	fle.s	t3, ft1, fa0
	beq	t3, zero, red_else
	fsub.s	fa0, fa0, ft1
red_else:
	fdiv.s	ft1, ft1, ft2
	j	red_break
red_ret:
	ret

kernel_sin:
	fsgnj.s	ft0, fa0, fa0
	fmul.s	ft1, fa0, fa0
	la	t2, l.sin_3
	fmul.s	ft0, ft0, ft1
	flw	ft2, 0(t2)
	fmul.s	ft2, ft2, ft0
	fadd.s	fa0, fa0, ft2
	la	t2, l.sin_5
	fmul.s	ft0, ft0, ft1
	flw	ft2, 0(t2)
	fmul.s	ft2, ft2, ft0
	fadd.s	fa0, fa0, ft2
	la	t2, l.sin_7
	fmul.s	ft0, ft0, ft1
	flw	ft2, 0(t2)
	fmul.s	ft2, ft2, ft0
	fadd.s	fa0, fa0, ft2
	fsgnj.s	fa0, fa0, ft4
	ret
kernel_cos:
	fmul.s	ft1, fa0, fa0
	fsgnj.s	ft0, ft1, ft1
	la	t6, l.one
	flw	fa0, 0(t6)
	la	t6, l.cos_2
	flw	ft2, 0(t6)
	fmul.s	ft2, ft2, ft0
	fadd.s	fa0, fa0, ft2
	la	t6, l.cos_4
	flw	ft2, 0(t6)
	fmul.s	ft2, ft2, ft0
	fadd.s	fa0, fa0, ft2
	la	t6, l.cos_6
	flw	ft2, 0(t6)
	fmul.s	ft2, ft2, ft0
	fadd.s	fa0, fa0, ft2
	fsgnj.s	fa0, fa0, ft4
	ret

	.globl min_caml_sin
min_caml_sin:
	fsgnj.s	ft4, fa0, fa0
	fsgnjx.s	fa0, fa0, fa0
	sw	ra, 8(s0)
	addi	s0, s0, 12
	jal	reduction
	addi	s0, s0, -12
	lw	ra, 8(s0)
	la	t6, l.pi
	flw	ft0, 0(t6)
	fle.s	t6, ft0, fa0
	beq	t6, zero, sin_else1
	fsub.s	fa0, fa0, ft0
	fsgnjn.s	ft4, ft4, ft4
sin_else1:
	la	t6, l.hpi
	flw	ft1, 0(t6)
	fle.s	t3, ft1, fa0
	beq	t3, zero, sin_else2
	fsub.s	fa0, ft0, fa0
sin_else2:
	la t6, l.qpi
	flw	ft2, 0(t6)
	fle.s	t3, fa0, ft2
	beq	t3, zero, sin_else3
	j	kernel_sin
sin_else3:
	fsub.s	fa0, ft1, fa0
	j kernel_cos

	.globl min_caml_cos
min_caml_cos:
	fsgnjx.s	fa0, fa0, fa0
	fsgnj.s	ft4, fa0, fa0
	sw	ra, 8(s0)
	addi	s0, s0, 12
	jal	reduction
	addi	s0, s0, -12
	lw	ra, 8(s0)
	la	t6, l.pi
	flw	ft0, 0(t6)
	fle.s	t3, ft0, fa0
	beq	t3, zero, cos_else1
	fsub.s	fa0, fa0, ft0
	fsgnjn.s	ft4, ft4, ft4
cos_else1:
	la	t6, l.hpi
	flw	ft1, 0(t6)
	fle.s	t3, ft1, fa0
	beq	t3, zero, cos_else2
	fsub.s	fa0, ft0, fa0
	fsgnjn.s	ft4, ft4, ft4
cos_else2:
	la	t6, l.qpi
	flw	ft2, 0(t6)
	fle.s	t3, fa0, ft2
	beq	t3, zero, cos_else3
	j	kernel_cos
cos_else3:
	fsub.s	fa0, ft1, fa0
	j	kernel_sin

kernel_atan:
	fsgnj.s	ft0, fa0, fa0
	fmul.s	ft1, fa0, fa0
	la	t2, l.atan_kernel.3
	fmul.s	ft0, ft0, ft1
	flw	ft2, 0(t2)
	fmul.s	ft2, ft2, ft0
	fadd.s	fa0, fa0, ft2
	la	t2, l.atan_kernel.5
	fmul.s	ft0, ft0, ft1
	flw	ft2, 0(t2)
	fmul.s	ft2, ft2, ft0
	fadd.s	fa0, fa0, ft2
	la	t2, l.atan_kernel.7
	fmul.s	ft0, ft0, ft1
	flw	ft2, 0(t2)
	fmul.s	ft2, ft2, ft0
	fadd.s	fa0, fa0, ft2
	la	t2, l.atan_kernel.9
	fmul.s	ft0, ft0, ft1
	flw	ft2, 0(t2)
	fmul.s	ft2, ft2, ft0
	fadd.s	fa0, fa0, ft2
	la	t2, l.atan_kernel.11
	fmul.s	ft0, ft0, ft1
	flw	ft2, 0(t2)
	fmul.s	ft2, ft2, ft0
	fadd.s	fa0, fa0, ft2
	la	t2, l.atan_kernel.13
	fmul.s	ft0, ft0, ft1
	flw	ft2, 0(t2)
	fmul.s	ft2, ft2, ft0
	fadd.s	fa0, fa0, ft2
	ret

	.globl min_caml_aton
min_caml_atan:
	fsgnj.s	ft4, fa0, fa0
	fsgnjx.s	ft3, fa0, fa0
	la	t2, l.atan_cmp.1
	flw	ft1, 0(t2)
	flt.s	t2, ft3, ft1
	beq	t2, zero, atan_else.1
	j	kernel_atan
atan_else.1:
	la	t2, l.atan_cmp.2
	flw	ft1, 0(t2)
	flt.s	t2, ft3, ft1
	beq	t2, zero, atan_else.2
	la	t3, l.one
	flw	ft2, 0(t3)
	fsub.s	fa0, ft3, ft2
	fadd.s	ft2, ft3, ft2
	fdiv.s	fa0, fa0, ft2
	sw	ra, 4(s0)
	addi	s0, s0, 8
	jal	kernel_atan
	addi	s0, s0, -8
	lw	ra, 4(s0)
	la	t2, l.qpi
	flw	ft1, 0(t2)
	fadd.s	fa0, fa0, ft1
	fsgnj.s	fa0, fa0, ft4
	ret
atan_else.2:
	la	t3, l.one
	flw	ft2, 0(t3)
	fdiv.s	fa0, ft2, ft3
	sw	ra, 4(s0)
	addi	s0, s0, 8
	jal	kernel_atan
	addi	s0, s0, -8
	lw	ra, 4(s0)
	la	t2, l.hpi
	flw	ft1, 0(t2)
	fsub.s	fa0, ft1, fa0
	fsgnj.s	fa0, fa0, ft4
	ret
