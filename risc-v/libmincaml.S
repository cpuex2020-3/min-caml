#if defined(__CYGWIN__) || defined(__MACH__)
#define U(x) _##x
#else
#define U(x) x
#endif
#if defined(__MACH__)
#define ALIGNSTACK0 andl $0xfffffff0, %esp
#define ALIGNSTACK1 andl $0xfffffff0, %esp; pushl %eax; pushl %eax; pushl %eax
#define ALIGNSTACK2 andl $0xfffffff0, %esp; pushl %eax; pushl %eax
#define ALIGNSTACK3 andl $0xfffffff0, %esp; pushl %eax
#else
#define ALIGNSTACK0
#define ALIGNSTACK1
#define ALIGNSTACK2
#define ALIGNSTACK3
#endif
.text
.globl min_caml_print_int
min_caml_print_int:
	addi	sp, sp, -8
	sw	ra, 0(sp)
	sw	a1, 4(sp)
	sw	a0, 8(sp)
	mv	a1, a0
	la	a0, format_int
	call	printf
	lw	a0, 8(sp)
	lw	a1, 4(sp)
	lw	ra, 0(sp)
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
