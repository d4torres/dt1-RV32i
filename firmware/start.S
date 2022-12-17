// This is free and unencumbered software released into the public domain.
//
// Anyone is free to copy, modify, publish, use, compile, sell, or
// distribute this software, either in source code form or as a compiled
// binary, for any purpose, commercial or non-commercial, and by any
// means.
.section .text
.globl _start
.globl uart_putchar

_start:
	/* zero-initialize all registers */

	addi x1, zero, 0
	addi x2, zero, 0
	addi x3, zero, 0
	......
	addi x31, zero, 0
#	j _start

#  define TEST(n) \
	.global n; \
	addi x1, zero, 1000; \
	jal zero,n; \
	.global n ## _ret; \
	n ## _ret:

	#picorv32_timer_insn(zero, x1); \

	TEST(lui)
	TEST(auipc)
	TEST(j)
	TEST(jal)
	TEST(jalr)
	TEST(beq)
	TEST(bne)
	TEST(blt)
	TEST(bge)
	TEST(bltu)
	TEST(bgeu)
	TEST(lb)
	TEST(lh)
	TEST(lw)
	TEST(lbu)
	TEST(lhu)
	TEST(sb)
	TEST(sh)
	TEST(sw)
	TEST(addi)
	TEST(slti)
	TEST(xori)
	TEST(ori)
	TEST(andi)
	TEST(slli)
	TEST(srli)
	TEST(srai)
	TEST(add)
	TEST(sub)
	TEST(sll)
	TEST(slt)
	TEST(xor)
	TEST(srl)
	TEST(sra)
	TEST(or)
	TEST(and)

# void uart_putchar(char c)
uart_putchar:
	la t2, 0x93000000
1:
    lw t0, 8(t2)
    beq t0, zero, 1b
    sw a0, 0(t2) # do the real work
2:
    lw t0, 8(t2)
    beq t0, zero, 2b
	ret