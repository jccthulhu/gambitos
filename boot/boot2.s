	.globl	start
	.code16

	.set STACK_TOP,0x7c00
	.set ORG_TARG,0x600
	.set GDT_SIZE,0x20	# 4 8-byte entries
	.set TSS_SPC,0x8020
	.set TSS_SZ,0x65

start:
	# string ops increment
	cld
	# clear segments
	xorw	%ax,%ax
	movw	%ax,%es
	movw	%ax,%ds
	movw	%ax,%ss
	# set up the stack
	movw	$STACK_TOP,%sp
	# DEBUG
	movb	$0x42,%al
	callw	putchr
	# enable more memory than anyone would ever need
	callw	seta20
	# set the GDT
	lgdt	gdtdesc
	# set up the IDT
	
	# set protected mode
	# set up the TSS
	movw	$TSS_SPC,%di
	callw	createtss
	movw	$TSS_SPC,%ax
	ltr	%ax
	# jump to 32 bit code
	# DEBUG
	movb	$0x43,%al
	callw	putchr
	jmp	.

# create the TSS in the memory pointed to by %di
createtss:
	# save registers
	# write the stack segment
	movw	$0x10,0x8(%di)
	# write the kernel stack base
	movw	$STACK_TOP,0x4(%di)
	# write the io bitmap
	movw	$TSS_SZ,0x66(%di)
	# restore registers
	retw

seta20:
	pushw	%ax
	cli	# disable interrupts
seta20.1:
	inb	$0x64,%al	# get status
	testb	$0x2,%al	# busy?
	jnz	seta20.1	# yes
	movb	$0xd1,%al	# commands: write
	outb	%al,$0x64	#	output port
seta20.2:
	inb	$0x64,%al	# get status
	testb	$0x2,%al	# busy?
	jnz	seta20.2	# yes
	movb	$0xdf,%al	# enable
	outb	%al,$0x60	# 	A20
	sti			# enable interrupts
	popw	%ax
	retw

# prints character in %al
putchr:
	# save registers
	pushw	%bx
	pushw	%ax
	movw	$0x7,%bx
	movb	$0xe,%ah
	int	$0x10
	# TODO: error handling
	# restore registers
	popw	%ax
	popw	%bx
	retw

gdt:
	# TODO: entries that we can live with
	# null entry
	.word	0x0	# zero limit
	.word	0x0	# zero base
	.byte	0x0	# zero base some more
	.byte	0x10	# access byte
	.byte	0xc0	# flags:limit
	.byte	0x0	# zero base
	# kernel code segment
	.word	0xffff	# high limit	
	.word	0x0	# zero base
	.byte	0x0	# zero base some more
	.byte	0x9a	# access byte
	.byte	0xcf	# flags:high limit
	.byte	0x0	# zero base some more
	# kernel data segment
	.word	0xffff	# high limit
	.word	0x0	# zero base
	.byte	0x0	# zero base some more
	.byte	0x92	# access byte
	.byte	0xcf	# flags:high limit
	.byte	0x0	# revenge of the zero base
	# TSS segment
	.word	TSS_SZ	# 100 byte limit
	.word	TSS_SPC	# base
	.byte	0x0	# zero the rest of the base
	.byte	0x89	# access byte
	.byte	0x40	# flags:low limit
	.byte	0x0	# zero the rest of the base

gdtdesc:
	.word	GDT_SIZE-1
	.word	0x0
	.word	gdt

