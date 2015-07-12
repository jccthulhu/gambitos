	.globl	start
	.code16

	.set STACK_TOP,0x7c00
	.set ORG_TARG,0x600
	.set GDT_SIZE,0x20	# 4 8-byte entries
	.set TSS_SPC,0x8020
	.set TSS_SZ,0x65
	.set IDT_SPC,0x8085
	.set IDT_SZ,0x800
	.set VIDT_SPC,0x9000
	.set VIDT_SZ,0x10*0x4	# 32 bit pointers

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
	# clear space for the IDT
	movw	$IDT_SPC,%di
	movw	$IDT_SZ,%cx
	shr	$0x1,%cx
start.0:
	movw	$0x0,(%di)
	loop	start.0
	# create an abstraction over the IDT, call it VIDT
	# fill in VIDT with default ISRs
	movw	$VIDT_SPC,%di
	movw	$VIDT_SZ,%cx
start.1:
	movw	$default_isr,(%di)
	loop	start.1
	# fill in the IDT with permanent entries
	movw	$IDT_SPC,%di
	movw	isr_array,%si
	movw	$VIDT_SZ,%cx
	shr	$0x2,%cx	# 4 bytes per entry
start.2:
	movw	(%si),%ax	# load the isr target
	callw	installisr	# subcontract out
	addw	$0x8,%di	# increment the IDT index
	addw	$0x2,%si	# increment the isr target pointer
	loop	start.2
	# set it as the IDT
	lidt	idtdesc
	# reprogram the PIC
	movw	$2820,%ax
	callw	mappic
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

mappic:
	in	$0x21,%al
	pushw	%ax
	in	$0xa1,%al
	pushw	%ax
	movb	$0x11,%al
	outb	%al,$0x20
	outb	%al,$0xa0
	movb	%bl,%al
	outb	%al,$0x21
	movb	%bh,%al
	outb	%al,$0xa1
	movb	$0x4,%al
	outb	%al,$0x21
	movb	$0x2,%al
	outb	%al,$0xa1
	movb	$0x1,%al
	outb	%al,$0x21
	outb	%al,$0xa1
	popw	%ax
	outb	%al,$0xa1
	popw	%ax
	outb	%al,$0x21
	retw


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

# installs the isr pointed to by %ax in the IDT pointed to by %di
installisr:
	# save registers
	pushw	%ax
	# write the offset
	movw	%ax,(%di)
	movw	$0x0,0x6(%di)
	# write the code selector (any code segment, really)
	movw	$0x10,0x2(%di)
	# write zero
	movb	$0x0,0x4(%di)
	# write type and attributes
	xorb	%al,%al
	orb	$0x80,%al	# set present
	orb	$0x60,%al	# set descriptor privilege level
	andb	$0xef,%al	# clear storage segment
	orb	$0x0e,%al	# 386 interrupt gate
	movb	%al,0x5(%di)
	# restore registers
	popw	%ax
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
	.word	gdt		# endianness swap
	.word	0x0

idtdesc:
	.word	IDT_SZ-1
	.word	IDT_SPC
	.word	0x0

	.code32

# accepts the interrupt number in %eax
default_isr:
	# save registers
	pusha
	# just gtfo
	# TODO: something useful
	# restore registers
	popa
	ret

# isr gates

isr_0:
	pushl	$0x0
	jmp	isr_gate
isr_1:
	pushl	$0x1
	jmp	isr_gate
isr_2:
	pushl	$0x2
	jmp	isr_gate
isr_3:
	pushl	$0x3
	jmp	isr_gate
isr_4:
	pushl	$0x4
	jmp	isr_gate
isr_5:
	pushl	$0x5
	jmp	isr_gate
isr_6:
	pushl	$0x6
	jmp	isr_gate
isr_7:
	pushl	$0x7
	jmp	isr_gate
isr_8:
	pushl	$0x8
	jmp	isr_gate
isr_9:
	pushl	$0x9
	jmp	isr_gate
isr_10:
	pushl	$0xa
	jmp	isr_gate
isr_11:
	pushl	$0xb
	jmp	isr_gate
isr_12:
	pushl	$0xc
	jmp	isr_gate
isr_13:
	pushl	$0xd
	jmp	isr_gate
isr_14:
	pushl	$0xe
	jmp	isr_gate
isr_15:
	pushl	$0xf

isr_gate:
	# save registers, just in case
	pushal
	# look up the actual interrupt handler from the VIDT
	mov	$VIDT_SPC,%edi
	mov	(%edi,%eax,4),%edi
	# call it
	call	*%edi
	# restore registers
	popal
	ret

isr_array:
	.word	isr_0
	.word	isr_1
	.word	isr_2
	.word	isr_3
	.word	isr_4
	.word	isr_5
	.word	isr_6
	.word	isr_7
	.word	isr_8
	.word	isr_9
	.word	isr_10
	.word	isr_11
	.word	isr_12
	.word	isr_13
	.word	isr_14
	.word	isr_15


