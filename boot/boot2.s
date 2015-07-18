	.globl	start
	.code16

	.set STACK_TOP,0x7c00
	.set ORG_TARG,0x600
	.set GDT_SIZE,0x20	# 4 8-byte entries
	.set TSS_SPC,0x8020
	.set TSS_SZ,0x65
	.set IDT_SPC,0x8085
	.set IDT_SZ,0x190
	.set VIDT_SPC,0x9000
	.set VIDT_SZ,0x10*0x4	# 32 bit pointers
	.set BASE_INT,0x20*0x8
	.set NUM_INT,0x10

	.set DATA_SEL,0x10
	.set TSS_SEL,0x18

	.set VIDEO_BASE,0xb8000

start:
	# interrupts are for chumps
	cli
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
	# TODO: interrupt handlers
	# set protected mode
	mov	%cr0,%eax
	orb	$0x1,%al
	mov	%eax,%cr0
	# set up the TSS
	movw	$TSS_SPC,%di
	callw	createtss
	# jump to 32 bit code
	ljmp	$0x8,$main

mappic:
	# save regsiers
	pushw	%ax
	pushw	%bx
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
	# restore registers
	popw	%bx
	popw	%ax
	retw

seta20:
	pushw	%ax
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
	movw	$0x08,0x2(%di)
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
	.word	TSS_SZ+TSS_SPC	# 100 byte limit
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

	.code32

# 32 bit main
main:
	# set up the 32 bit stack
	xorw	%ecx,%ecx
	movb	$DATA_SEL,%cl
	movw	%cx,%ss
	# set up the TSS
	movw	$TSS_SEL,%ax
	ltr	%ax
	# DEBUG
	mov	$isr_flag,%eax
	movb	$0x0,%eax
	# enable interrupts
	#sti
main.0:
	mov	$isr_flag,%eax
	movb	(%eax),%al
	testb	%al,%al
	je	main.1
	# DEBUG
	movl	$0x58,%eax
	call	prputchr
	mov	$isr_flag,%eax
	movb	$0x0,(%eax)
main.1:
	jmp	main.0		# DEBUG

# accepts the interrupt number in %eax
default_isr:
	# save registers
	pusha
	# just gtfo
	# TODO: something useful
	# restore registers
	popa
	iret

# prints the character in %eax
prputchr:
	# save registers
	pushl	%edi
	# put the character into video memory
	movl	$VIDEO_BASE,%edi
	movb	%al,(%edi)
	movb	$0x7,0x1(%edi)
	# restore registers
	popl	%edi
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


isr_flag:
	.byte	0x0

