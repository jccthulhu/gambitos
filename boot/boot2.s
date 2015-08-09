	.globl	start
	.code16

	.set STACK_TOP,0x7c00	# stack origin
	.set GDT_SIZE,0x20	# size of the 32 bit global descriptor table
	.set TSS_SZ,0x65	# size of the TSS
	.set TSS64_SZ,0x65	# size of the long mode TSS
	.set IDT_SZ,0x300	# size of the IDT
	.set VIDT_SZ,0x280	# size of the IDT abstraction

	.set CODE_SEL,0x8	# Code segment index in the GDT
	.set DATA_SEL,0x10	# Data segment index in the GDT
	.set TSS_SEL,0x18	# TSS segment index in the (32 bit) GDT

	.set VIDEO_BASE,0xb8000	# the start of video memory (for printing to screen)

	.set PT_SZ,0x1000
	.set PML4T,0x10000	# the 4th level of the paging hierarchy
	.set PDPT,PML4T+PT_SZ	# the 3rd level of the paging hierarchy
	.set PDT,PDPT+PT_SZ	# the 2nd level of the paging hierarchy
	.set PT,PDT+PT_SZ	# the page table (1st level)
	.set VIDT,PT+PT_SZ	# virtual interrupt table
	.set SYSTBL,VIDT+VIDT_SZ	# the system call table

	.set GDT64_SZ,0x28	# the size of the 64 bit GDT

	.set NEXT_SEG,0x8c00	# the next stage entry point

start:
	# interrupts are for chumps
	cli			# disable interrupts
	cld			# cause string operations to increment
	# clear segment selectors
	xorw	%ax,%ax
	movw	%ax,%es
	movw	%ax,%ds
	movw	%ax,%ss
	movw	$STACK_TOP,%sp	# set up the stack
	# enable more memory than anyone would ever need
	callw	seta20		# by setting the A20 line, we enable >1MB of memory
	lgdt	gdt64desc		# set the GDT
	# check for CPUID extended
	movl	$0x80000000,%eax
	cpuid
	movl	$0x80000001,%ebx
	cmpl	%eax,%ebx
	jg	start.0

	movl	$0x80000001,%eax
	cpuid
	andl	$0x20000000,%edx
	testl	%edx,%edx
	je	start.1
	
	# set protected mode
	mov	%cr0,%eax	# load control register zero
	orb	$0x1,%al	# set the PM bit
	mov	%eax,%cr0	# write the control register

	# jump to long mode
	call	disable_paging
	call	create_page_tables
	call	enable_pae
	call	enable_lm
	call	enable_paging

	ljmp	$0x8,$main64	# jump to the 32 bit entry point

start.0:
	# Warning, no support for CPUID extended
	mov	$no_cpuid_msg,%ax
	call	putstr
	jmp	start.2
start.1:
	# Warning, no support for long mode
	mov	$no_long_mode_msg,%ax
	call	putstr
	jmp	start.2

start.2:
	# TODO: Either load a more shitty version of the kernel,
	# or just explode
	jmp	.

# utils

# disable paging; no parameters or return value
disable_paging:
	pushl	%eax			# save the value in %eax to the stack
	movl	%cr0,%eax		# load the control register
	andl	$0x7FFFFFFF,%eax	# clear the paging bit
	movl	%eax,%cr0		# write the control register back
	popl	%eax			# restore %eax from the stack
	ret				# exit

# enable paging; no parameters or return value
enable_paging:
	pushl	%eax			# save the value in %eax to the stack
	movl	%cr0,%eax		# load the control register
	orl	$0x80000000,%eax	# set the paging bit
	movl	%eax,%cr0		# write the control register back
	popl	%eax			# restore the value of %eax from the stack
	ret				# exit

# enable physical address extension (PAE); no parameters or return value
enable_pae:
	pushl	%eax			# save value of register onto stack
	movl	%cr4,%eax		# load the control register
	orl	$0x20,%eax		# set PAE bit
	movl	%eax,%cr4		# write the control register
	popl	%eax			# restore value of register from stack
	ret

# enable long mode; no parameters or return value
enable_lm:
	# save register values onto the stack
	pushl	%ecx
	pushl	%eax
	movl	$0xC0000080,%ecx	# select the EFER Moder Specific Register (MSR)
	rdmsr				# read the MSR
	orl	$0x100,%eax		# set the long mode bit
	wrmsr				# save the MSR
	# restore register values from the stack
	popl	%eax
	popl	%ecx
	ret				# exit

create_page_tables:
	# save register values onto the stack
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edi
	# loop to clear the tables
	movl	$PML4T,%edi		# load pointer to the start of the page table construct
	movl	$0x1000,%ecx		# set loop counter to the size of the page table construct in doublewords
create_page_tables.1:
	movl	$0x0,(%edi)		# write zero to the current index of the page table construct
	addl	$0x4,%edi		# increment the page table construct pointer
	loop	create_page_tables.1	# decrement %ecx, check for zero and jump to the start of the loop and continue
	# make the first entry of the PML4T point to the first PDPT
	movl	$PML4T,%edi	# load &PML4T
	movl	$PDPT,%eax	# load &PDPT
	orl	$0x03,%eax	# set PDPT to be Present/Readable/Writable
	movl	%eax,(%edi)	# PML4T[0] = &PDPT
	# make the first entry of the PDPT point to the first PDT
	movl	$PDPT,%edi	# load &PDPT
	movl	$PDT,%eax	# load $PDT
	orl	$0x03,%eax	# set the PDT to be Present/Readable/Writable
	movl	%eax,(%edi)	# PDPT[0] = &PDT
	# make the first entry of the PDT point to the first PT
	movl	$PDT,%edi	# load &PDT
	movl	$PT,%eax	# load &PT
	orl	$0x03,%eax	# set the PT to be Present/Readable/Writable
	movl	%eax,(%edi)	# PDT[0] = &PT
	# identity map the PT
	movl	$PT,%edi	# load &PT
	movl	$0x200,%ecx	# set the loop counter to the size of the Page Table (PT) in quadwords
	movl	$0x03,%ebx	# each page is Present/Readable/Writable
create_page_tables.0:
	movl	%ebx,(%edi)	# write the page address to the page table
	addl	$0x08,%edi	# increment the page table index
	addl	$0x1000,%ebx	# increment the page address
	loop	create_page_tables.0	# jump to the start of the loop and continue
	# set all this shit to be canonical
	movl	$PML4T,%eax	# load &PML4T
	movl	%eax,%cr3	# save PML4T as the location of the PML4T
	# restore register values from the stack
	popl	%edi
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret			# exit this subroutine

# sets the A20 line to enable >1MB of memory
# accepts no parameters, returns no values
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

# print a character to the screen
# params:
#	al	the character (ASCII value)
putchr:
	# save register values to the stack
	pushw	%bx
	pushw	%ax
	movw	$0x7,%bx	# white characters on black background
	movb	$0xe,%ah	# bios call E
	int	$0x10		# trigger the interrupt
	# restore register values from the stack
	popw	%ax
	popw	%bx
	retw

###
# prints a string to the screen in 16 bit mode
# params:
#	ax	the pointer to the string
putstr:
	# save register values to the stack
	mov	%ax,%di		# save the pointer
putstr.0:
	mov	(%di),%al
	testb	%al,%al
	je	putstr.1
	call	putchr
	inc	%di
	jmp	putstr.0
putstr.1:
	# restore register values from the stack
	retw			# return from subroutine

# some data!
no_cpuid_msg:
	.ascii	"Your processor does not support CPUID extended"
	.byte	0x0

no_long_mode_msg:
	.ascii	"Your processor does not support 64 bit mode"
	.byte	0x0

long_mode_full_msg:
	.ascii	"Welcome to full 64 bit mode!"
	.byte	0x0

# 64 bit target
	.code64

main64:
	cli			# no interrupts, please
	# set segment registers
	mov	$DATA_SEL,%ax

	movw	%ax,%ds
	movw	%ax,%es
	movw	%ax,%fs
	movw	%ax,%gs
	
	mov	$STACK_TOP,%rsp			# reset the stack

	# set up the long mode TSS
	call	createtss64
	mov	$TSS_SEL,%rax
	ltr	%ax

	call	prclrscrn64

	mov	$long_mode_full_msg,%rax	# note to the user that we made it to
						#	protected mode
	call	prputstr64			# print that message

	movw	$0x2820,%bx			# remap PIC interrupts
	call	setpic
	movb	$0xfd,%al			# set PIC enable masks
	movb	$0xff,%ah			# only enable keyboard interrupts for now
	call	enablepic
	mov	$idtspc,%rdi
	call	build_idt			# build the IDT
	mov	$VIDT,%rdi
	call	build_vidt			# build the VIDT
	lidt	idtdesc64			# install the IDT
	sti					# enable interrupts

main64.0:
	mov	$NEXT_SEG,%rax
	jmp	*%rax

###
# build the long mode TSS
# params:
#	rdi	pointer to the TSS space
createtss64:
	# save register values onto the stack
	movq	$STACK_TOP,0x4(%rdi)	# write the kernel stack base
	movq	$TSS64_SZ,0x66(%rdi)	# write the IO map offset
	# restore register values from the stack
	ret

# long mode printing stuff

# print a string in long mode
# params:
#	rax	Pointer to the string
prputstr64:
	# save register values to the stack
	pushq	%rax
	pushq	%rdi
	mov	%rax,%rdi	# save the pointer in a pointer register
	xorq	%rax,%rax	# perma-clear the upper 48 bits of %rax
prputstr64.0:
	movb	(%rdi),%al	# load the character being pointed to
	testb	%al,%al		# test the character for zero
	je	prputstr64.1	# if the character is zero, we've reached the end
				# of the string and can exit
	call	prputchr64	# print the character
				# the 32 bit version seems to work in long mode
	inc	%rdi		# increment the string pointer
	jmp	prputstr64.0	# jump to the start of the loop to continue
prputstr64.1:
	# restore register values from the stack
	popq	%rdi
	popq	%rax
	ret			# exit

# print a character in long mode
# params:
#	al	The character to print
prputchr64:
	# save register values to the stack
	push	%rdi
	push	%rsi
	push	%rax
	# put the character into video memory
	mov	$current_video_mem64,%rdi
	mov	(%rdi),%rsi
	movb	%al,(%rsi)
	movb	$0x7,0x1(%rsi)
	# increment the video memory pointer
	add	$0x2,%rsi
	mov	%rsi,(%rdi)
	# increment x
	xor	%rax,%rax
	movb	0x8(%rdi),%al
	inc	%rax
	movq	$0x50,%rsi
	cmp	%rax,%rsi
	jne	prputchr64.0
	xor	%rax,%rax
	movb	%al,0x8(%rdi)
	movb	0x9(%rdi),%al
	inc	%rax
	movb	%al,0x9(%rdi)
	jmp	prputchr64.1
prputchr64.0:
	movb	%al,0x8(%rdi)
prputchr64.1:
	# restore register values from the stack
	pop	%rax
	pop	%rsi
	pop	%rdi
	ret	# exit

###
# prints an integer to the screen
# params:
#	rdi	the integer to print
prputint64:
	# save register values onto the stack
	pushq	%rax
	pushq	%rcx
	pushq	%rdi
	mov	$0x10,%rcx	# loop once for each of 16 nibbles
prputint64.0:
	rol	$0x4,%rdi		# start with the highest nibble
	# isolate the lower nibble
	mov	%rdi,%rax
	and	$0x0f,%rax
	add	$0x30,%rax
	cmp	$0x3A,%rax
	jb	prputint64.1
	add	$0x07,%rax
prputint64.1:
	call	prputchr64
	loop	prputint64.0
	# restore register values from the stack
	popq	%rdi
	popq	%rcx
	popq	%rax
	ret

### clears the screen
# params:
#	none
prclrscrn64:
	# save register values to the stack
	pushq	%rax
	pushq	%rcx
	mov	$0x2500,%rcx
	mov	$VIDEO_BASE,%rax
prclrscrn64.0:
	movq	$0x0,0x0(%rax)
	inc	%rax
	loop	prclrscrn64.0
	mov	$current_video_mem64,%rax
	movq	$VIDEO_BASE,(%rax)
	movw	$0x0,0x4(%rax)
	# restore register values from the stack
	popq	%rcx
	popq	%rax
	ret

# interrupt stuff


# enable certain PIC interrupts
# params:
#	al	PIC1 mask
#	ah	PIC2 mask
enablepic:
	pushq	%rax		# save register values to the stack
	outb	%al,$0x21	# write out the PIC1 mask
	movb	%ah,%al		# shuffle the masks
	outb	%al,$0xa1	# write out the PIC2 mask
	popq	%rax		# restore register values from the stack
	ret			# exit the subroutine

# remap PIC interrupts
# params:
#	bl	starting interrupt number for PIC1
#	bh	starting interrupt number for PIC2
setpic: 	
	pushq	%rax
	inb $0x21,%al			# Save master
	push %rax			#  IMR
	inb $0xa1,%al			# Save slave
	push %rax			#  IMR
	movb $0x11,%al			# ICW1 to
	outb %al,$0x20			#  master,
	outb %al,$0xa0			#  slave
	movb %bl,%al			# ICW2 to
	outb %al,$0x21			#  master
	movb %bh,%al			# ICW2 to
	outb %al,$0xa1			#  slave
	movb $0x4,%al			# ICW3 to
	outb %al,$0x21			#  master
	movb $0x2,%al			# ICW3 to
	outb %al,$0xa1			#  slave
	movb $0x1,%al			# ICW4 to
	outb %al,$0x21			#  master,
	outb %al,$0xa1			#  slave
	pop  %rax				# Restore slave
	outb %al,$0xa1			#  IMR
	pop  %rax				# Restore master
	outb %al,$0x21			#  IMR
	popq	%rax
	ret				# To caller

# constructs a reasonable 'default' Interrupt Descriptor Table (IDT) in long mode
# params:
#	rdi	IDT pointer
build_idt:
	# save register values onto the stack
	pushq	%rax
	pushq	%rcx
	pushq	%rdx
	pushq	%rdi
	pushq	%rsi
	mov	%rdi,%rsi	# shift the IDT pointer because the next function expects
				#	it in rsi (and preserves it thusly)
	# the first 0x13 interrupts are hardware exceptions ('faults');
	#	exceptions should be handled by a default exception handler that pretty much
	#	handles them all the same way
	# this is a loop to install the exception handler for the first 0x13 interrupts
	xor	%rdx,%rdx	# use rdx as the loop counter/interrupt number
	mov	$0x14,%rax	# use rax to do loop counter check
build_idt.0:
	mov	$idtmap,%rdi	# load up the function pointer
	leaq	(%rdi,%rdx,8),%rdi
	mov	(%rdi),%rdi
	# IDT pointer is already in place
	# the interrupt number is already in place
	movb	$0x8e,%cl	# set the type:attributes
				# specifically, set it as present, kernel priv,
				#	32 bit interrupt gate
	call	install_isr	# call the subroutine to install the exception handlee
	inc	%rdx		# increment the loop counter
	cmpq	%rdx,%rax	# check it against 0x14
	jne	build_idt.0	# if the loop counter is 0x14, then we can stop looping
				# otherwise, jump to the start of the loop and continue
	# interrupts 20-30 are user interrupts; these should all be handled pretty much the same
	# this is a loop to install the user interrupt handler for interrupts 0x20-0x30
	mov	$0x20,%rdx	# use rdx as the loop counter/interrupt number
	mov	$0x31,%rax	# use rax to do loop counter check
build_idt.1:
	# load up the function pointer
	mov	$idtmap,%rdi
	leaq	(%rdi,%rdx,8),%rdi
	mov	(%rdi),%rdi
	# IDT pointer is already in place
	# the interrupt number is already in place
	movb	$0x8e,%cl	# set the type:attributes
				# specifically, set it as present, kernel priv,
				#	32 bit interrupt gate
	call	install_isr	# call the subroutine to install the handler
	inc	%rdx		# increment the loop counter
	cmpq	%rdx,%rax	# check it against 0x31
	jne	build_idt.1	# if the loop counter is 31, then we can stop looping
				# otherwise, jump to the start of the loop and continue
	# install the system call gate for int 0x31
	mov	$syscall_gate,%rdi	# load the function pointer
	# IDT pointer is already in place
	# the interrupt number is already in place
	movb	$0x8e,%cl	# set the type:attributes
				# specifically, set it as present, kernel priv,
				#	64 bit interrupt gate
	call	install_isr
	# restore register values from the stack
	popq	%rsi
	popq	%rdi
	popq	%rdx
	popq	%rcx
	popq	%rax
	ret			# exit this function

###
# subroutine that constructs the Virtual IDT (VIDT)
# params:
#	rdi	pointer to the VIDT
build_vidt:
	# save register values onto the stack
	pushq	%rcx
	pushq	%rdi
	# first, fill it in with the default handler
	mov	$VIDT_SZ,%rcx
build_vidt.0:
	movq	$default_handler,-8(%rdi,%rcx,8)
	loop	build_vidt.0
	# then fill in specific handlers that we have
	movq	$keyboard_handler,0x108(%rdi)
	# restore register values from the stack
	popq	%rdi
	popq	%rcx
	ret

# install specified ISR for specified interrupt into specified IDT
# params:
#	rdi	ISR function pointer
#	rsi	IDT pointer
#	rdx	interrupt number
#	cl	type:attributes byte
install_isr:
	# save register values onto the stack
	pushq	%rdx
	pushq	%rdi
	pushq	%rsi
	shl	$0x4,%rdx		# get the offset into the IDT
	add	%rdx,%rsi		# offset = interrupt number * 0x10 + IDT
	movw	%di,(%rsi)		# write the low word of the function pointer
	movw	$CODE_SEL,0x2(%rsi)	# write the code segment selector
	movb	$0x0,0x4(%rsi)		# write zero (1 byte)
	movb	%cl,0x5(%rsi)		# write the type:attributes byte
	shr	$0x10,%rdi		# write the next word
	movw	%di,0x6(%rsi)		#	of the function pointer
	shr	$0x10,%rdi		# write the upper doubleword
	movl	%edi,0x8(%rsi)		#	of the function pointer
	movl	$0x0,0xc(%rsi)		# write zero (4 bytes)
	# restore register values from the stack
	popq	%rsi
	popq	%rdi
	popq	%rdx
	ret

isr_0:
	pushq	$0x0
	pushq	$0x0
	jmp	isr_gate
isr_1:
	pushq	$0x0
	pushq	$0x1
	jmp	isr_gate
isr_2:
	pushq	$0x0
	pushq	$0x2
	jmp	isr_gate
isr_3:
	pushq	$0x0
	pushq	$0x3
	jmp	isr_gate
isr_4:
	pushq	$0x0
	pushq	$0x4
	jmp	isr_gate
isr_5:
	pushq	$0x0
	pushq	$0x5
	jmp	isr_gate
isr_6:
	pushq	$0x0
	pushq	$0x6
	jmp	isr_gate
isr_7:
	pushq	$0x0
	pushq	$0x7
	jmp	isr_gate
isr_8:
	pushq	$0x8
	jmp	isr_gate
isr_9:
	pushq	$0x0
	pushq	$0x9
	jmp	isr_gate
isr_10:
	pushq	$0xA
	jmp	isr_gate
isr_11:
	pushq	$0xB
	jmp	isr_gate
isr_12:
	pushq	$0xC
	jmp	isr_gate
isr_13:
	pushq	$0xD
	jmp	isr_gate
isr_14:
	pushq	$0xE
	jmp	isr_gate
isr_15:
	pushq	$0x0
	pushq	$0xF
	jmp	isr_gate
isr_16:
	pushq	$0x0
	pushq	$0x10
	jmp	isr_gate
isr_17:
	pushq	$0x11
	jmp	isr_gate
isr_18:
	pushq	$0x12
	jmp	isr_gate
isr_19:
	pushq	$0x13
	jmp	isr_gate

isr_20:
	pushq	$0x0
	pushq	$0x14
	jmp	isr_gate

isr_21:
	pushq	$0x0
	pushq	$0x15
	jmp	isr_gate

isr_22:
	pushq	$0x0
	pushq	$0x16
	jmp	isr_gate

isr_23:
	pushq	$0x0
	pushq	$0x17
	jmp	isr_gate

isr_24:
	pushq	$0x0
	pushq	$0x18
	jmp	isr_gate

isr_25:
	pushq	$0x0
	pushq	$0x19
	jmp	isr_gate

isr_26:
	pushq	$0x0
	pushq	$0x1a
	jmp	isr_gate

isr_27:
	pushq	$0x0
	pushq	$0x1b
	jmp	isr_gate

isr_28:
	pushq	$0x0
	pushq	$0x1c
	jmp	isr_gate

isr_29:
	pushq	$0x0
	pushq	$0x1d
	jmp	isr_gate

isr_30:
	pushq	$0x0
	pushq	$0x1e
	jmp	isr_gate

isr_31:
	pushq	$0x0
	pushq	$0x1f
	jmp	isr_gate

isr_32:
	pushq	$0x0
	pushq	$0x20
	jmp	isr_gate

isr_33:
	pushq	$0x0
	pushq	$0x21
	jmp	isr_gate

isr_34:
	pushq	$0x0
	pushq	$0x22
	jmp	isr_gate

isr_35:
	pushq	$0x0
	pushq	$0x23
	jmp	isr_gate

isr_36:
	pushq	$0x0
	pushq	$0x24
	jmp	isr_gate

isr_37:
	pushq	$0x0
	pushq	$0x25
	jmp	isr_gate

isr_38:
	pushq	$0x0
	pushq	$0x26
	jmp	isr_gate

isr_39:
	pushq	$0x0
	pushq	$0x27
	jmp	isr_gate

isr_40:
	pushq	$0x0
	pushq	$0x28
	jmp	isr_gate

isr_41:
	pushq	$0x0
	pushq	$0x29
	jmp	isr_gate

isr_42:
	pushq	$0x0
	pushq	$0x2a
	jmp	isr_gate

isr_43:
	pushq	$0x0
	pushq	$0x2b
	jmp	isr_gate

isr_44:
	pushq	$0x0
	pushq	$0x2c
	jmp	isr_gate

isr_45:
	pushq	$0x0
	pushq	$0x2d
	jmp	isr_gate

isr_46:
	pushq	$0x0
	pushq	$0x2e
	jmp	isr_gate

isr_47:
	pushq	$0x0
	pushq	$0x2f
	jmp	isr_gate

isr_48:
	pushq	$0x0
	pushq	$0x30
	jmp	isr_gate

### 
# ISR basis; all interrupts go through this subroutine, which will
# set up and make the call to the actual interrupt handlers appropriately
isr_gate:
	# save all the general purpose register values to the stack
	# note: pusha is apparently not supported in long mode
	pushq	%rax	# 8
	pushq	%rbx	# 10
	pushq	%rcx	# 18
	pushq	%rdx	# 20
	pushq	%rdi	# 28
	pushq	%rsi	# 30

	pushq	%r8	# 38
	pushq	%r9	# 40
	pushq	%r10	# 48
	pushq	%r11	# 50
	pushq	%r12	# 58
	pushq	%r13	# 60
	pushq	%r14	# 68
	pushq	%r15	# 70

	pushq	%rbp	# 78

	# load the error code and interrupt vector
	mov	0x78(%rsp),%rdi
	mov	0x80(%rsp),%rsi
	# look up the handler
	mov	$VIDT,%rax
	movq	(%rax,%rdi,8),%rax
	# call it
	call	*%rax

	# acknowledge that we handled this interrupt
	movb	$0x20,%al
	outb	%al,$0x20

	# TODO: acknowledge PIC 2 interrupts

	# restore all the general purpose register values from the stack

	popq	%rbp

	popq	%r15	
	popq	%r14	
	popq	%r13	
	popq	%r12	
	popq	%r11	
	popq	%r10	
	popq	%r9	
	popq	%r8	

	popq	%rsi
	popq	%rdi
	popq	%rdx
	popq	%rcx
	popq	%rbx
	popq	%rax
	addq	$0x8,%rsp	# drop interrupt number
	addq	$0x8,%rsp	# drop error code
	iretq			# return in an extra special, interrupt-y kinda way


###
# system call redirection handler
# params:
#	rdi	system call number
syscall_gate:
	# save registers as per the System V ABI
	pushq	%rbp
	pushq	%rbx
	pushq	%r12
	pushq	%r13
	pushq	%r14
	pushq	%r15

	mov	$SYSTBL,%rbx		# load the pointer to the syscall table
	mov	(%rbx,%rax,8),%rax	# index into it to get the appropriate syscall handler
	call	*%rax			# call the handler

	# return value is in rax; that's how that works

	# restore registers as per the System V ABI
	popq	%r15
	popq	%r14
	popq	%r13
	popq	%r12
	popq	%rbx
	popq	%rbp
	iret		# return in an extra special, interrupt-y kinda way

###
# default interrupt service routine
# params:
#	rdi	interrupt number
#	rsi	error code
default_handler:
	ret

###
# keyboard interrupt handler
# params:
#	rdi	interrupt number
#	rsi	error code
keyboard_handler:
	# save register values
	pushq	%rax
	pushq	%rdi
	pushq	%rsi
	# read the keyboard data port
	inb	$0x64,%al
	andb	$0x01,%al
	testb	%al,%al
	je	keyboard_handler.0
	# there is data to be read!
	xor	%rax,%rax
	inb	$0x60,%al
	mov	%rax,%rdi
	call	prputint64
	mov	$0x2c,%rax
	call	prputchr64
keyboard_handler.0:
	# restore register values
	popq	%rsi
	popq	%rdi
	popq	%rax
	ret

	.align	0x8

current_video_mem64:
	.quad	VIDEO_BASE
	.byte	0x0	# x
	.byte	0x0	# y

gdt64:
	# null entry
	.word	0x0
	.word	0x0
	.byte	0x0
	.byte	0x10
	.byte	0x20
	.byte	0x0
	# code
	.word	0x0
	.word	0x0
	.byte	0x0
	.byte	0x98
	.byte	0x20
	.byte	0x0
	# data
	.word	0x0
	.word	0x0
	.byte	0x0
	.byte	0x92
	.byte	0x20
	.byte	0x0
	# TSS segment
	.word	tssspc64+TSS64_SZ	# limit[0:15]
	.word	tssspc64		# base[0:15]
	.byte	0x0			# base[16:23]
	.byte	0x89			# P:DPL:0:Type
	.byte	0x0			# flags:limit[16:19]
	.byte	0x0			# base[24:31]
	.long	0x0			# base[32:63]
	.long	0x0			# reserved

gdt64desc:
	.word	GDT64_SZ-1
	.quad	gdt64

idtdesc64:
	.word	IDT_SZ-1
	.quad	idtspc


# make space for the IDT
idtspc:
.fill	IDT_SZ,0x1,0x0
	
tssspc64:
.fill	TSS64_SZ,0x1,0x0

idtmap:
	.quad	isr_0
	.quad	isr_1
	.quad	isr_2
	.quad	isr_3
	.quad	isr_4
	.quad	isr_5
	.quad	isr_6
	.quad	isr_7
	.quad	isr_8
	.quad	isr_9
	.quad	isr_10
	.quad	isr_11
	.quad	isr_12
	.quad	isr_13
	.quad	isr_14
	.quad	isr_15
	.quad	isr_16
	.quad	isr_17
	.quad	isr_18
	.quad	isr_19
	.quad	isr_20
	.quad	isr_21
	.quad	isr_22
	.quad	isr_23
	.quad	isr_24
	.quad	isr_25
	.quad	isr_26
	.quad	isr_27
	.quad	isr_28
	.quad	isr_29
	.quad	isr_30
	.quad	isr_31
	.quad	isr_32
	.quad	isr_33
	.quad	isr_34
	.quad	isr_35
	.quad	isr_36
	.quad	isr_37
	.quad	isr_38
	.quad	isr_39
	.quad	isr_40
	.quad	isr_41
	.quad	isr_42
	.quad	isr_43
	.quad	isr_44
	.quad	isr_45
	.quad	isr_46
	.quad	isr_47
	.quad	isr_48

