	.globl	start
	.code16

	.set STACK_TOP,0x7c00	# stack origin
	.set GDT_SIZE,0x20	# size of the 32 bit global descriptor table
	.set TSS_SPC,tssspc	# location of the Task State Segment (TSS)
	.set TSS_SZ,0x65	# size of the TSS
	#.set IDT_SPC,0x8085	# location of the Interrupt Descriptor Table (IDT)
	.set IDT_SZ,0x300	# size of the IDT

	.set CODE_SEL,0x8	# Code segment index in the GDT
	.set DATA_SEL,0x10	# Data segment index in the GDT
	.set TSS_SEL,0x18	# TSS segment index in the (32 bit) GDT

	.set VIDEO_BASE,0xb8000	# the start of video memory (for printing to screen)

	.set PML4T,0x10000	# the 4th level of the paging hierarchy
	.set PDPT,0x11000	# the 3rd level of the paging hierarchy
	.set PDT,0x12000	# the 2nd level of the paging hierarchy
	.set PT,0x13000		# the page table (1st level)

	.set GDT64_SZ,0x18	# the size of the 64 bit GDT

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
	lgdt	gdtdesc		# set the GDT
	# TODO: interrupt handlers
	# set protected mode
	mov	%cr0,%eax	# load control register zero
	orb	$0x1,%al	# set the PM bit
	mov	%eax,%cr0	# write the control register
	# set up the TSS
	movw	$TSS_SPC,%di
	callw	createtss
	ljmp	$0x8,$main	# jump to the 32 bit entry point

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
	# TODO: error handling
	# restore register values from the stack
	popw	%ax
	popw	%bx
	retw

# this is the global descriptor table, which describes memory segments;
# we currently use the bare minimum table because this table is quickly
# abandoned in our jump to 64 bit
gdt:
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

# create the TSS in memory
# params:
#	di	a pointer to the TSS
createtss:
	movw	$0x10,0x8(%di)		# write the stack segment
	movw	$STACK_TOP,0x4(%di)	# write the kernel stack base
	movw	$TSS_SZ,0x66(%di)	# write the io bitmap
	retw

	.code32

# 32 bit main
main:
	# set up the 32 bit stack
	xor	%ecx,%ecx
	movb	$DATA_SEL,%cl
	movw	%cx,%ss
	# set up the TSS
	movw	$TSS_SEL,%ax
	ltr	%ax
	# DEBUG
	call	prclrscrn		# call the subroutine that clears the screen
	movl	$port_msg,%eax		# load the 32 bit protected mode welcome message
	call	prputstr		# print the message to the screen
	call	prputn			# print a new line
	# try to jump to 64 bit mode
	# check for CPUID extended
	movl	$0x80000000,%eax	# CPUID parameter; asks for the highest value supported
	cpuid				# get CPU information
	movl	$0x80000001,%ebx	# see if the CPUID operation can take this value
	cmpl	%eax,%ebx
	jg	main.0			# if the CPUID operation does not support extended features
					# jump to an error state
	# check for 64 bit support
	movl	$0x80000001,%eax	# CPUID parameter; asks for list of supported features
	cpuid				# get CPU information
	andl	$0x20000000,%edx	# mask out the long mode bit
	testl	%edx,%edx		# check that the long mode bit is set;
	je	main.1			# if the long mode bit is clear (zero), jump to error state
	# actually start going to 64 bit mode
	movl	$do_long_mode_msg,%eax	# load a message notifying the user of our intent to boot into
					# long mode
	call	prputstr		# print the message
	call	prputn			# print a new line
	call	disable_paging		# disable paging so that we can construct the page table
	call	create_page_tables	# call the subroutine to create the page tables
	call	enable_pae		# enable physical address extension
	call	enable_lm		# enable long mode
	call	enable_paging		# enable paging
	# debugging
	movl	$long_mode_comp_msg,%eax	# load a message telling the user that we are now in 
					# IA-32e long mode compatibility mode
	call	prputstr		# print the message
	call	prputn			# print a new line
	lgdt	gdt64desc		# load the long mode descriptor table;
					# see, I told you we were just gonna ditch that other one
	ljmp	$0x08,$main64		# jump to the 64 bit entry point
main.0:
	movl	$no_cpuid_msg,%eax	# load a message telling the user that their CPU sucks a lot
	call	prputstr		# print it
	call	prputn			# print a new line
	jmp	main.2			# jump to error handling
main.1:
	movl	$no_long_mode_msg,%eax	# load a message telling the user that their CPU sucks a little
	call	prputstr		# print it
	call	prputn			# print a new line
	jmp	main.2			# jump to error handling
main.2:
	# TODO: Either load a 32 bit version of the kernel or Michael Bay-splode
	# because we don't support 32 bit CPUs
	jmp	.

# utils

# disable paging; no parameters or return value
disable_paging:
	pushl	%eax			# save the value in %eax to the stack
	movl	%cr0,%eax		# load the control register
	andl	$0x7FFFFFFF,%eax	# clear the paging bit
	movl	%eax,%cr0		# write the control register back
	movl	$pgng_off_msg,%eax	# notify the user that paging is off
	call	prputstr		# print that message
	call	prputn			# print a new line
	popl	%eax			# restore %eax from the stack
	ret				# exit

# enable paging; no parameters or return value
enable_paging:
	pushl	%eax			# save the value in %eax to the stack
	movl	%cr0,%eax		# load the control register
	orl	$0x80000000,%eax	# set the paging bit
	movl	%eax,%cr0		# write the control register back
	movl	$pgng_on_msg,%eax	# notify the user that paging is on
	call	prputstr		# print that message
	call	prputn			# print a new line
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

# printing stuff
current_video_mem:
	.long	VIDEO_BASE
	.byte	0x0		# x
	.byte	0x0		# y

# clear the screen
prclrscrn:
	# save register values to the stack
	pushl	%eax
	pushl	%ecx
	movl	$current_video_mem,%eax
	movl	$VIDEO_BASE,(%eax)
	movw	$0x0,0x4(%eax)
	movl	$0x25,%ecx
prclrscrn.0:
	call	prputn
	loop	prclrscrn.0
	movl	$current_video_mem,%eax
	movl	$VIDEO_BASE,(%eax)
	movw	$0x0,0x4(%eax)
	movl	$0x25,%ecx
	# restore register values to the stack
	popl	%ecx
	popl	%eax
	ret

# print the string pointed to by %eax
prputstr:
	# save register values to the stack
	pushl	%eax		
	pushl	%edi
	movl	%eax,%edi	# save string pointer to %edi
	xorl	%eax,%eax	# %eax = 0
prputstr.0:
	movb	(%edi),%al	# load the character pointed to by the string pointer
	testb	%al,%al		# test the character for 0
	je	prputstr.1	# if the character is 0, we're done and can exit
	call	prputchr	# print out the character
	incl	%edi		# increment the string pointer
	jmp	prputstr.0	# jump to the start of the loop to continue
prputstr.1:
	# restore register values from the stack
	popl	%edi
	popl	%eax
	ret			# exit subroutine

# print a new line
prputn:
	# save regs
	pushl	%eax
prputn.0:
	# print space
	movl	$0x20,%eax
	call	prputchr
	# load current x
	movl	$current_video_mem,%eax
	movb	0x4(%eax),%al
	# is it zero?
	testb	%al,%al
	# if yes, exit
	jz	prputn.1
	# loop
	jmp	prputn.0
prputn.1:
	# restore regs
	popl	%eax
	ret

# prints the character in %eax
prputchr:
	# save registers
	pushl	%edi
	pushl	%esi
	pushl	%eax
	# put the character into video memory
	movl	$current_video_mem,%edi
	movl	(%edi),%esi
	movb	%al,(%esi)
	movb	$0x7,0x1(%esi)
	# increment the video memory pointer
	addl	$0x2,%esi
	movl	%esi,(%edi)
	# increment x
	xorl	%eax,%eax
	movb	0x4(%edi),%al		# eax = current_video_mem.x
	incl	%eax			# x++
	movl	$0x50,%esi		# esi = 81
	cmpl	%eax,%esi		# x == 81?
	jne	prputchr.0
	# if x == 81, x = 0, y++
	xorl	%eax,%eax		# x = 0
	movb	%al,0x4(%edi)		# save x
	movb	0x5(%edi),%al		# al = y
	incl	%eax			# y++
	movb	%al,0x5(%edi)		# save y
	jmp	prputchr.1
prputchr.0:
	# if x != 81, save x
	movb	%al,0x4(%edi)
prputchr.1:
	# restore registers
	popl	%eax
	popl	%esi
	popl	%edi
	ret

# some data!
port_msg:
	.ascii	"Welcome to 32 bit protected mode!"
	.byte	0x0

no_cpuid_msg:
	.ascii	"Your processor does not support CPUID extended"
	.byte	0x0

no_long_mode_msg:
	.ascii	"Your processor does not support 64 bit mode"
	.byte	0x0

do_long_mode_msg:
	.ascii	"Proceeding with 64 bit boot up"
	.byte	0x0

pgng_off_msg:
	.ascii	"Disabled paging"
	.byte	0x0

pgng_on_msg:
	.ascii	"Enabled paging"
	.byte	0x0

long_mode_comp_msg:
	.ascii	"Welcome to 64 bit compatibility mode!"
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

	#movw	%ax,%ss
	movw	%ax,%ds
	movw	%ax,%es
	movw	%ax,%fs
	movw	%ax,%gs
	

	mov	$STACK_TOP,%rsp			# reset the stack
	mov	$long_mode_full_msg,%rax	# note to the user that we made it to
						#	protected mode
	call	prputstr64			# print that message

	movw	$0x2820,%bx			# remap PIC interrupts
	call	setpic
	movb	$0xff,%al			# set PIC enable masks
	movb	$0xff,%ah			# only enable keyboard interrupts for now
	call	enablepic
	mov	$idtspc,%rdi
	call	build_idt			# build the IDT
	lidt	idtdesc64			# install the IDT
	#sti					# enable interrupts
	int	$0x21				# trigger an interrupt to make sure we did it right
	jmp	.

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
	mov	$default_isr,%rdi	# load up the function pointer
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
	mov	$default_isr,%rdi	# load up the function pointer
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
	# restore register values from the stack
	popq	%rsi
	popq	%rdi
	popq	%rdx
	popq	%rcx
	popq	%rax
	ret			# exit this function

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

# default ISR, for testing purposes
default_isr:
	# save all the general purpose register values to the stack
	# note: pusha is apparently not supported in long mode
	pushq	%rax
	pushq	%rbx
	pushq	%rcx
	pushq	%rdx
	mov	$0x41,%rax	# load 'A'
	call	prputchr64	# print 'A' to the screen to indicate that we got an interrupt
	# restore all the general purpose register values from the stack
	popq	%rdx
	popq	%rcx
	popq	%rbx
	popq	%rax
	jmp	.
	iretq			# return in an extra special, interrupt-y kinda way

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
	.byte	0x90
	.byte	0x20
	.byte	0x0

gdt64desc:
	.word	GDT64_SZ-1
	.quad	gdt64

idtdesc64:
	.word	IDT_SZ-1
	.quad	idtspc


# make space for the IDT
idtspc:
.fill	IDT_SZ,0x1,0x0
	
tssspc:
.fill	TSS_SZ,0x1,0x0
