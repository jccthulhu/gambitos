	.globl	start
	.code16

	.set STACK_TOP,0x7c00	# memory location of the stack's origin
	.set GDT_SIZE,0x20	# size of the 32 bit Global Descriptor Table (GDT)
	.set TSS_SPC,0x8020	# memory location of the Task State Segment (TSS), which we only use in 32 bit (real) mode
	.set TSS_SZ,0x65	# size of the TSS
	.set IDT_SPC,0x8085	# memory location of the Interrupt Descriptor Table (IDT)
	.set IDT_SZ,0x190	# size of the IDT

	.set CODE_SEL,0x8	# the index for the code segment in the GDT
	.set DATA_SEL,0x10	# the index for the data segment in the GDT
	.set TSS_SEL,0x18	# the index for the TSS segment in the (32 bit) GDT

	.set VIDEO_BASE,0xb8000	# the memory location for the origin of video memory, used for printing to the screen
				# this is the start of a 25 by 80 character block of memory, which is written to the screen 60 times each second

	# the memory locations of the page tables used for virtualization in protected long (64 bit) mode
	# we can't jump into long mode without these locations
	# we set these up with the bare minimum work so that we can start writing C code to set them up properly
	# the many levels of addressing to get into a page table is used so that 512^4 pages of memory can be accessed
	# this many levels of granularity makes it easy to isolate specific segemnts in memory
	# having 4 levels of addressing is required for entering into long mode
	# each of these tables takes up 0x1000 bytes in memory, and each entry within each table is 8 bytes long, which is a pointer in long mode
	.set PML4T,0x10000	# this is the address to the 'page map level 4' (PML4) table, which points to the 512 PDP tables
	.set PDPT,0x11000	# this is the address to the page directory pointer (PDP) table, which points to the 512 PD tables
	.set PDT,0x12000	# this is the address to the page directory (PD) table, which points to the 512 page tables
	.set PT,0x13000		# this is the address to the first page table, which points to 512 pages in memory

	.set GDT64_SZ,0x18	# the size of the 64 bit GDT, witch describes segments in memory
				# it is strange that this is necessary since segments are obsolete in long mode

start:
	# interrupts are for chumps
	cli			# disable interrupts to prevent trippple faults, which forces a hard reset
	cld			# cause string operations to increment the used pointers

	# clear segment selectors
	xorw	%ax,%ax		# set %ax to 0 using XOR
	movw	%ax,%es		# write %ax (0) to %es, the extra segment regsiter
	movw	%ax,%ds		# write %ax (0) to %ds, the data segment register
	movw	%ax,%ss		# write %ax (0) to %ss, the stack segment register
	movw	$STACK_TOP,%sp	# set %sp, the stack pointer register, to the stack origin

	# enable more than 1MB of memory, more than anyone would ever need
	callw	seta20		# call the 'seta20' subroutine to the A20 line, which will stop the automatic wrapping of memory addresses past 1MB
	
	lgdt	gdtdesc		# load GDT by passing in the address of the GDT descriptor
				# GNU requires that this instruction take a label instead of a regsiter value

	# get us into protected mode so that we can then jump into 32 bit mode
	# protected mode is necessary for virtual memory
	mov	%cr0,%eax	# set %eax to %cr0, the 0th control register
				# %cr9 is a bit field that holds information for protected mode, paging enabled, floating point emulation, and more
	orb	$0x1,%al	# set the protected mode bit in the copied bit field to 1
	mov	%eax,%cr0	# write the modified bit field to %cr0, the 0th control register

	# set up the Task State Segment (TSS)
	movw	$TSS_SPC,%di	# set %di to the TSS pointer
	callw	createtss	# call the 'createtss' subroutine to setup the TSS
	ljmp	$CODE_SEL,$main	# jump to the 32 bit mode entry point (main) and set the code segment register to the value in the proper GDT index



###
# sets the A20 line, the 21st bit of memory addresses, to enable more than 1MB of memory
# params: none
# returns: none
# note: the A20 line is effectively, in the past, off the CPU within the keyboard, so we have to communicate as though it's off the chip
seta20:
	pushw	%ax		# save %ax to the stack
seta20.1:
	inb	$0x64,%al	# read the keyboard control chip's status register to see if it's busy
	testb	$0x2,%al	# is the keyboard control chip busy?
	jnz	seta20.1	# if the keyboard control chip is busy, loop and try again (busy wait)

	# if the keyboard control chip is ready for commands
	movb	$0xd1,%al	# set %al to the value of the chip's write command, which is 0xd1
	outb	%al,$0x64	# send the command within %al to the keyboard control chip
seta20.2:
	inb	$0x64,%al	# read the keyboard control chip's status register to see if it's busy
	testb	$0x2,%al	# is the keyboard control chip busy?
	jnz	seta20.2	# if the keyboard control chip is busy, loop and try again (busy wait)

	# if the keyboard control chip is ready for commands
	movb	$0xdf,%al	# set %al to the value of the chip's enable command, which is 0xdf
	outb	%al,$0x60	# write out the command within %al to the keyboard control chip to finally enable the A20 line

	popw	%ax		# reset %ax to its value from the stack
	retw			# return from the subroutine



###
# print a character to the screen
# note: this is for use in real mode, and will cause a triple fault in protected mode
# params:
#	al	the character (ASCII value)
putchr:
	# save register values to the stack
	pushw	%bx
	pushw	%ax

	movw	$0x7,%bx	# print white characters on black background
	movb	$0xe,%ah	# when we're stil in real mode, set %al to 0xe, which is the bios call to print to the screen
	int	$0x10		# trigger the interrupt
	# TODO: error handling

	# restore register values from the stack
	popw	%ax
	popw	%bx
	retw			# return from the subroutine



###
# this is the global descriptor table, which describes memory segments;
# we currently use the bare minimum table because this table is quickly
# abandoned in our jump to 64 bit
# note: two words is four bytes
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



###
# create the TSS in memory
# params:
#	di	a pointer to the TSS
createtss:
	movw	$DATA_SEL,0x8(%di)	# write data segment index into the stack segment slot of the TSS
	movw	$STACK_TOP,0x4(%di)	# write the stack origin pointer into the kernel stack slot of the TSS
	movw	$TSS_SZ,0x66(%di)	# write the size of the TSS into the io bitmap pointer slot of the TSS
					# we do this because we don't use the io bitmap, so any data pointer will do
	retw




	.code32		# start the 32 bit protected mode code

# 32 bit main
main:
	# set up the 32 bit stack
	xor	%ecx,%ecx

	movb	$DATA_SEL,%cl		# set %cl to the data segment index
	movw	%cx,%ss			# set %ss, the stack segment, to %cx, the data segment index

	# set up the TSS
	movw	$TSS_SEL,%ax		# set %ax to the TSS index
	ltr	%ax			# this is the 'load TSS register' command, which loads the TSS from %ax

	# DEBUG
	call	prclrscrn		# call the subroutine that clears the screen
	movl	$port_msg,%eax		# load the 32 bit protected mode welcome message
	call	prputstr		# print the message to the screen
	call	prputn			# print a new line
	# END DEBUG

	# now we will try to jump into 64 bit mode

	# check for CPUID extended
	movl	$0x80000000,%eax	# CPUID parameter; asks for the highest value of that parameter supported
	cpuid				# get CPU information specified in %eax
	movl	$0x80000001,%ebx	# see if the CPUID operation can take the extended bit value
	cmpl	%eax,%ebx		# compare %eax, the result of CPUID, to %ebx, the minimum supported value we need
	jg	main.0			# if the CPUID operation does not support extended features, jump to an error state

	# check for 64 bit support
	movl	$0x80000001,%eax	# CPUID parameter; asks for list of supported features
	cpuid				# get CPU information specified in %eax
	andl	$0x20000000,%edx	# mask out the long mode bit from the result of the CPUID operation
	testl	%edx,%edx		# is the long mode bit set in the result from the CPUID operation?
	je	main.1			# if the long mode bit is clear (zero), jump to the error state

	# if the CPUID extended bit is set, adn there is 64 bit support, we start going into 64 bit mode

	# DEBUG
	movl	$do_long_mode_msg,%eax	# load a message notifying the user of our intent to boot into
					# long mode
	call	prputstr		# print the message
	call	prputn			# print a new line
	# END DEBUG

	call	disable_paging		# call the disable paging subroutine so that we can construct the page table
	call	create_page_tables	# call the subroutine to create the page tables
	call	enable_pae		# call the subroutine to enable physical address extension, which is needed for 4-level paging
	call	enable_lm		# call the subroutine to finally enable long mode
	call	enable_paging		# call the subroutine to enable paging

	# DEBUG
	movl	$long_mode_comp_msg,%eax	# load a message telling the user that we are now in 
					# IA-32e long mode compatibility mode
	call	prputstr		# print the message
	call	prputn			# print a new line
	# END DEBUG

	lgdt	gdt64desc		# load the long mode descriptor table;
					# see, I told you we were just gonna ditch that other one
	ljmp	$0x08,$main64		# jump to the 64 bit entry point and set the code segment register to the value in the proper GDT index

# print an error message that the CPU doesn't support extended features
main.0:
	movl	$no_cpuid_msg,%eax	# load a message telling the user that their CPU sucks a lot
	call	prputstr		# print it
	call	prputn			# print a new line
	jmp	main.2			# jump to error handling

# print an error message that the CPU doesn't support 64 bit mode
main.1:
	movl	$no_long_mode_msg,%eax	# load a message telling the user that their CPU sucks a little
	call	prputstr		# print it
	call	prputn			# print a new line
	jmp	main.2			# jump to error handling

main.2:
	# TODO: Either load a 32 bit version of the kernel or Michael Bay-splode
	# because we don't support 32 bit CPUs
	jmp	.			# create a tight loop to halt the program




###
# utilities



###
# disable paging
# params: none
# returns: none
disable_paging:
	pushl	%eax			# save the value in %eax to the stack
	movl	%cr0,%eax		# load the 0th control register into %eax
	andl	$0x7FFFFFFF,%eax	# clear the paging bit from %eax
	movl	%eax,%cr0		# write %eax back into the 0th control register

	# DEBUGGING
	movl	$pgng_off_msg,%eax	# notify the user that paging is off
	call	prputstr		# print that message
	call	prputn			# print a new line
	# END DEBUGGING

	popl	%eax			# restore %eax from the stack
	ret				# return from the subroutine



###
# enable paging
# params: none
# returns: none
enable_paging:
	pushl	%eax			# save the value in %eax to the stack
	movl	%cr0,%eax		# load the 0th control register into %eax
	orl	$0x80000000,%eax	# set the paging bit in %eax
	movl	%eax,%cr0		# write %eax back into the 0th control register

	# DEBUGGING
	movl	$pgng_on_msg,%eax	# notify the user that paging is on
	call	prputstr		# print that message
	call	prputn			# print a new line
	# END DEBUGGING

	popl	%eax			# restore the value of %eax from the stack
	ret				# return from the subroutine



###
# enable physical address extension (PAE)
# params: none
# returns: none
enable_pae:
	pushl	%eax			# save value of %eax onto stack
	movl	%cr4,%eax		# load the 4th control register into %eax
	orl	$0x20,%eax		# set PAE bit in %eax
	movl	%eax,%cr4		# write %eax into the 4th control register
	popl	%eax			# restore value of register from stack
	ret				# return from the subroutine



###
# enable long mode
# params: none
# returns: none
enable_lm:
	# save register values onto the stack
	pushl	%ecx
	pushl	%eax

	movl	$0xC0000080,%ecx	# set %ecx to the selector value for the EFER Model Specific Register (MSR)
	rdmsr				# execute the instruction to get the MSR value specified in %ecx and place the result into %eax
	orl	$0x100,%eax		# set the long mode bit in the EFER value
	wrmsr				# write the new bit field in %eax to the MSR to turn on long mode

	# restore register values from the stack
	popl	%eax
	popl	%ecx
	ret				# return from the subroutine



###
# create the page tables
# params: none
# returns: none
create_page_tables:
	# save register values onto the stack
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edi

	# set up values for a loop to clear the tables
	movl	$PML4T,%edi		# load the pointer to the start of the page table construct into %edi
	movl	$0x1000,%ecx		# use %ecx as a loop counter (used by the loop instruction)
					# set the loop counter to the size in terms of double-words of the page table construct,
					# which is the continuous space in memory that contians the page table addressing heirarchy
# loop to clear all entries at all levels of the page table addressing heirarchy
create_page_tables.1:
	movl	$0x0,(%edi)		# write zero to the current index of the page table construct
	addl	$0x4,%edi		# increment the page table construct pointer
	loop	create_page_tables.1	# decrement %ecx, check for zero, 
					# and jump to the beginning of the loop to continue if the counter (%ecx) is non-zero

	# make the first entry of the PML4 table point to the first PDP table
	movl	$PML4T,%edi		# load the address of the PML4 table into %edi
	movl	$PDPT,%eax		# load the address of the PDP table into %eax
	orl	$0x03,%eax		# set the lower two bits of the PDP table pointer to be Present/Readable/Writable
					# this is valid since all addresses must be aligned and therefore don't use the lower two bits
	movl	%eax,(%edi)		# set the first entry of the PML4 table to the pointer to the first PDP table

	# make the first entry of the PDP table point to the first PD table
	movl	$PDPT,%edi		# load the address of the PDP table into %edi
	movl	$PDT,%eax		# load the address of the PD table into %eax
	orl	$0x03,%eax		# set the lower two bits of the PD table pointer to be Present/Readable/Writable
	movl	%eax,(%edi)		# set the first entry of the PDP table to the pointer to the first PD table

	# make the first entry of the PDT point to the first PT
	movl	$PDT,%edi		# load the address of the PD table into %edi
	movl	$PT,%eax		# load the address of the Page table into %eax
	orl	$0x03,%eax		# set the lower two bits of the Page table pointer to be Present/Readable/Writable
	movl	%eax,(%edi)		# set the first entry of the PD table to the pointer to the first Page table

	# set up the first page table to have every entry point to the corresponding physical page of memory
	# we set up the first page table to identiy map to the first pages in memory, starting with the page at address 0x0
	# this is needed because we are currently operating within the bounds of the zeroth page table and need to prevent pointer confusion when paging is finally turned on
	movl	$PT,%edi		# load the address of the first page table into %edi
	movl	$0x200,%ecx		# set the loop counter to the size in terms of quad-words of the Page Table (PT)
	movl	$0x03,%ebx		# each page is Present/Readable/Writable, so the pointer to the zeroth page and all other pages will end with 0x03
# loop to set up the entries of the zeroth page table
create_page_tables.0:
	movl	%ebx,(%edi)		# write the address of the current page into the appropriate page table entry
	addl	$0x08,%edi		# increment the page table index
	addl	$0x1000,%ebx		# increment the page address
	loop	create_page_tables.0	# jump to the start of the loop and continue, decrementing %ecx, while %ecx remains non-zero

	# load the pointer to the PML4 table into Control Register 3 (%cr3), which will allow the CPU to use our virtual memory configuration
	movl	$PML4T,%eax	# load the address to the PML4 table into %eax
	movl	%eax,%cr3	# save the address to the PML4 table into Control Register 3 to notify the CPU of its location
				# this is %cr3's only purpose in life

	# restore register values from the stack
	popl	%edi
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret				# return from thie subroutine



###
# data used for printing stuff
current_video_mem:
	.long	VIDEO_BASE
	.byte	0x0		# x
	.byte	0x0		# y


# TODO: CONTINUE COMMENTING HERE

###
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
	mov	$long_mode_full_msg,%rax
	call	prputstr64
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
	call	prputchr	# print the character
				# the 32 bit version seems to work in long mode
	inc	%rdi		# increment the string pointer
	jmp	prputstr64.0	# jump to the start of the loop to continue
prputstr64.1:
	# restore register values from the stack
	popq	%rdi
	popq	%rax
	ret			# exit
	
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

