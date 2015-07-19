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

	.set PML4T,0x10000
	.set PDPT,0x11000
	.set PDT,0x12000
	.set PT,0x13000

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
	xor	%ecx,%ecx
	movb	$DATA_SEL,%cl
	movw	%cx,%ss
	# set up the TSS
	movw	$TSS_SEL,%ax
	ltr	%ax
	# DEBUG
	call	prclrscrn
	movl	$port_msg,%eax
	call	prputstr
	call	prputn
	# try to jump to 64 bit mode
	# see if we can check for 64 bit support
	movl	$0x80000000,%eax
	cpuid
	movl	$0x80000001,%ebx
	cmpl	%eax,%ebx
	jg	main.0
	# check for 64 bit support
	movl	$0x80000001,%eax
	cpuid
	andl	$0x20000000,%edx
	testl	%edx,%edx
	je	main.1
	# actually start going to 64 bit mode
	movl	$do_long_mode_msg,%eax
	call	prputstr
	call	prputn
	# enforce that paging is disabled
	call	disable_paging
	# set up paging
	call	create_page_tables
	call	enable_pae
	call	enable_lm
	# reenable paging
	call	enable_paging
	# debugging
	movl	$long_mode_comp_msg,%eax
	call	prputstr
	call	prputn
	# make the leap to 64 bit code
	jmp	.
main.0:
	movl	$no_cpuid_msg,%eax
	call	prputstr
	call	prputn
	jmp	main.2
main.1:
	movl	$no_long_mode_msg,%eax
	call	prputstr
	call	prputn
	jmp	main.2
main.2:
	# TODO: Either load a 32 bit version of the kernel or Michael Bay-splode
	# because we don't support 32 bit CPUs
	jmp	.

# utils
disable_paging:
	pushl	%eax
	movl	%cr0,%eax
	andl	$0x7FFFFFFF,%eax
	movl	%eax,%cr0
	movl	$pgng_off_msg,%eax
	call	prputstr
	call	prputn
	popl	%eax
	ret

enable_paging:
	pushl	%eax
	movl	%cr0,%eax
	orl	$0x80000000,%eax
	movl	%eax,%cr0
	movl	$pgng_on_msg,%eax
	call	prputstr
	call	prputn
	popl	%eax
	ret

enable_pae:
	pushl	%eax
	movl	%cr4,%eax
	orl	$0x10,%eax
	movl	%eax,%cr4
	popl	%eax
	ret

enable_lm:
	pushl	%ecx
	pushl	%eax
	movl	$0xC0000080,%ecx
	rdmsr
	orl	$0x100,%eax
	wrmsr
	popl	%eax
	popl	%ecx
	ret

create_page_tables:
	# save registers
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edi
	# clear the tables
	movl	$PML4T,%edi
	movl	$0x1000,%ecx
create_page_tables.1:
	movl	$0x0,(%edi)
	addl	$0x4,%edi
	loop	create_page_tables.1
	# make the first entry of the PML4T point to the first PDPT
	movl	$PML4T,%edi	# load &PML4T
	movl	$PDPT,%eax	# load &PDPT
	orl	$0x03,%eax	# set PDPT to be P/R/W
	movl	%eax,(%edi)	# PML4T[0] = PDPT
	# make the first entry of the PDPT point to the first PDT
	movl	$PDPT,%edi
	movl	$PDT,%eax
	orl	$0x03,%eax
	movl	%eax,(%edi)
	# make the first entry of the PDT point to the first PT
	movl	$PDT,%edi
	movl	$PT,%eax
	orl	$0x03,%eax
	movl	%eax,(%edi)
	# identity map the PT
	movl	$PT,%edi
	movl	$0x200,%ecx
	movl	$0x03,%ebx
create_page_tables.0:
	movl	%ebx,(%edi)
	addl	$0x08,%edi
	addl	$0x1000,%ebx
	loop	create_page_tables.0
	# set all this shit to be canonical
	movl	$PML4T,%eax
	movl	%eax,%cr3
	# restore registers
	popl	%edi
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret

# printing stuff
current_video_mem:
	.long	VIDEO_BASE
	.byte	0x0		# x
	.byte	0x0		# y

# clear the screen
prclrscrn:
	# save regs
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
	# restore regs
	popl	%ecx
	popl	%eax
	ret

# print the string pointed to by %eax
prputstr:
	# save regs
	pushl	%eax
	pushl	%edi
	movl	%eax,%edi
	xorl	%eax,%eax
prputstr.0:
	movb	(%edi),%al
	testb	%al,%al
	je	prputstr.1
	call	prputchr
	incl	%edi
	jmp	prputstr.0
prputstr.1:
	# restore regs
	popl	%edi
	popl	%eax
	ret

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
