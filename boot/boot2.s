	.globl	start
	.code16

	.set STACK_TOP,0x7c00
	.set ORG_TARG,0x600
	.set GDT_SPC,0x8000
	.set GDT_SIZE,0x20	# 4 8-byte entries

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
	# clear space for the GDT
	movw	$GDT_SPC,%di
	movw	$GDT_SIZE,%cx
	shrw	$0x1,%cx
gdt_clear:
	movw	$0x0,(%di)
	loop	gdt_clear
	# fill in the GDT
	# write the null segment
	movw	$GDT_SPC,%di
	xorw	%si,%si
	xorw	%ax,%ax
	xorw	%bx,%bx
	xorw	%cx,%cx
	xorw	%dx,%dx
	callw	installgdt
	# write a code segment
	addw	$0x8,%di
	# the base is still clear
	# set a crazy high limit
	movw	$0xffff,%bx	# almost all the pages
	movb	$0xff,%dl	# all the pages
	movb	$0x0,%dh	# kernel ring
	movb	$0x1,%cl	# eXe
	movb	$0x1,%dh	# readable
	callw	installgdt
	# write a data segment
	addw	$0x8,%di
	# base is already cleared
	# limit is already set
	# kernel ring is already set
	movb	$0x0,%cl	# noX
	# writable is already set
	callw	installgdt
	# set it as the GDT
	lgdt	gdtdesc
	# DEBUG
	movb	$0x43,%al
	callw	putchr
	jmp	.

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

# installs an entry in the GDT
# %si:%ax - base
# %dl:%bx - limit
# %di - GDT pointer
# %ch - ring level
# %cl - executable
# %dh - readable/writable
installgdt:
	# save registers
	pushaw
	# write the lower word of the limit
	movw	%bx,(%di)
	# write the lower word of the base
	movw	%ax,0x2(%di)
	# write the next byte of the base
	movw	%si,%ax
	movb	%al,0x4(%di)
	# form the access byte
	xorb	%al,%al		# clear byte
	orb	$0x90,%al	# set the present bit and another, misc. bit
	rolb	$0x5,%ch	# roll the ring level into place
	orb	%ch,%al		# mask it into the access byte
	rolb	$0x3,%cl	# roll the exe bit into place
	orb	%cl,%al		# mask it into the access byte
	andb	$0xfb,%al	# all segments count up/do not conform
	rolb	$0x1,%dh	# roll rw bit into place
	orb	%dh,%al		# set read/write-ability
	# write it
	movb	%al,0x5(%di)
	# combine the last limit nibble with the flags
	movb	%dl,%al
	andb	$0xcf,%al
	# write it
	movb	%al,0x6(%di)
	# write the last byte of the base
	movw	%si,%ax
	movb	%ah,0x7(%di)
	# restore registers
	popaw
	retw

gdtdesc:
	.word	GDT_SIZE-1
	.word	0x0
	.word	GDT_SPC

