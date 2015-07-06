	.globl start
	.code16

	.set STACK_TOP,0x7c00
	.set PART_TBL,0x7e00
	.set NEXT_SEG,0x8000

start:
	# clear segments
	xorw	%ax,%ax
	movw	%ax,%es
	movw	%ax,%ds
	movw	%ax,%ss
	# set up the stack
	movw	$STACK_TOP,%sp
	# TODO: boot from other drives
	# get the drive number
	# load the partition table from sector two on that drive
	movw	$0x2,%cx
	movw	$0x0,%dx
	movw	$PART_TBL,%bx
	movb	$0x1,%al
	callw	loaddsk
	# scan the partition table for bootable partitions
	# print the bootable partitions to the screen
	movw	$PART_TBL,%ax
	callw	printtbl
	# allow the user to choose one
	callw	getint
	# load that one into memory
	move	$NEXT_SEG,%bx
	callw	loadprt
	# hop to it
	jmp	*%bx

# load from a disk in CHS mode
# params:
#	dl	drive number
#	cl	sector number
#	ch	cylinder number
#	dh	head
#	bx	buffer
#	al	number of sectors to read
loaddsk:
	movb	$0x2,%ah
	int	$0x13
	# TODO: error printing
	retw

# prints the partition table in a readable format
# params:
#	ax	partition table
printtbl:
	# each entry in the table has 4 parts
	#	1. 1 byte filesystem type
	#	2. 1 byte bootable flag
	#	3. 6 byte descriptor in CHS format
	#	4. 8 byte descriptor in LBA format
	# NOTE: format shamelessly ripped off from FreeBSD
	# save registers
	pushw	%bx
	pushw	%di
	pushw	%ax
	movw	%ax,%di
	xorw	%bx,%bx
	xorw	%ax,%ax
printtbl.0:
	# load the filesystem type
	movb	0x0(%di),%al
	# is it zero?
	testb	%al,%al
	# if yes, we're done
	je	printtbl.2
	# load the bootable flag
	movb	0x1(%di),%al
	# is it zero?
	testb	%al,%al
	# if yes, skip to the next one
	je	printtbl.1
	# print out the number
	movw	%bx,%ax
	callw	putint
	# convert the filesystem type to a name
	movb	0x0(%di),%al
	callw	getname
	# print out the name
	callw	putstr
printtbl.1:
	# increment the pointer
	addw	$0x10,%di
	# increment the number
	inc	%bx
	# loop
	jmp	printtbl.0
printtbl.2:
	# restore registers
	popw	%ax
	popw	%di
	popw	%bx
	# exit
	retw

# TODO
putint:
	retw

# TODO
putstr:
	retw

# prints character in %al
putchr:
	pushw	%bx
	push	%ax
	movw	$0x7,%bx
	movb	$0xe,%ah
	int	$0x10
	# TODO: error handling
	popw	%ax
	popw	%bx
	retw

# TODO
getname:
	retw

# TODO
getint:
	retw

# TODO
# load a partition
loadprt:
	retw
