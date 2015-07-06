	.globl start
	.code16

	.set STACK_TOP,0x7c00
	.set PART_TBL,0x7e00

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
	callw	loaddsk
	# scan the partition table for bootable partitions
	# print the bootable partitions to the screen
	# allow the user to choose one
	# load that one into memory
	# hop to it

# load from a disk in CHS mode
# params:
#	dl	drive number
#	cl	sector number
#	ch	cylinder number
#	dh	head
#	bx	buffer
loaddsk:
	movw	$0x2,%ah
	int	$0x13
	# TODO: error printing
	retw

# prints the partition table in a readable format
# params:
#	bx	partition table
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
	movw	%bx,%di
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

# TODO
getname:
	retw
