	.globl start
	.code16

	.set STACK_TOP,0x7c00
	.set PART_TBL,0x7e00
	.set PART_SZ,0x10
	.set NEXT_SEG,0x8000
	.set DAB_SPC,0x600	# TODO: This reasonably

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
	movw	$NEXT_SEG,%bx
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
	# new line
	callw	putn
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

# print the integer in %ax
putint:
	# save registers
	pushw	%ax
	pushw	%bx
	pushw	%cx
	movw	%ax,%bx		# clear out %ax
	xorw	%ax,%ax
	movw	$0x4,%cx	# there are 4 nibbles in a word
putint.0:
	rolw	$0x4,%bx	# roll left
	# isolate the lower nibble
	movw	%bx,%ax
	andw	$0x0f,%ax
	addw	$0x30,%ax	# shift it into the printable range
	# adjust for hex
	cmpw	$0x3A,%ax
	jae	putint.1
	addw	$0x07,%ax
putint.1:
	callw	putchr		# print the character
	loop	putint.0	# loop
	# restore registers
	popw	%cx
	popw	%bx
	popw	%ax
	retw

# print the string pointed to by %ax
putstr:
	# save registers
	pushw	%ax
	pushw	%di
	movw	%di,%ax
	xorw	%ax,%ax
putstr.0:
	movb	(%di),%al	# load the byte
	testb	%al,%al		# is it zero?
	je	putstr.1	# yes, done
	callw	putchr		# print character
	inc	%di		# increment pointer
	jmp	putstr.0	# loop
putstr.1:
	# restore registers
	popw	%di
	popw	%ax
	retw

putn:
	pushw	%ax
	movw	$0x13,%ax
	callw	putchr
	movw	$0x10,%ax
	callw	putchr
	popw	%ax
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

# convert the filesystem ID in %al to a name
# result is pointer in %ax
getname:
	# save registers
	pushw	%di
	pushw	%bx
	xorw	%bx,%bx
	# load the partition index table
	movw	$partindx,%di
getname.0:
	# scan it, looking for matches
	movb	(%di),%bl	# load current entry
	testb	%bl,%bl		# zero?
	je	getname.1	# yes, no matches
	cmpb	%al,%bl		# match?
	je	getname.1	# yes
	addw	$0x3,%di	# increment pointer
	jmp	getname.0	# loop
getname.1:
	# get string pointer
	incw	%di
	movw	(%di),%ax
	# restore registers
	popw	%cx
	popw	%bx
	popw	%di
	retw

# TODO: add timeout
# returns the user input integer in %ax
getint:
	# call the bios routine to get a key press
	movb	$0x0,%ah
	int	$0x16
	# convert to integer
	movb	$0x0,%ah
	subw	$0x30,%ax
	# correct for hexadecimal
	cmpw	$0x0a,%ax
	jg	getint.0
	subw	$0x07,%ax
getint.0:
	retw

# load the partition whose number is in %ax to the buffer in %bx
# drive number in %dl
loadprt:
	# save registers
	# calculate index
	movw	$PART_SZ,%di
	mulw	%di
	movw	%di,%ax
	# get the pointer to the partition table
	addw	$PART_TBL,%di
	# step to its LBA descriptor
	addw	$0x08,%di
	# load the number of sectors
	movw	(%di),%ax
	# load the index of the first sector
	movw	0x2(%di),%bx
	movw	0x4(%di),%cx
	movw	0x6(%di),%dx
	# create the DAP
	movw	$DAB_SPC,%di
	movb	$0x10,(%di)		# table size
	movb	$0x0,0x1(%di)		# reserved
	movw	%ax,0x2(%di)		# number of sectors to read
	popw	%ax			# reload buffer offset
	movw	%ax,0x4(%di)		# write buffer offset
	movw	$0x0,0x6(%di)		# write segment (zero)
	movw	$0x0,0x8(%di)		# only 6 bytes in sector address
	movw	%bx,0x0a(%di)		# sector address
	movw	%cx,0x0c(%di)		# sector address
	movw	%dx,0x0e(%di)		# sector address
	# do an extended read
	movw	$DAB_SPC,%si
	movb	$0x42,%ah
	popw	%dx			# reload drive number
	int	$0x13
	# report if an error occured
	jnc	loadprt.0
	movw	$load_error_msg,%ax
	callw	putstr
	callw	putn
loadprt.0:
	# restore registers
	retw

load_error_msg:	.ascii	"LOAD ERROR"
	.byte	0x0

oswin:	.ascii	"WINDOWS"
	.byte	0x0
oslin:	.ascii	"LINUX"
	.byte	0x0
osbsd:	.ascii	"BSD"
	.byte	0x0
osgmb:	.ascii	"GAMBIT"
	.byte	0x0
osunkn:	.ascii	"UNKNOWN"
	.byte	0x0

partindx:
.byte	0x07		# Windows
.word	oswin
.byte	0x83		# Linux
.word	oslin
.byte	0xa5		# BSD
.word	osbsd
.byte	0x66		# TODO: gambit
.word	osgmb
.byte	0x00		# end of table
.word	osunkn
