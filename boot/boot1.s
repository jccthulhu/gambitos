	.globl start
	.code16

	.set STACK_TOP,0x7c00   # the origin of the stack (we create it ourselves) (incidentally where we get loaded into)
	.set PART_TBL,0x800     # partition table
	.set PART_SZ,0x10       # size of entries in partition table
	.set NEXT_SEG,0x7c00    # the segment in memory to load the next stage
	.set TARGET,0x600       # where stage 1 is relocated
	.set SEG_SIZE,0x200     # the size of a sector on disk (also, stage 1 cannot be any larger than this)

# note: the bios gives us the drive number that we are loading from in %dl. please take very special care of this vlaue. notice that this note is the only one with periods. it's that important, but not important enough for capitalization.

start:
	cld                     # set string operations to increment poitner value after every operation

	# clear segment selectors

	# zero out the data segments' addresses
	xorw	%ax,%ax
	movw	%ax,%es
	movw	%ax,%ds
	movw	%ax,%ss

	# set up the stack pointer
	movw	$STACK_TOP,%sp

	# relocate stage 1 by copying the entire segment from STACK_TOP to TARGET
	# we copy the entire segment from STACK_TOP to TARGET
	movw	%sp,%si
	movw	$TARGET,%di
	movw	$SEG_SIZE,%cx
	rep	                # repeatedly execute the next instruction the value of register %cx times
	movsw	                # take from %si to %di, then increment both (because cld)\

	jmp	resume-STACK_TOP+TARGET # jump to the relocated version of stage 1's 'resume' subroutine



###
# code executed in relocated stage 1
resume:
	# we expect the partition table to be located in sector 2 of the drive	
	# note: the bios gives us the first dive number into %dl

	# TODO: boot from other drives
	
	# load the partition table from sector two on that drive
	movw	$0x2,%cx
	movb	$0x0,%dh
	movw	$PART_TBL,%bx
	movb	$0x1,%al
	callw	loaddsk       # call the 'loaddisk' subroutine

	# scan the partition table for bootable partitions
	# and print the bootable partitions to the screen
	movw	$PART_TBL,%ax
	callw	printtbl      # call the 'printtbl' subroutine

	# allow the user to choose a bootable partition
	callw	getint        # call the 'getint' subroutine from user input

	# load the selected partition returned from 'getint' into memory
	movw	$NEXT_SEG,%bx
	callw	loadprt       # call the 'loadprt' subroutine
	jmp	*%bx          # jump to the loaded stage 2, which is store in %bx



###
# load from a disk in Cylinder-Head-Sector (CHS) mode, which is obsolete in more sophisticated bios
# params:
#	dl	drive number to load from
#	cl	sector number to load from the drive (or cylinder if there is one)
#	ch	cylinder number to load from the drive
#	dh	head of the cylinder to load from
#	bx	buffer to write into
#	al	number of sectors to read
loaddsk:
	pushw	%ax          # push %ax to stack to save its value
	movb	$0x2,%ah     # move the bios call number into %ah (that's where the bios likes it)
	int	$0x13        # send interrupt  0x13, which tells the bios to do stuff
	jnc	loaddsk.0    # on error, the bios sets carry, so we check that to make sure we succeed.
		             # if we succeed, jump to the end of the subroutine

	# if we fail, display an error to the user
	movw	%ax, %cx     # move the error code form %ax into %cx
	movw	$load_error_msg,%ax # move the error message into %ax for the next subroutine to use
	callw	putstr       # call the 'putstr' subroutine with the error string in %ax
	callw	putn         # call the 'putn' subroutine to print a new line
	movw	%cx, %ax     # move the error code from %cx back into %ax
	callw	putint       # call the 'putint' subroutine to print the error code from %
	callw	putn         # call the 'putn' subroutine to print a new line
	jmp	.            # hang forever. this is an eternal no-op loop.

# return out of the 'loaddsk' subroutine
loaddsk.0:
	popw	%ax          # pop the stack into %ax
		             # we don't push or pop %cx because it is only used in the failure case, which results in a hang.
	retw	             # return from the subroutine



###
# prints the partition table in a readable format
# params:
#	ax	partition table
printtbl:
	# each entry in the table has 4 parts
	#	1. 1 byte filesystem type
	#	2. 1 byte bootable flag
	#	3. 6 byte descriptor in CHS format
	#	4. 8 byte descriptor in Logical-Block-Addressing (LBA) format
	# NOTE: format shamelessly ripped off from FreeBSD

	# save registers to the stack to save their values
	pushw	%bx
	pushw	%di           # in real mode, we can only use %di and %si for indexing into memory
	pushw	%ax

	movw	%ax,%di       # move the partition table pointer into %di so that we can use it as a pointer
	xorw	%bx,%bx       # clear %bx, which XORs its value to zero out quickly
	xorw	%ax,%ax       # clear %ax, which XORs its value to zero out quickly

# a loop for finding an entry with a filesystem type and bootable flag from the partition table
printtbl.0:
	movb	0x0(%di),%al  # dereference the filesystem type and store it in %al
	testb	%al,%al       # is %al, the filesystem type, zero? if so, the equal flag is set
	je	printtbl.2    # if %al is zero, jump to the end of the 'printbl' subroutine
	
	# if %al is not zero, we have a real filesystem type
	movb	0x1(%di),%al  # move the bootable flag value into %al
	testb	%al,%al       # is %al, the bootable flag, zero? if so, the equal flag is set
	je	printtbl.1    # if %al is zero, then the partition is not bootable and we can skip it

	# if %al, is not zero, we have a real bootable flag value

	# print out the index of the partition entry in the table
	movw	%bx,%ax       # move %bx, the loop counter, into %ax for use in the 'putint' subroutine
	callw	putint        # call the 'putint' subroutine to print the index of the partition entry in the table

	# print out a dot and a space
	movw	$0x2e,%ax     # move the ASCII value for '.' into %ax for use in the 'prutchr' subroutine
	callw	putchr        # call the 'putchr' subroutine to print '.' onto the screen
	movw	$0x20,%ax     # move the ASCII value for ' ' into %ax for use in the 'putchr' subroutine
	callw	putchr        # call the 'putchr' subroutine to print ' ' onto the screen

	# convert the filesystem type to a name and print it to the screen
	movb	0x0(%di),%al  # move the filesystem type into %al for use in the 'getname' subroutine
	callw	getname       # call the 'getname' subroutine to get the name of the filesystem type 
	callw	putstr        # call the 'putstr' subroutine to print the name of the filesystem type, which is returned in %ax from 'getname'
	callw	putn          # call the 'putn' subroutine to print a new line to the screen

# contine onto the next iteration of the loop
printtbl.1:
	addw	$0x10,%di     # increment the pointer of the current entry in the partition table
	inc	%bx           # increment the loop counter
	jmp	printtbl.0    # jump to the beginning of the loop to start again

# return out of the 'printbl' subroutine
printtbl.2:
	# restore registers to their previous values
	popw	%ax
	popw	%di
	popw	%bx
	retw	              # return from the subroutine



###
# print an integer onto the screen
# note: we will always print leading zeroes
# params:
# 	ax	the integer to print in hex
putint:
	# save these registers onto the stack
	pushw	%ax
	pushw	%bx
	pushw	%cx

	movw	%ax,%bx	      # move the value from %ax into %bx
	xorw	%ax,%ax       # zero out the %ax register by XORing with itself
	movw	$0x4,%cx      # set %cx to 4 because there are 4 nibbles in a word, and we don't want to loop more than that

# a loop for printing the four characters to the screen
putint.0:
	# we want to print the high nibbles first because '1234' has '4' in the lowest nibble
	rolw	$0x4,%bx      # rotate (barrel shift) the nibbles in %bx by one nibble (0x4 bits)

	# isolate the lower nibble
	movw	%bx,%ax       # copy the rotated value of %bx into %ax
	andw	$0x0f,%ax     # mask out all nibbles but the lowest (right-most) one
	addw	$0x30,%ax     # convert the value into ASCII by adding the character '0'

	# if the value of %ax is greater than 10, we need to adjust the character into the 'ABCDEF' range, since this value is HEX
	cmpw	$0x3A,%ax     # compare the character value in %ax with 10 plus the value of character '0' 
	jb	putint.1      # if the value in %ax is below that 10+'0', jump to 'putint.1' to skip the HEX adjustment
	addw	$0x07,%ax     # otherwise, adjust the character value in %ax to the proper HEX character
putint.1:
	callw	putchr	      # call the 'putchr' subroutine to print the character value in %ax
	loop	putint.0      # continue on with the loop, decrementing %cx and testing it for 0

	# restore registers from the stack
	popw	%cx
	popw	%bx
	popw	%ax
	retw	              # return from the subroutine



###
# print a string onto the screen
# params:
# 	ax	a pointer to the string to print onto the screen
putstr:
	# save registers to the stack
	pushw	%ax
	pushw	%di

	movw	%ax,%di       # move the string pointer from %ax into %di so that we can use it as a pointer
	xorw	%ax,%ax       # zero out %ax by XORing itself

# a loop to print each and every character in the string (or else)
putstr.0:
	movb	(%di),%al     # dereference the string pointer into %al to get the first character pointed to
	testb	%al,%al       # is %al, the character, zero?
	je	putstr.1      # if the characetr is zero, we are done printing the string, and jump to 'putstr.1'
	callw	putchr        # call the 'putchr' subroutine to print the character in %al onto the screen
	inc	%di           # increment the pointer to access the next character in the screen
	jmp	putstr.0      # jump to the beginning of the loop to continue printing the string
putstr.1:
	# restore the register values from the stack
	popw	%di
	popw	%ax
	retw	              # return from the subroutine



###
# print a new line character onto the screen
# params:
# 	go fuck yourself
putn:
	pushw	%ax           # save %ax to the stack
	movw	$0xd,%ax      # copy the value of the carrage return character into %ax
	callw	putchr        # call the 'putchr' subroutine to print the carrage return character
	movw	$0xa,%ax      # copy the value of the newline character into %ax
	callw	putchr        # call the 'putchr' subroutien to print the newline character
	popw	%ax           # restore %ax from the stack
	retw	              # return from the subroutine



###
# print a character onto the screen
# params:
# 	al	the character vlaue to print onto the screen
putchr:
	# push registers onto the stack
	pushw	%bx
	pushw	%ax

	movw	$0x70,%bx      # set %bx to 0x70, which represents black text on a white background
	movb	$0xe,%ah       # set %ah to 0xe, which is the bios call for printing onto the screen
	int	$0x10          # invoke the interrupt for IO to get the bios to print onto the screen

	# TODO: error handling

	# restore registers from the stack
	popw	%ax
	popw	%bx
	retw	               # return from the subroutine



###
# get the name of the filesystem type
# params:
# 	al	the filesystem type
# returns:
# 	ax	a pointer to the resultign name string
getname:
	# save registers
	pushw	%di
	pushw	%bx
	pushw	%cx

	xorw	%bx,%bx        # reset %bx to zero by XORing itself
	movw	$partindx,%di  # move the partition index table pointer into %di so that we can use it as a pointer

# a loop to scan the partition index table, defined by us, to look for filesystem IDs
getname.0:
	movb	(%di),%bl      # dereference the partition index table to set the current entry into %bl
	testb	%bl,%bl	       # is %bl, the current entry in the partition index table, zero?
	je	getname.1      # if %bl is zero, we know that none of the previous entries matched, so we jump to 'getname.1' to return the unknown string
	cmpb	%al,%bl        # compare the values in %al and %bl to see if the current entry matches the passed in parameter
	je	getname.1      # if the values match, jump to 'getname.1' to return the string

	# if the values don't match
	addw	$0x3,%di       # increment to the next entry in the partition index table
	jmp	getname.0      # jump to 'getname.0' to continue with the loop

getname.1:
	# get string pointer
	incw	%di            # increment the pointer to the current index so that it is now pointing to the pointer to the string
	movw	(%di),%ax      # dereference the poitner to the pointer to the string and store the string pointer in %ax

	# restore registers onto the stack
	popw	%cx
	popw	%bx
	popw	%di
	retw	               # return from the subroutine



###
# get an integer value from the user
# TODO: add a timeout
# returns:
# 	ax	the integer value input by the user
getint:
	# call the bios routine to get a key press
	movb	$0x0,%ah       # move the bios call for get key into %ah
	int	$0x16          # send interrupt 0x16, which calls on the bios to get a key press from the user, which will be stored in %ax
		               # this is a blocking call that won't return until the user has interacted
	# convert to integer
	movb	$0x0,%ah       # set %ah, the higher byte, whose value we ignore, to zero
	subw	$0x30,%ax      # subtract ASCII '0' from the value returned from the bios call

	# correct for hexadecimal
	cmpw	$0x0a,%ax      # compare the value in %ax to 10 to see if it is a HEX value
	jle	getint.0       # if the value in %ax is less than q0, we jump to 'getint.0' to return from the subroutine
	subw	$0x07,%ax      # if the value is HEX, subtract 7 from %ax to shift the value into a number value
		               # note: there are 7 ASCII characters between '9' and 'A'
getint.0:
	retw	               # return from the subroutine



###
# get the drive geometry to be used for CHS math
# params:
# 	dl	the drive number to get the geomerty for
# 		this should be the value that was noted at the top of this file
# returns:
# 	bh	the number of heads on the disk
# 	bl	the number of sectors per track on the disk
# note: the return values are placed within %bx
getgeo:
	# save registers to the stack
	pushw	%di
	pushw	%dx
	pushw	%cx
	pushw	%ax

	movb	$0x8,%ah        # move, 0x8, the bios call for drive geometry into %ah
	movw	$0x0,%di        # move 0x0 into %di so that we can cope with shitty bios implementations (as quoted from somewhere)
	int	$0x13           # thorw the interrupt 0x13 to call the bios to read drive parameters
	movb	%dh,%bh         # move the resulting highest index used for a drive head into %bh
	addb	$0x1,%bh        # add one to the drive number in %bh so that it is the actual number of heads
	movb	%cl,%bl         # move the resulting sectors per track value into %bl
	andb	$0x3f,%bl       # AND 0x3f withthe number of sectors per track in %bl for some reason

	# reload registers from the stack
	popw	%ax
	popw	%cx
	popw	%dx
	popw	%di
	retw	                # return from the subroutine



###
# load the partition whose number is in %ax to the buffer in %bx
# drive number in %dl
# params:
# 	ax	the partition number to load into the buffer
# 	bx	a pointer to the buffer to load the partition into
# 	dl	the drive number to load the partition from
loadprt:
	# save registers onto the stack
	pushw	%di
	pushw	%si
	pushw	%ax
	pushw	%cx
	pushw	%dx
	pushw	%bx

	movw	%bx,%si          # move the pointer to the buffer into %si, the index register, so we can use it as a pointer

	# calculate the index of the individual partition
	movw	$PART_SZ,%di     # set %di to the size of a partition
	pushw	%dx              # save %dx to the stack
	mulw	%di              # multiply %di by %ax and place the result in %ax concatinated with %dx
	popw	%dx              # restore %dx to its pre-multiplication value
	movw	%ax,%di          # push the result of the multiplication, which is the offset of the entry in the table, from %ax into %di

	# convert the index into a pointer, and use that pointer to get the Logical Block Address (LBA) descriptor
	addw	$PART_TBL,%di    # add the base of the partition table to the offset to get the pointer to the partition, and save it into %di
	addw	$0x08,%di        # %di is a pointer to a structure that contains useful data
		                 # we want to add an offset to the pointer to get a pointer to the LBA descriptor field in the structure
	movw	(%di),%cx        # dereference the pointer of the LBA descriptor into %cx to give us the total number of sectors we'll need to read
	movw	0x6(%di),%di     # load the index of the first sector into %di, hoping that it is among the first 65,000 sectors
		                 # the upper 4 bytes aren't useful to us because we are still in real mode (i.e. 16-bit in the 80's)

# a loop to load one sector at a time
loadprt.0:
	pushw	%cx              # push the index in %cx onto the stack
	pushw	%dx              # push the drive number in %dx onto the stack

	# get the drive geometry so that we can convert the LBA descriptor into CHS coordinates
	callw	getgeo	         # call the 'getgeo' subroutine to get the drive geometry
		                 # the result is stored in %bx as concatinated values
		                 # the number of heads is in %bh, and the amount of sectors per track is in %bl
		                 # bx <- number_of_heads:sectors_per_head

	# convert the LBA into CHS
	movw	%di,%ax	         # move %di, the index of the first sector into %ax
		                 # ax <- first sector
	div	%bl              # divide %ax, the absolute index of the sector, by %bl, the number of sectors per track
		                 # division retruns the result of division in %al, and the remainder in %ah
		                 # al <- LBA / sectors per track
		                 # ah <- LBA % sectors per track
	movb	%ah,%bl          # move %ah, the result of LBA % sectors per track, into %bl, which is now the sector to be loaded
		                 # bl <- sector to be read
	movb	$0x0,%ah         # set %ah to zero so that %ax is now the result of LBA / sectors per track
		                 # ax <- LBA / sectors per track
	div	%bh              # divide %ax by %bh, the number of heads, to get the result of (LBA / sectors per track) / number of heads
		                 # now, %al is the cylinder, %ah is the head, and %bl is the sector to read from, so we now have CHS
		                 # al <- (LBA / sectors per track) / number of heads
		                 # ah <- (LBA / sectors per track) % number of heads
		                 # al <- cylinder
		                 # ah <- head
		                 # bl <- sector

	# Congradulations! You got through the horrors of division! Notice that these sentinces are capitalized! That's a huge achievement!

	# this value is extremely important, and %dx is not safe in the following code
	# %dl within %dx is the disk number we are reading from
	popw	%dx              # we want to restore the value of %dx from the stack
	pushw	%dx              # we also want to make sure that we don't lose this value, so we push it back onto the stack

	# move the values calculated earlier into the appropriate registers for the following calls
	movb	%bl,%cl          # move the sector number from %bl into %cl
	movb	%al,%ch          # move the cylinder number from %al into %ch
	movb	%ah,%dh          # move the head number from %ah into %dh
	movw	%si,%bx          # move the buffer pointer from %si into %al to pass it into the next subroutine
	movb	$0x1,%al         # set %al to be 1 so that we only load one sector at a time
	callw	loaddsk          # call the 'loaddsk' subroutine to load the sector at the given CHS point into the buffer

	# increment the buffer
	addw	$SEG_SIZE,%si    # increment the buffer pointer to the next sector to write to
	# increment the CHS
	incw	%di              # increment the LBA index for later use in calculating the next CHS sector location
	popw	%dx              # pop the disk number back into %dx from the stack to restore its value
	popw	%cx              # pop the loop counter from the stack into %cx
	# loop
	loop	loadprt.0        # decrement %cx and jump to 'loadprt.0' if it is not zero

	# if %cx is zero, we are done copying these sectors into memory 
        # and we can resturn from the god forsaken subroutine and continue on to stage 2
	# but first we need to restore all the registers
	popw	%bx
	popw	%dx
	popw	%cx
	popw	%ax
	popw	%si
	popw	%di
	retw                     # finally, thank the lord, return from this god forsaken subroutine



###
# this is the error string that is printed when the bios fails to read from the disk... well shit...
load_error_msg:	.ascii	"LOAD ERROR"
	.byte	0x0              # a zero to end the string 



###
# strings for filesystem type names
oswin:	.ascii	"WINDOWS"        # Micro$oft Windows
	.byte	0x0              # a zero to end the string
oslin:	.ascii	"LINUX"          # that penguin os thing... linux, there's not GNU
	.byte	0x0              # a zero to end the string
osbsd:	.ascii	"BSD"            # Bee Ess Die
	.byte	0x0              # a zero to end the string
osgmb:	.ascii	"GAMBIT"         # the best fucking os that never was until now awesome
	.byte	0x0              # a zero to end the string
osunkn:	.ascii	"UNKNOWN"        # a string to represent the uncertanty in life for the case when we can't figure out what the filesystem type is
	.byte	0x0              # a zero to end the string



###
# this is the partition index table
# it is used to map filesystem IDs to filesystem names
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
