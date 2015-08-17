# the executable names of the desired as and ld commands
AS_CMD=x86_64-unknown-elf-as
LD_CMD=x86_64-unknown-elf-ld

# set AS and LD to the executable for the as and ld commands
ifneq ($(strip ${GNU_x86_64_UNKNOWN_ELF_PATH}),)
# if $GNU_x86_64_UNKNOWN_ELF_PATH is defined in the environment, use it
AS=${GNU_x86_64_UNKNOWN_ELF_PATH}/${AS_CMD}
LD=${GNU_x86_64_UNKNOWN_ELF_PATH}/${LD_CMD}
else
# if there is no path defined, rely on the user's path to find the executables
AS=${AS_CMD}
LD=${LD_CMD}
endif

CC=clang

all:	create_bin

run-qemu:	all
	qemu-system-x86_64 -hda boot.img

create_bin:	buildboot1 buildboot2 buildboot3 buildparttbl
	hdiutil create -megabytes 64 -fs MS-DOS -fsargs "-F 32" -volname FAT32 -o boot.dmg
	dd if=boot1 of=boot.dmg conv=notrunc
	dd if=parttbl of=boot.dmg conv=notrunc seek=1 count=1 bs=512
	dd if=boot2 of=boot.dmg conv=notrunc seek=2 count=8 bs=512
	dd if=boot3 of=boot.dmg conv=notrunc seek=10 count=8 bs=512
	mv boot.dmg boot.img

buildparttbl:
	# it's going to be EXACTLY 1 sector
	touch parttbl
	dd if=/dev/zero of=parttbl count=1 bs=512
	./write_boot parttbl_tmp
	dd if=parttbl_tmp of=parttbl conv=notrunc

buildboot1:
	$(AS) boot/boot1.s -o boot1.o
	$(LD) -o boot1 -Ttext 0x600 -e start -S -N --oformat binary boot1.o

buildboot2:
	$(AS) boot/boot2.s -o boot2.o
	$(LD) -o boot2 -Ttext 0x7c00 -e start -S -N --oformat binary boot2.o

buildboot3:
	$(CC) -target x86_64-unknown-elf -nostartfiles -c boot/boot3.c -o boot3.o
	$(LD) -o boot3 -Ttext 0x8c00 -e start -S -N --oformat binary boot3.o

clean:
	rm -f boot1 boot1.o boot2 boot2.o boot3 boot3.o boot.dmg boot.img parttbl
