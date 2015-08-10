AS=/Users/jcc/gnu/bin/x86_64-unknown-elf-as
LD=/Users/jcc/gnu/bin/x86_64-unknown-elf-ld
CC=clang
INC=-I./include

all:	create_bin

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
	$(CC) $(INC) -target x86_64-unknown-elf -nostartfiles -c boot/boot3.c -o boot3.o
	$(AS) boot/kern_assist.s -o kern_assist.o
	$(LD) -o boot3 -Ttext 0x8c00 -e start -S -N --oformat binary boot3.o kern_assist.o

clean:
	rm -f boot1 boot1.o boot2 boot2.o boot3 boot3.o boot.dmg boot.img parttbl
