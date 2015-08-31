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

BUILD_DIR=build/

all:	create_bin

run-qemu: all
	qemu-system-x86_64 -hda ${BUILD_DIR}boot.img -m 64

build_dir:
	mkdir -p build

write_boot: build_dir
	$(CC) write_boot.c -o ${BUILD_DIR}write_boot

create_bin: buildboot1 buildboot2 buildboot3 buildparttbl build_dir
	hdiutil create -megabytes 64 -fs MS-DOS -fsargs "-F 32" -volname FAT32 -o ${BUILD_DIR}boot.dmg
	dd if=${BUILD_DIR}boot1 of=${BUILD_DIR}boot.dmg conv=notrunc
	dd if=${BUILD_DIR}parttbl of=${BUILD_DIR}boot.dmg conv=notrunc seek=1 count=1 bs=512
	dd if=${BUILD_DIR}boot2 of=${BUILD_DIR}boot.dmg conv=notrunc seek=2 count=8 bs=512
	dd if=${BUILD_DIR}boot3 of=${BUILD_DIR}boot.dmg conv=notrunc seek=10 count=16 bs=512
	mv ${BUILD_DIR}boot.dmg ${BUILD_DIR}boot.img

buildparttbl: write_boot build_dir
	# it's going to be EXACTLY 1 sector
	touch ${BUILD_DIR}parttbl
	dd if=/dev/zero of=${BUILD_DIR}parttbl count=1 bs=512
	./${BUILD_DIR}write_boot ${BUILD_DIR}parttbl_tmp
	dd if=${BUILD_DIR}parttbl_tmp of=${BUILD_DIR}parttbl conv=notrunc

buildboot1: build_dir
	$(AS) boot/boot1.s -o ${BUILD_DIR}boot1.o
	$(LD) -o ${BUILD_DIR}boot1 -Ttext 0x600 -e start -S -N --oformat binary ${BUILD_DIR}boot1.o

buildboot2: build_dir
	$(AS) boot/boot2.s -o ${BUILD_DIR}boot2.o
	$(LD) -o ${BUILD_DIR}boot2 -Ttext 0x7c00 -e start -S -N --oformat binary ${BUILD_DIR}boot2.o

buildboot3: build_dir
	$(CC) -Iinclude -target x86_64-unknown-elf -nostartfiles -c boot/boot3.c -o ${BUILD_DIR}boot3.o
	$(CC) -Iinclude -target x86_64-unknown-elf -nostartfiles -c vm/vm.c -o ${BUILD_DIR}vm.o
	$(CC) -Iinclude -target x86_64-unknown-elf -nostartfiles -c exc/exc.c -o ${BUILD_DIR}exc.o
	$(LD) -o ${BUILD_DIR}boot3 -Ttext 0x8c00 -e start -S -N --oformat binary ${BUILD_DIR}boot3.o ${BUILD_DIR}vm.o ${BUILD_DIR}exc.o

clean:
	rm -Rf ${BUILD_DIR}
