/**
 *	boot3.c
 *
 *	"Third" stage bootloader; gets loaded with the second stage, but is the first C code we can use in the boot process
 **/

// inclusions
#include "exc.h"
#include "kernel.h"
#include "vm.h"

// definitions

#define	VIDEO_MEM	0xb8000
#define	VIDEO_MEM_SIZE	0xfa0
#define	META_MEM	0x500

#define	VIDT		(0x19000)
#define	SYSTBL		(VIDT+0x180)

#pragma pack(0)

/// private function definitions
void install_syscalls( long * syscallTable );

//////
/// Bootloader Stage 3 Entry Point and Execution

/// start
/// the entry point into our C code from stage 2 of the bootloader
/// from here, we can finish up the bootloading sequence and continue on to initialize the kernel
void start()
{
	putstr("Welcome to some C code!");

	// DEBUG
	//((long*)VIDT)[ 0x0E ] = fault_handler;
	// END DEBUG

	vm_init( (meta_mem_t*)META_MEM );
	exc_init( (virtual_interrupt_table)VIDT );

	install_syscalls( (long*)SYSTBL );

	// test memory allocation
	long * p;
	// allocate enough to force creation of multiple page tables
	p = vm_allocate_page();
	putint(p);
	p[0] = 0;
	p[511] = 0;
	putstr(";");
	p = vm_allocate_page(0);
	putint(p);
	p[0] = 0;
	p[511] = 0;

	// as my old comp sci teacher once said:
	// "operating systems are easy; if nothing happens, do nothing"
	for (;;) { asm( "hlt" ); }
}

//////
/// Data Structures and Constant Values

/// struct video_memory
/// a structure that represents a single location within video memory
/// the structure conforms to how the video memory is read to display information onto the screen
struct video_memory {

	/// the character contained within this location in memory
	unsigned char character;

	/// an attribute value describing how the character appears on the screen
	unsigned char display_attribute;
};

/// a pointer to the first entry in video memory
/// this can be treated as an array of all locations within video memory
static struct video_memory *const video_memory = (void *)0xb8000;

/// amount of entries within video memory
/// this is the total size of the video memory entries array
static const unsigned int video_memory_size = (0xfa0 * sizeof(char)) / sizeof(struct video_memory);

/// the empty character that represents the termination of a string
static const char null_character = '\0';

//////
/// Utility Functions

/// putchr
/// print a character onto the next available space on the screen
/// params:
///	character	- the string to print onto the screen
/// returns:
///	none
/// post:
///	characters will be printed in the next available spaces on the screen
///	when the end of the screen is reached, characters will be written over from the beginning
void putchr(char character)
{
	// a value to keep track of the current location that will be printed to on the screen
	static unsigned int current_index = 0;

	video_memory[current_index].character = character;
	video_memory[current_index].display_attribute = 0x70;

	current_index ++;
	if (current_index > video_memory_size) {
		current_index = 0;
	}
}

/// putstr
/// print a null-terminated string onto the screen
/// params:
///	string	- the string to print onto the screen
/// returns:
///	none
/// post:
///	characters will be printed in the next available spaces on the screen
///	when the end of the screen is reached, characters will be written over from the beginning
void putstr(char *string)
{
	for (; string[0] != null_character; string++) {
		putchr(string[0]);
	}
}

/// putint
/// print and integer onto the screen
/// params:
///	value	- the integer to print
/// returns:
///	none
void putint(long value)
{
	char c[16];

	for ( int i = 15; i >= 0; i-- )
	{
		c[i] = value & 0x0f;
		c[i] = c[i] >= 10 ? (c[i] - 10) + 'A' : c[i] + '0';
		value = value >> 4;
	}
	for ( int i = 0; i < 16; i++) 
	{
		putchr( c[i] );
	}
}

/// install_syscalls
/// install all the system calls
/// params:
///	syscallTable	- The table of system calls
/// returns:
///	none
void install_syscalls( long * syscallTable )
{
	syscallTable[ 0x10 ] = (long)vm_allocate_page;
	syscallTable[ 0x12 ] = (long)vm_access_page;
}
