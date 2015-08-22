/**
 *	boot3.c
 *
 *	"Third" stage bootloader; gets loaded with the second stage, but is the first C code we can use in the boot process
 **/

// inclusions
#include "vm.h"

// definitions

#define	VIDEO_MEM	0xb8000
#define	VIDEO_MEM_SIZE	0xfa0
#define	META_MEM	0x500

#define	PANIC()		putstr( "Woops." ); for (;;) {}

void putstr( char * c );
void * k_malloc( long blockSize );
void form_page_lists( meta_mem_t * mem );
#pragma pack(0)


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

physical_page_t * freeList = 0;
physical_page_t * nonFreeList = 0;
unsigned char * heap = KERNEL_HEAP_START;
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

/// k_malloc
/// allocate a block of memory explicitly for use by the kernel
/// params:
///	blockSize - the number of bytes to allocate
/// returns:
///	a pointer to some 'fresh' memory
void * k_malloc( long blockSize )
{
	// variables
	unsigned char * current;

	// function body
	current = heap;
	heap = heap + blockSize;
	if ( (long)heap > KERNEL_HEAP_END )
	{
		PANIC();
	}

	// clean up
	return current;
}

/// form_page_lists
/// create free list of pages
/// params:
///	mem - a pointer to the list of memory section data
/// returns:
///	none
void form_page_lists( meta_mem_t * mem )
{
	// variables
	physical_page_t * current;

	// function body
	for ( unsigned long i = 0; mem[i].type; i++ )
	{
		// we can only allocate out of free blocks
		if ( FREE_MEM_TYPE == mem[i].type )
		{
			unsigned long base = mem[i].base;
			// we can only allocate out of non-kernel space
			base = base >= KERNEL_HEAP_END ? base : KERNEL_HEAP_END;
			// we can only go off of clean page boundaries
			base = 0 == ( base % PAGE_SIZE ) ? base : base + PAGE_SIZE - ( base % PAGE_SIZE );
			unsigned long end = mem[i].base + mem[i].length;
			// we can only go off of etc. etc.
			end = 0 == ( end % PAGE_SIZE ) ? end : end - ( end % PAGE_SIZE );
			// chunk must have more than zero bytes
			end = end > base ? end : base;
			// ok, now we can split it into pages
			for ( unsigned long j = 0; j < (( end - base ) / PAGE_SIZE); j++ )
			{
				if ( 0 == freeList )
				{
					freeList = k_malloc( sizeof(physical_page_t) );
					current = freeList;
				}
				else
				{
					current->next = k_malloc( sizeof(physical_page_t) );
					current = current->next;
				}
				current->pagePointer = base + j * PAGE_SIZE;
			}
		}
	}

	// clean up
	return;
}

//////
/// Bootloader Stage 3 Entry Point and Execution

/// start
/// the entry point into our C code from stage 2 of the bootloader
/// from here, we can finish up the bootloading sequence and continue on to initialize the kernel
void start()
{
	putstr("Welcome to some C code!");

	form_page_lists( (meta_mem_t*)META_MEM );

	// as my old comp sci teacher once said:
	// "operating systems are easy; if nothing happens, do nothing"
	for (;;) { asm( "hlt" ); }
}

