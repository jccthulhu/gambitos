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

// entry point

void start()
{
	// variables
	// function body
	putstr( "Welcome to some C code!" );

	form_page_lists( (meta_mem_t*)META_MEM );

	for (;;)
	{
		// as my old comp sci teacher once said:
		// "operating systems are easy; if nothing happens, do nothing"
		asm( "hlt" );
	}

	// clean up
	// next point
}

// data members

char * currentVideo = VIDEO_MEM;
physical_page_t * freeList = 0;
physical_page_t * nonFreeList = 0;
unsigned char * heap = KERNEL_HEAP_START;

// routine definitions

void putchr( char c )
{
	// variables

	// function body
	// write the character
	currentVideo[0] = c;
	// write the attributes
	currentVideo[1] = 0x70;
	// increment
	currentVideo += 2;
	// maybe wrap
	if ( ((int)( currentVideo - VIDEO_MEM )) > VIDEO_MEM_SIZE ) { currentVideo = (char*)VIDEO_MEM; }

	// clean up
	return;
}

void putstr( char * c )
{
	// variables
	// function body
	while ( c[0] )
	{
		putchr( *c++ );
	}

	// clean up
}

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

