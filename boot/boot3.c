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

void putstr( char * c );
void dumpmem( meta_mem_t * mem );

// entry point

void start()
{
	// variables
	// function body
	putstr( "Welcome to some C code!" );

	dumpmem( (meta_mem_t*)META_MEM );

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

void dumpmem( meta_mem_t * mem )
{
	// variables

	// function body
	for ( int i = 0; mem[i].type; i++ )
	{
		switch( mem[i].type )
		{
			case 1:
				putstr( "Usable;" );
				break;
			case 2:
				putstr( "Reserved;" );
				break;
			case 3:
				putstr( "Reclaimable;" );
				break;
			case 4:
				putstr( "NVS;" );
				break;
			case 5:
				putstr( "Bad Memory;" );
				break;
		};
	}

	// clean up
}

