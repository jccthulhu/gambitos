/**
 *	boot3.c
 *
 *	"Third" stage bootloader; gets loaded with the second stage, but is
 *	the first C code we can use in the boot process
 **/

// inclusions
#include "process.h"
#include "vm.h"

// definitions

#define	VIDEO_MEM	0xb8000
#define	VIDEO_MEM_SIZE	0xfa0

#define	SYSCALL_TABLE	0x14280		// TODO: this passed from the bootloader

// utilities
void putstr( char * c );
void putint( long v );
void installSyscalls( long * syscallTable );
// TODO: nix this for a better version
pml4_table_t * get_pml4t();

// system calls!
void * allocate( long numPages );
long deallocate( void * pagePointer );
void * vm_map_page( void * pageStart );

// user side system calls
void * user_allocate( long numPages );
long user_deallocate( void * pagePointer );
void * user_vm_map_page( void * pageStart );

// data definitions
extern pml4_table_t * pml4t;

// entry point

void start()
{
	// variables
	// function body
	putstr( "Welcome to some C code!" );

	// install actual interrupt handlers
	// install system call handlers
	installSyscalls( ((long*)SYSCALL_TABLE) );
	// set up actual page tables
	// load up the bare minimum set of processes we need to run this piece
	// start the scheduler, round robin style
	// just hang out until the scheduler fires

	// BEGIN DEBUG
	//pml4t = get_pml4t();
	// END DEBUG

	char * vidMem = (char*)vm_map_page( (long)0xb8000 );
	//putint( (long)vidMem );
	//vidMem[0] = '?';
	//vidMem[1] = 0x70;

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
pml4_table_t * pml4t = 0;
unsigned long pageCounter = 511;

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
	if ( ((int)( currentVideo - VIDEO_MEM )) > VIDEO_MEM_SIZE )
	{ 
		currentVideo = (char*)VIDEO_MEM;
	}

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

void putint( long v )
{
	// variables
	char vals[16];

	// function body
	for ( int i = 0; i < 16; i++ )
	{
		vals[i] = v & 0x0f;	// grab the lowest nibble
		if ( vals[i] > 9 )
		{
			vals[i] = (vals[i] - 10) + 'A';
		}
		else
		{
			vals[i] = vals[i] + '0';
		}
		v = v >> 0x4;
	}
	for ( int i = 15; i >= 0; i-- )
	{
		putchr( vals[i] );
	}

	// clean up
	return;
}

void installSyscalls( long * syscallTable )
{
	// variables
	
	// function body
	syscallTable[ 0x10 ] = allocate;
	syscallTable[ 0x11 ] = deallocate;
	syscallTable[ 0x12 ] = vm_map_page;
	
	// clean up
	return;
}


void * allocate( long numPages )
{
	// TODO
	return 0;
}

long deallocate( void * pagePointer )
{
	// TODO
	return -1;
}

void * vm_allocate_page()
{
	// variables

	// function body

	// clean up
	return 0;
}

void * vm_map_page( void * pageStart )
{
	// variables
	void * virtualPointer;

	// function body
	// TODO
	// pointer must be page aligned
	if ( 0 != ( (long)pageStart & 0xfff ) )
	{
		return 0;
	}
	// TODO: check permissions
	// clean up
	return virtualPointer;
}

