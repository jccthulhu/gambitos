/**
 *	boot3.c
 *
 *	"Third" stage bootloader; gets loaded with the second stage, but is
 *	the first C code we can use in the boot process
 **/

// inclusions
#include "process.h"

// definitions

#define	VIDEO_MEM	0xb8000
#define	VIDEO_MEM_SIZE	0xfa0

#define	SYSCALL_TABLE	0x14280		// TODO: this passed from the bootloader

// utilities
void putstr( char * c );
void putint( long v );
void installSyscalls( long * syscallTable );

// system calls!
void * allocate( long numPages );
long deallocate( void * pagePointer );

// user side system calls
void * user_allocate( long numPages );
long user_deallocate( void * pagePointer );

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
	// schedule the timer, round robin style
	// jump out to user space

	// DEBUG
	putint( user_allocate( 0x1af ) );
	// END DEBUG

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
	
	// clean up
	return;
}


void * allocate( long numPages )
{
	// TODO
	// DEBUG
	putstr( "Allocating," );
	putint( numPages );
	// END DEBUG
	return 0x145f;
}

long deallocate( void * pagePointer )
{
	// TODO
	// DEBUG
	putstr( "Deallocating,") ;
	// END DEBUG
	return -1;
}


