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
	user_allocate( 1 );
	user_allocate( 1 );
	user_allocate( 1 );
	user_allocate( 1 );
	user_allocate( 1 );
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
	// END DEBUG
	return 0;
}

long deallocate( void * pagePointer )
{
	// TODO
	// DEBUG
	putstr( "Deallocating,") ;
	// END DEBUG
	return -1;
}


// TODO: move these to user space
void * user_allocate( long numPages )
{
	asm( "movq	$0x10,%rax" );
	asm( "int	$0x31" );
}

long user_deallocate( void * pagePointer )
{
	asm( "movq	$0x11,%rax" );
	asm( "int	$0x31" );
}

