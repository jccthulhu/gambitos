/**
 *	boot3.c
 *
 *	"Third" stage bootloader; gets loaded with the second stage, but is
 *	the first C code we can use in the boot process
 **/

// definitions

#define	VIDEO_MEM	0xb8000
#define	VIDEO_MEM_SIZE	0xfa0

void putstr( char * c );

// entry point

void start()
{
	// variables
	// function body
	putstr( "Welcome to some C code!" );

	// install actual interrupt handlers
	// set up actual page tables
	// load up the bare minimum set of processes we need to run this piece
	// schedule the timer, round robin style

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
