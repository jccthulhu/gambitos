/**
 *	boot3.c
 *
 *	"Third" stage bootloader; gets loaded with the second stage, but is the first C code we can use in the boot process
 **/

// definitions

#define	VIDEO_MEM	0xb8000
#define	VIDEO_MEM_SIZE	0xfa0

void putchr( char );
void putstr( char * c );

// entry point

void start()
{
	// variables
	// function body
	putstr( "Welcome to some C code!" );

	for (;;){}

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
	currentVideo[1] = 0x07;
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
