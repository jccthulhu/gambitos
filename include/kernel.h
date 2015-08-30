/**
 *	kernel.h
 *
 *	definitions useful for kernel stuff
 **/

// guard
#ifndef	KERNEL_H_
#define	KERNEL_H_

// inclusions

// definitions

#define PANIC(x)        putstr( "Woops." ); putstr(x); for (;;) {}

// subroutine definitions

void putchr(char character);
void putstr(char *string);
void putint(long value);

#endif
