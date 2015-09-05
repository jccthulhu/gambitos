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

#define	SYSCALL_TABLE	0x1D280

// subroutine definitions

void putchr(char character);
void putstr(char *string);
void putint(long value);

#endif
