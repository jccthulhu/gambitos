/**
 *	exc.h
 *
 *	Exception and interrupt handling
 **/

#ifndef	EXC_H_
#define	EXC_H_

// inclusions
#include "kernel.h"

// type definitions
typedef void (*interrupt_handler)(long interruptHandler, long errorCode);

typedef	interrupt_handler	fault_handler;

typedef	interrupt_handler 	*virtual_interrupt_table;

// other definitions

#define	DIVIDE_BY_ZERO	0x0
#define	NMI		0x2
#define	BREAKPOINT	0x3
#define	OVERFLOW	0x4
#define	BOUND		0x5
#define	INVALID_OPCODE	0x6
#define	DEVICE_NOAVAIL	0x7
#define	DOUBLE_FAULT	0x8
#define	SEG_OVERRUN	0x9
#define	INV_TSS		0xA
#define	SEG_ABSENT	0xB
#define	STACK_FAULT	0xC
#define	GP_FAULT	0xD
#define	PAGE_FAULT	0xE
#define	X87_FAULT	0x10
#define	ALIGNMENT_CHECK	0x11
#define	MACHINE_CHECK	0x12
#define	SIMD_FAULT	0x13

// data members

// subroutine declarations

/// exc_init
/// Initializes the interrupt handling subsystem
/// params:
///	handlerTable	- pointer to the virtual IDT
/// returns:
///	none
void exc_init( virtual_interrupt_table handlerTable );

#endif

