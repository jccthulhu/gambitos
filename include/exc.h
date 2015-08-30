/**
 *	exc.h
 *
 *	Exception and interrupt handling
 **/

#ifndef	EXC_H_
#define	EXC_H_

// inclusions

// type definitions
typedef (*interrupt_handler)(long interruptHandler, long errorCode);

typedef	interrupt_handler	fault_handler;

typedef	virtual_interrupt_table	* interrupt_handler;

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

