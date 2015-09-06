/**
 *	exc.c
 *
 *	Exception and interrupt handling
 **/

// inclusions
#include "exc.h"

// data members

// private subroutine declarations

void exc_div_zero_handler(long interruptNumber, long errorCode);
void exc_nmi_handler(long interruptNumber, long errorCode);
void exc_breakpoint_handler(long interruptNumber, long errorCode);
void exc_overflow_handler(long interruptNumber, long errorCode);
void exc_bounds_handler(long interruptNumber, long errorCode);
void exc_inv_opcode_handler(long interruptNumber, long errorCode);
void exc_noavail_handler(long interruptNumber, long errorCode);
void exc_double_fault_handler(long interruptNumber, long errorCode);
void exc_seg_overrun_handler(long interruptNumber, long errorCode);
void exc_inv_tss_handler(long interruptNumber, long errorCode);
void exc_seg_abs_handler(long interruptNumber, long errorCode);
void exc_stack_fault_handler(long interruptNumber, long errorCode);
void exc_gp_fault_handler(long interruptNumber, long errorCode);
void exc_page_fault_handler(long interruptNumber, long errorCode);
void exc_x87_handler(long interruptNumber, long errorCode);
void exc_alignment_handler(long interruptNumber, long errorCode);
void exc_machine_check_handler(long interruptNumber, long errorCode);
void exc_simd_fault_handler(long interruptNumber, long errorCode);

// subroutines

/// exc_init
/// Initializes the interrupt handling subsystem
/// params:
///	handlerTable	- pointer to the virtual IDT
/// returns:
///	none
void exc_init( virtual_interrupt_table handlerTable )
{
	// install all our fault handlers
	handlerTable[DIVIDE_BY_ZERO] = exc_div_zero_handler;
	handlerTable[NMI] = exc_nmi_handler;
	handlerTable[BREAKPOINT] = exc_breakpoint_handler;
	handlerTable[OVERFLOW] = exc_overflow_handler;
	handlerTable[BOUND] = exc_bounds_handler;
	handlerTable[INVALID_OPCODE] = exc_inv_opcode_handler;
	handlerTable[DEVICE_NOAVAIL] = exc_noavail_handler;
	handlerTable[DOUBLE_FAULT] = exc_double_fault_handler;
	handlerTable[SEG_OVERRUN] = exc_seg_overrun_handler;
	handlerTable[INV_TSS] = exc_inv_tss_handler;
	handlerTable[SEG_ABSENT] = exc_seg_abs_handler;
	handlerTable[STACK_FAULT] = exc_stack_fault_handler;
	handlerTable[GP_FAULT] = exc_gp_fault_handler;
	handlerTable[PAGE_FAULT] = exc_page_fault_handler;
	handlerTable[X87_FAULT] = exc_x87_handler;
	handlerTable[ALIGNMENT_CHECK] = exc_alignment_handler;
	handlerTable[MACHINE_CHECK] = exc_machine_check_handler;
	handlerTable[SIMD_FAULT] = exc_simd_fault_handler;

	// clean up
	return;
}

void exc_div_zero_handler(long interruptNumber, long errorCode)
{
	putstr("Division by zero exception");
	return;
}

void exc_nmi_handler(long interruptNumber, long errorCode)
{
	putstr("NMI exception");
	return;
}

void exc_breakpoint_handler(long interruptNumber, long errorCode)
{
	putstr("Breakpoint");
	return;
}

void exc_overflow_handler(long interruptNumber, long errorCode)
{
	putstr("Overflow");
	return;
}

void exc_bounds_handler(long interruptNumber, long errorCode)
{
	putstr("Bounds range exceeded");
	return;
}

void exc_inv_opcode_handler(long interruptNumber, long errorCode)
{
	putstr("Invalid opcode");
	return;
}

void exc_noavail_handler(long interruptNumber, long errorCode)
{
	putstr("Hardware not available");
	return;
}

void exc_double_fault_handler(long interruptNumber, long errorCode)
{
	putstr("Double fault!");
	return;
}

void exc_seg_overrun_handler(long interruptNumber, long errorCode)
{
	putstr("Segment overrun");
	return;
}

void exc_inv_tss_handler(long interruptNumber, long errorCode)
{
	putstr("Invalid TSS");
	return;
}

void exc_seg_abs_handler(long interruptNumber, long errorCode)
{
	putstr("Segment not present");
	return;
}

void exc_stack_fault_handler(long interruptNumber, long errorCode)
{
	putstr("Stack fault!");
	return;
}

void exc_gp_fault_handler(long interruptNumber, long errorCode)
{
	putstr("General protection fault");
	putint(errorCode);
	PANIC("We don't handle these yet :-/");
}

void exc_page_fault_handler(long interruptNumber, long errorCode)
{
	putstr("Page fault");
	putint(errorCode);
	putstr(";");
	long cr2;
	asm(
		"mov %%cr2,%0":"=r"(cr2)
	);
	putint(cr2);
	putstr(";");
	PANIC("We don't handle these yet :-/");
}

void exc_x87_handler(long interruptNumber, long errorCode)
{
	putstr("x87 exception");
	return;
}

void exc_alignment_handler(long interruptNumber, long errorCode)
{
	putstr("Alignment check exception");
	return;
}

void exc_machine_check_handler(long interruptNumber, long errorCode)
{
	putstr("Machine check exception");
	return;
}

void exc_simd_fault_handler(long interruptNumber, long errorCode)
{
	putstr("SIMD exception");
	return;
}




