/**
 *	vm.c
 *
 *	Virtual memory subsystem code
 **/

// inclusions
#include "kernel.h"
#include "vm.h"

// data members
unsigned char * heap = KERNEL_HEAP_START;
unsigned long * currentVPointer = KERNEL_HEAP_END;
physical_page_t * freeList = 0;
physical_page_t * nonFreeList = 0;	// these pages can be evicted at any time
physical_page_t * memList = 0;
physical_page_t * memableList = 0;

void * currentPt = (void*)0x1C000;
void * currentPdt = (void*)0x12000;
void * currentPdpt = (void*)0x11000;
void * currentPml4t = (void*)0x10000;

// definitions

// subroutines

// privately visible subroutines

void form_page_lists( meta_mem_t * mem );
physical_page_t * vm_get_physical_page();
void * vm_get_pml4t();
void * vm_get_pdpt();
void * vm_get_pdt();
void * vm_get_pt();
void * k_malloc( long blockSize );

// subroutine definitions

/// vm_init
/// initializes the memory subsystem
/// params:
///	mem	- a pointer to the metadata about the system's physical memory
/// returns:
///	none

void vm_init( meta_mem_t * mem )
{
	// variables

	// function body
	form_page_lists( mem );
	// create a memory queue for emergencies
	for ( long i = 0; i < MEMABLE_LIST_SIZE; i++ )
	{
		physical_page_t * p = vm_get_physical_page();
		if ( 0 == p )
		{
			PANIC("Ran out of memory really early");
		}
		p->next = memableList;
		memableList = p;
		PT_ADD(((long*)vm_get_pt()),(p->pagePointer|PG_PRESENT|PG_READWRITE));
		p->pageVPointer = (long)CURRENT_V_POINTER();
	}

	// clean up
	return;
}

physical_page_t * vm_get_memable_page()
{
	if ( 0 == memableList )
	{
		PANIC("Ran out of pages for memory");
	}
	physical_page_t * p = memableList;
	memableList = p->next;
	return p;
}

/// form_page_lists
/// create free list of pages
/// params:
///	mem - a pointer to the list of memory section data
/// returns:
///	none
void form_page_lists( meta_mem_t * mem )
{
	// variables
	physical_page_t * current;

	// function body
	for ( unsigned long i = 0; mem[i].type; i++ )
	{
		// we can only allocate out of free blocks
		if ( FREE_MEM_TYPE == mem[i].type )
		{
			unsigned long base = mem[i].base;
			// we can only allocate out of non-kernel space
			base = base >= KERNEL_HEAP_END ? base : KERNEL_HEAP_END;
			// we can only go off of clean page boundaries
			base = 0 == ( base % PAGE_SIZE ) ? base : base + PAGE_SIZE - ( base % PAGE_SIZE );
			unsigned long end = mem[i].base + mem[i].length;
			// we can only go off of etc. etc.
			end = 0 == ( end % PAGE_SIZE ) ? end : end - ( end % PAGE_SIZE );
			// chunk must have more than zero bytes
			end = end > base ? end : base;
			// ok, now we can split it into pages
			for ( unsigned long j = 0; j < (( end - base ) / PAGE_SIZE); j++ )
			{
				if ( 0 == freeList )
				{
					freeList = k_malloc( sizeof(physical_page_t) );
					current = freeList;
				}
				else
				{
					current->next = k_malloc( sizeof(physical_page_t) );
					current = current->next;
				}
				current->pagePointer = base + j * PAGE_SIZE;
				current->next = 0;
			}
		}
	}

	// clean up
	return;
}

/// vm_get_physical_page
/// a private function that gets the next available physical page from memory
/// and unlinks it from the free list
/// params:
///	none
/// returns:
///	a pointer to the beginning of the physical page
physical_page_t * vm_get_physical_page()
{
	// variables
	physical_page_t * page;

	// function body
	page = freeList;
	if ( 0 != freeList )
	{
		freeList = freeList->next;
	}

	// clean up
	return page;
}

/// vm_get_pml4t
/// get the current pml4 table
/// params:
///	none
/// returns:
///	the pointer to the current pml4t
void * vm_get_pml4t()
{
	void * pml4t;
	asm( "movq %%cr3,%0"
		: "=r"(pml4t) );
	return (void*)((long)pml4t & ~(0x7FF));
}

/// vm_get_pdpt
/// get the current pdp table
/// params:
///	none
/// returns:
/// 	the pointer to the current pdp table
void * vm_get_pdpt()
{
	return currentPdpt;

	long * pml4Table;

	pml4Table = (long*)vm_get_pml4t();
	for ( int i = PML4_TABLE_SIZE - 1; i >= 0; i-- )
	{
		if ( 0 != pml4Table[i] )
		{
			return (void*)(pml4Table[i] & ~(0x7FF));
		}
	}

	// no current pdpt? confusing thought, really
	// implies a pml4 table with no entries; not really possible
	// therefore, panic
	PANIC("PML4 table is empty");
	return 0;
}

/// vm_get_pdt
/// get the current pd table
/// params:
///	none
/// returns:
///	the pointer to the current pd table
void * vm_get_pdt()
{
	return currentPdt;

	long * pdpTable;

	pdpTable = (long*)vm_get_pdpt();
	for ( int i = PDP_TABLE_SIZE - 1; i >= 0; i-- )
	{
		if ( 0 != pdpTable[i] )
		{
			return (void*)(pdpTable[i] & ~(0x7FF));
		}
	}

	// current pdt is not in the current pdpt
	// implies that the current pdpt is "fresh"
	PANIC("vm_get_pdt implementation is incomplete");

	return 0;
}

/// vm_get_pt
/// get the current page table
/// params:
///	none
/// returns:
///	the pointer to the current page table
void * vm_get_pt()
{

	return currentPt;

	long * pdTable;

	pdTable = (long*)vm_get_pdt();
	for ( int i = PD_TABLE_SIZE - 1; i >= 0; i-- )
	{
		if ( 0 != pdTable[i] )
		{
			return (void*)(pdTable[i] & ~(0x7FF));
		}
	}

	PANIC("vm_get_pt implementation is incomplete");

	return 0;
}


/// vm_map_page
/// allocate a virtual page and map it to the specified physical page
/// params:
///	page - a pointer to the beginning of the physical page
/// returns:
///	a pointer to the beginning of the virtual page
void * vm_map_page( void * page )
{
	// variables

	// function body

	// get the current page table
	long * currentPageTable = vm_get_pt();
	// if there is no space left in the page table
	if ( PT_FULL(currentPageTable) )
	{
		// allocate a page for the new page table
		physical_page_t * physPTable = vm_get_physical_page();
		if ( 0 == physPTable )
		{
			// TODO: evict a page and use it
			PANIC("Ran out of Page table pages");
		}
		long pTable = physPTable->pagePointer;
		// link the new page into the memory page list
		physPTable->next = memList;
		memList = physPTable;
		// mark the new page's attributes
		pTable = pTable | PG_PRESENT | PG_READWRITE;
		// add the new page table to the previous page table
		PT_ADD(currentPageTable,pTable);
		// flush the TLB
		FLUSH_TLB();
		// we're in virtual memory, so we have to write to the new page
		// table's virtual address instead of its real one
		long * pVTable = (long*)CURRENT_V_POINTER();
		// clear the new page table
		for ( long i = 0; i < PAGE_TABLE_SIZE; i++ )
		{
			((long*)pVTable)[i] = 0;
		}
		// set it as the current page table for later in this subroutine
		currentPageTable = (long*)pVTable;
		// get the current pdt
		long * currentPdTable = vm_get_pdt();
		// if there is no space left in the pdt
		if ( PDT_FULL(currentPdTable ) )
		{
			// get the current pdpt
			long * currentPdpTable = vm_get_pdpt();
			// if there is no space left in the pdpt
			if ( PDPT_FULL(currentPdpTable) )
			{
				// DEBUG
				putstr("PDPT FULL");
				putint(page);
				// END DEBUG
				// get the current pml4t
				long * currentPml4Table = vm_get_pml4t();
				// if there is no space left in the pml4t
				if ( PML4T_FULL(currentPml4Table) )
				{
					// panic
					PANIC("Ran out of virtual address space");
				}
				// allocate a page for a new pdpt
				physical_page_t * physPDPTable = vm_get_memable_page();
				if ( 0 == physPDPTable )
				{
					PANIC("Ran out of pdpt pages");
				}
				long pdpTable = physPDPTable->pagePointer;
				physPDPTable->next = memList;
				memList = physPDPTable;
				// clear the physical page
				long * pdpVTable = (long*)physPDPTable->pageVPointer;
				for ( long i = 0; i < PAGE_TABLE_SIZE; i++ )
				{
					((long*)pdpVTable)[i] = 0;
				}
				currentPdpTable = (long*)pdpVTable;
				currentPdpt = pdpVTable;
				// replace the memable page that we borrowed
				physical_page_t * replacement = vm_get_physical_page();
				if ( 0 == replacement )
				{
					PANIC("Need extra memory again");
				}
				replacement->next = memableList;
				memableList = replacement;
				PT_ADD(pVTable,(replacement->pagePointer|PG_PRESENT|PG_READWRITE));
				replacement->pageVPointer = (long)CURRENT_V_POINTER();
				// mark all of its attributes appropriately
				pdpTable = pdpTable | PG_PRESENT | PG_READWRITE;
				// add the new pdpt to the current pml4t
				PDT_ADD(currentPml4Table,pdpTable);
				putstr("PDPT NO LONGER FULL");
			}
			// allocate a page for a new pdt
			physical_page_t * physPDTable = vm_get_memable_page();
			if ( 0 == physPDTable )
			{
				PANIC("Ran out of pd table pages");
			}
			long pdTable = physPDTable->pagePointer;
			// link the new page into the memory page list
			physPDTable->next = memList;
			memList = physPDTable;
			// mark the new page's attributes
			pdTable = pdTable | PG_PRESENT | PG_READWRITE;
			long * pdVTable = (long*)physPDTable->pageVPointer;
			// clear the new pd table
			for ( long i = 0; i < PAGE_TABLE_SIZE; i++ )
			{
				pdVTable[i] = 0;
			}
			// set it as the current pd table for later in this subroutine
			currentPdTable = (long*)pdVTable;
			currentPdt = pdVTable;
			// replenish the memable page that we borrowed
			physical_page_t * replacement = vm_get_physical_page();
			if ( 0 == replacement )
			{
				PANIC("Need extra memory");
			}
			replacement->next = memableList;
			memableList = replacement;
			PT_ADD(pVTable,(replacement->pagePointer|PG_PRESENT|PG_READWRITE));
			replacement->pageVPointer = (long)CURRENT_V_POINTER();
			// add the new pdt to the pdpt
			PDT_ADD(currentPdpTable,pdTable);
			FLUSH_TLB();
		}
		// add the new page table to the page directory table
		PDT_ADD(currentPdTable,pTable);
		// mark this page table as the current page table for the entire subsystem
		currentPt = pVTable;
		// flush the TLB so this all takes effect
		FLUSH_TLB();
	}
	// mark the physical page's attributes appropriately
	long pgLong = (long)page;
	pgLong = pgLong | PG_PRESENT | PG_READWRITE | PG_USER;
	// add the physical page to the current page table
	PT_ADD(currentPageTable,pgLong);
	// flush the TLB
	FLUSH_TLB();

	// clean up
	return CURRENT_V_POINTER();
}

/// vm_allocate_page
/// allocate a page of memory
/// params:
///	none
/// returns:
///	a pointer to the beginning of the virtual page
void * vm_allocate_page()
{
	// variables
	physical_page_t * physicalPage;
	void * virtualPage;

	// function body 
	// try to get a physical page
	physicalPage = vm_get_physical_page();
	// if we could not get a physical page
	if ( 0 == physicalPage )
	{
		// TODO: paging!
		PANIC("We don't do paging yet :-/");
		// get the most evictable virtual page
		// get its physical pointer
		// evict the virtual page
	}
	// map the physical page into virtual memory
	virtualPage = vm_map_page( ((void*)(physicalPage->pagePointer)) );
	// put it in the evictable list
	physicalPage->next = nonFreeList;
	nonFreeList = physicalPage;

	// clean up
	return virtualPage;
}

/// k_malloc
/// allocate a block of memory explicitly for use by the kernel
/// params:
///	blockSize - the number of bytes to allocate
/// returns:
///	a pointer to some 'fresh' memory
void * k_malloc( long blockSize )
{
	// variables
	unsigned char * current;

	// function body
	current = heap;
	heap = heap + blockSize;
	if ( (long)heap > KERNEL_HEAP_END )
	{
		PANIC("Ran out of kernel heap.");
	}

	// clean up
	return current;
}

