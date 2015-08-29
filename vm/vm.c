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
physical_page_t * freeList = 0;
physical_page_t * nonFreeList = 0;

// definitions

// subroutines

// privately visible subroutines

void form_page_lists( meta_mem_t * mem );
void * vm_get_physical_page();
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

	// clean up
	return;
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
/// params:
///	none
/// returns:
///	a pointer to the beginning of the physical page
void * vm_get_physical_page()
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
	asm( "movq %%cr0,%0"
		: "=r"(pml4t) );
	return pml4t;
}

/// vm_get_pdpt
/// get the current pdp table
/// params:
///	none
/// returns:
/// 	the pointer to the current pdp table
void * vm_get_pdpt()
{
	long * pml4Table;

	pml4Table = (long*)vm_get_pml4t();
	for ( int i = PML4_TABLE_SIZE - 1; i >= 0; i-- )
	{
		if ( 0 != pml4Table[i] )
		{
			return (void*)(pml4Table[i]);
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
	long * pdpTable;

	pdpTable = (long*)vm_get_pdpt();
	for ( int i = PDP_TABLE_SIZE - 1; i >= 0; i-- )
	{
		if ( 0 != pdpTable[i] )
		{
			return (void*)(pdpTable[i]);
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
	long * pdTable;

	pdTable = (long*)vm_get_pdt();
	for ( int i = PD_TABLE_SIZE - 1; i >= 0; i-- )
	{
		if ( 0 != pdTable[i] )
		{
			return (void*)(pdTable[i]);
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
		// get the current pdt
		long * currentPdTable = vm_get_pdt();
		// if there is no space left in the pdt
		if ( PDT_FULL(currentPdTable ) )
		{
			// get the current pdpt
			// if there is no space left in the pdpt
				// get the current pml4t
				// if there is no space left in the pml4t
					// panic
				// allocate a page for a new pdpt
				// mark all of its attributes appropriately
				// clear the physical page
				// add the new pdpt to the current pml4t
			// allocate a page for a new pdt
			// mark all of its attributes appropriately
			// clear the physical page
			// add the new pdt to the pdpt
		}
		// allocate a page for the new page table
		long * physicalPageTable = vm_get_physical_page();
		if ( 0 == physicalPageTable )
		{
			PANIC("Ran out of space for pd tables");
		}
		// mark all of its attributes appropriately
		long pTable = (long)physicalPageTable;
		pTable = pTable | PG_PRESENT | PG_READWRITE;
		// clear the physical page
		for ( long i = 0; i < PAGE_SIZE/sizeof(long); i++ )
		{
			physicalPageTable[i] = 0;
		}
		// add the new page table to the current pdt

	}
	// mark the physical page's attributes appropriately
	long pgLong = (long)page;
	pgLong = pgLong | PG_PRESENT | PG_READWRITE | PG_USER;
	// add the physical page to the current page table
	PT_ADD(currentPageTable,pgLong);
	// calculate the new virtual pointer

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
	void * virtualPage;
	void * physicalPage;

	// function body 
	// try to get a physical page
	// if we could not get a physical page
		// get the most evictable virtual page
		// get its physical pointer
		// evict the virtual page
	// map the physical page into virtual memory
	virtualPage = vm_map_page( physicalPage );

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

