/**
 *	boot3.c
 *
 *	"Third" stage bootloader; gets loaded with the second stage, but is the first C code we can use in the boot process
 **/

// inclusions
#include "vm.h"

// definitions

#define	VIDEO_MEM	0xb8000
#define	VIDEO_MEM_SIZE	0xfa0
#define	META_MEM	0x500

#define	PANIC(x)	putstr( "Woops." ); putstr(x); for (;;) {}

#pragma pack(0)

//////
/// Data Structures and Constant Values

/// struct video_memory
/// a structure that represents a single location within video memory
/// the structure conforms to how the video memory is read to display information onto the screen
struct video_memory {

	/// the character contained within this location in memory
	unsigned char character;

	/// an attribute value describing how the character appears on the screen
	unsigned char display_attribute;
};

/// a pointer to the first entry in video memory
/// this can be treated as an array of all locations within video memory
static struct video_memory *const video_memory = (void *)0xb8000;

/// amount of entries within video memory
/// this is the total size of the video memory entries array
static const unsigned int video_memory_size = (0xfa0 * sizeof(char)) / sizeof(struct video_memory);

physical_page_t * freeList = 0;
physical_page_t * nonFreeList = 0;
unsigned char * heap = KERNEL_HEAP_START;
long * currentVPointer = KERNEL_HEAP_END;
/// the empty character that represents the termination of a string
static const char null_character = '\0';

//////
/// Utility Functions

/// putchr
/// print a character onto the next available space on the screen
/// params:
///	character	- the string to print onto the screen
/// returns:
///	none
/// post:
///	characters will be printed in the next available spaces on the screen
///	when the end of the screen is reached, characters will be written over from the beginning
void putchr(char character)
{
	// a value to keep track of the current location that will be printed to on the screen
	static unsigned int current_index = 0;

	video_memory[current_index].character = character;
	video_memory[current_index].display_attribute = 0x70;

	current_index ++;
	if (current_index > video_memory_size) {
		current_index = 0;
	}
}

/// putstr
/// print a null-terminated string onto the screen
/// params:
///	string	- the string to print onto the screen
/// returns:
///	none
/// post:
///	characters will be printed in the next available spaces on the screen
///	when the end of the screen is reached, characters will be written over from the beginning
void putstr(char *string)
{
	for (; string[0] != null_character; string++) {
		putchr(string[0]);
	}
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

/// vm_alloc_page
/// allocate a page of memory
/// params:
///	none
/// returns:
///	a pointer to the beginning of the virtual page
void * vm_alloc_page()
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

//////
/// Bootloader Stage 3 Entry Point and Execution

/// start
/// the entry point into our C code from stage 2 of the bootloader
/// from here, we can finish up the bootloading sequence and continue on to initialize the kernel
void start()
{
	putstr("Welcome to some C code!");

	form_page_lists( (meta_mem_t*)META_MEM );

	// as my old comp sci teacher once said:
	// "operating systems are easy; if nothing happens, do nothing"
	for (;;) { asm( "hlt" ); }
}

