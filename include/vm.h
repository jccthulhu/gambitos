/**
 *	vm.h
 *
 *	definitions for virtual memory subsystem
 **/

#ifndef	VM_H_
#define	VM_H_

// inclusions

// definitions
#define	PAGE_TABLE_SIZE	512
#define	PD_TABLE_SIZE	512
#define	PDP_TABLE_SIZE	512
#define	PML4_TABLE_SIZE	512

#define	TOTAL_MEM_SIZE	(32*1024*1024)

#define	KERNEL_HEAP_START	0x100000
#define	KERNEL_HEAP_END		0x3FF000

#define	FREE_MEM_TYPE		0x01
#define	PAGE_SIZE		0x1000

#define	PG_PRESENT	0x01
#define	PG_READWRITE	0x02
#define	PG_USER		0x04

#define	PT_FULL(x)	(0 != x[PAGE_TABLE_SIZE-2])
#define	PDT_FULL(x)	(0 != x[PD_TABLE_SIZE-1])
#define	PDPT_FULL(x)	(0 != x[PDP_TABLE_SIZE-1])
#define	PML4T_FULL(x)	(0 != x[PML4_TABLE_SIZE-1])

#define	PT_ADD(x,y)	{ for ( long i = 0; i < PAGE_TABLE_SIZE; i++ ) {\
				if ( 0 == x[i] ) {\
					x[i] = (y);\
					currentVPointer = currentVPointer + 0x200;\
					break;\
				} }\
			}
#define	PDT_ADD(x,y)	{ for ( int i = 0; i < PAGE_TABLE_SIZE; i++ ) {\
				if ( 0 == x[i] ) {\
					x[i] = (y); break;\
				} }\
			}
#define	PDT_REPLACE(x,y)	{ for ( int i = 0; i < PAGE_TABLE_SIZE; i++ ) {\
					if ( 0 == x[i+1] ) { x[i] = y; }\
				}\
			}
#define	CURRENT_V_POINTER()	(currentVPointer - 0x200)

#define	FLUSH_TLB()	{ long cr3;\
				asm("movq %%cr3,%0":"=r"(cr3) );\
				asm("movq %0,%%cr3":"=r"(cr3):"r"(cr3) );\
			}

// type definitions
typedef	long		page_t;
typedef	page_t		* page_table_t;//[PAGE_TABLE_SIZE];
typedef	page_table_t	* pd_table_t;//[PD_TABLE_SIZE];
typedef	pd_table_t	* pdp_table_t;//[PDP_TABLE_SIZE];
typedef	pdp_table_t	* pml4_table_t;//[PML4_TABLE_SIZE];

typedef struct __attribute__((packed))
{
	unsigned long base;
	unsigned long length;
	int type;
} meta_mem_t;

typedef struct physical_page_str
{
	unsigned long pagePointer;
	struct physical_page_str * next;
} physical_page_t;

// publicly visible function declarations

void vm_init();
void * vm_allocate_page();
void * vm_map_page( void * pageStart );

// data
extern unsigned long * currentVPointer;	// TODO: This more reasonably

#endif
