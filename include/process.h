/**
 *	process.h
 *
 *	definitions for processes
 **/

// inclusions

// type definitions

typedef	unsigned long long reg;

typedef struct  __attribute__((packed)) thread_state_str
{
	reg gps[16];	// there are 16 general purpose registers in x86_64
	reg xmm[2*16];	// there are 16 XMM registers, each 2 quads
	reg rip;	// current instruction pointer; x86_64 is reentrant, yes?
} thread_state_t;

typedef	struct thread_str
{
	unsigned int threadId;		// unique per thread in process
	void (*entryPoint)(void);	// function we call to get this started
	thread_state_t state;		// current state
} thread_t;

typedef struct process_str
{
	unsigned int processId;	// unique among currently running processes
	const char * name;	// the name of the program this process is running
	thread_t * threads;	// all of the threads associated with this process
				// thread id 0 is the main thread
} process_t;

