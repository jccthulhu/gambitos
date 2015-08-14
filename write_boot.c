#include <stdio.h>
#include <stdlib.h>

#pragma pack(0)

typedef struct
{
	uint16_t sz;
	uint16_t val1;
	uint16_t val2;
	uint16_t val3;
} lba_desc_t;

typedef struct
{
	unsigned char id;
	unsigned char bootable;
	unsigned char chs[6];	// currently unused
	lba_desc_t lba;
} boot_entry_t;

int main( int argc, char * argv[] )
{
	// variables

	// function body
	char * filename = argv[1];
	FILE * f = fopen( filename, "wb" );
	{
		boot_entry_t b;
		b.id = 0x66;
		b.bootable = 1;
		lba_desc_t lba;
		lba.sz = 16;
		lba.val1 = 0;
		lba.val2 = 0;
		lba.val3 = 2;
		b.lba = lba;
		fwrite( &b, sizeof(boot_entry_t), 1, f );
	}
	fclose( f );

	// clean up
	return 0;
}
