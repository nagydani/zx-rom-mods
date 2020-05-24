#include <stdlib.h>
#include <stdio.h>
#include "d2zx.c"

int main(int argc, char *argv[]) {
	unsigned char zx[5];
	int retval;
	if(argc != 2) {
		printf("Usage: %s NUMBER\n", argv[0]);
		return 0;
	}
	if (retval = d2zx(zx, atof(argv[1]))) return retval;
	for(int i = 0; i < 5; i++) {
		printf("%02X", zx[i]);
	}
	printf("\n");
	return 0;
}
