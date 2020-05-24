#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "d2zx.c"

char* note[] = {
	"C", "C#",
	"D", "D#",
	"E",
	"F", "F#",
	"G", "G#",
	"A", "A#",
	"B"
};

int main(int argc, char *argv[]) {
	unsigned char zx[5];
	if(argc != 1) return -1;
	for(int i = 0; i < 12; i++) {
		double f = 440.0 * pow(2, ((double)i - 9)/12);
		printf("\tDEFB\t");
		d2zx(zx, f);
		for(int j = 0; j < 5; j++) {
			if(j) printf(", ");
			printf("$%02X", zx[j]);
		}
		printf("\t; %3.10f Hz\t%s\n", f, note[i]);
	}
	return 0;
}
