#include <stdbool.h>

#define UNDERFLOW -2
#define OVERFLOW -1

int d2zx(unsigned char *out, double in) {
	long mantissa;
	bool sign = in < 0;
	if(sign) in = -in;
	out[0]=0x80;
	while(in < 0.5) {
		in *= 2.0;
		out[0]--;
		if(!out[0]) return UNDERFLOW;
	}
	while(in >= 1) {
		in *= 0.5;
		out[0]++;
		if(!out[0]) return OVERFLOW;
	}
	in *= 0x100000000l;
	in += 0.5;
	mantissa = in;
	out[1] = mantissa >> 24;
	mantissa &= 0xFFFFFFl;
	out[2] = mantissa >> 16;
	mantissa &= 0xFFFFl;
	out[3] = mantissa >> 8;
	mantissa &= 0xFFl;
	out[4] = mantissa;
	if(!sign) out[1] &= 0x7F;
	return 0;
}
