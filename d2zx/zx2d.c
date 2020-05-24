double sq(double x) { return x * x; }

double pow2(int n) {
	return n ?
		n < 0 ?
			1.0 / pow2(-n) :
			(n & 1 ? 2.0 : 1.0) * sq(pow2(n >> 1)) :
		1.0;
}

double zx2d(unsigned char *in) {
	int e = in[0];	unsigned long m[] = {
		in[1], in[2], in[3], in[4]
	};
	return e ?
		(pow2(e - 0xA0)) * (
			0x80000000
			+ ((m[0] & 0x7f) << 24)
			+ (m[1] << 16)
			+ (m[2] << 8)
			+ m[3]
			) * ((in[1] & 0x80) ? -1.0 : 1.0) :
		(in[1] ? -0x10000 : 0 ) + m[1] + (m[2] << 8);
}


# include <stdio.h>
void main() {
	unsigned char n[] = { 0x82, 0xe0, 0, 0, 0 };
	printf("%f\n", zx2d(n));
}
