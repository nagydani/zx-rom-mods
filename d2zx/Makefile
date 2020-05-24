all: semitones zxfp

semitones: semitones.c d2zx.c
	cc -o semitones semitones.c -lm

zxfp: zxfp.c d2zx.c
	cc -o zxfp zxfp.c -lm

clean:
	rm -vf semitones zxfp
