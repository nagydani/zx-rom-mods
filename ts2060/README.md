# TS2060 Replacement ROM For TS/TC2068 Computers

## Features

* ZX Spectrum 48k compatibility mode (using the ZX82 ROM)
  - pressing space during reset
  - `USR 0`
  - `SPECTRUM` instruction
* Support for additional video modes
  - `DISPLAY` instruction with a numerical argument 0 (ZX Spectrum), 1 (Multicolor) or 2 (HiRes)
* TS2068 instructions
  - `SOUND`
  - `ON ERR`
  - `RESET`
  - `DELETE`, albeit with a different token value (0x5F)
* TS2068 functions
  - `FREE`
  - `STICK` also supporting Kempston (joystick 0) and Cursor (joystick 3) joysticks as well as keyboard-based controls (second string argument in *right-left-down-up-fire* order).
* Spectrum 128k instructions
  - `SPECTRUM`
  - `PLAY`
* Additional instructions
  - `DPOKE` *address*, *16 bit word*
  - `FPOKE` *address*, *40 bit float*
  - `POKE` *address*, *string*
  - `USR` *address*
  - `DELETE` *variable* {, *variable* ...}
* Additional functions
  - `TIME`
  - `TIME$` *frames*
  - `MEM$`
  - `REF` *variable*
  - `DPEEK` *address*
  - `FPEEK` *address*
  - `STR$` (*value*, *base*) and `STR$` (*value*, *base*, *precision*)
* Additional operators
  - *dividend* `%` *divisor* for modulus
  - *switch* `?` (*expression 0*, *expression 1* ...)
* Labels beginning with `@` that can be used as jump destinations for `GO TO`, `GO SUB` and `RUN`.
* Expression literals between `{` and `}`: syntax-checked expressions stored as strings for evaluation by `VAL` or `VAL$`.
* String variables can have arbitrarily long names, just like numeric variables
* Numeric literals in hexadecimal (beginning with `$`) and octal (beginning with `\`) bases

## Keywords
|keyword|keys|
|---|---|
|`DELETE`|*ss* + `0`|
|`DISPLAY`|*E ss* + `K`|
|`DPEEK`|*E ss* + `O`|
|`DPOKE`|*E ss* + `L`|
|`FPEEK`|*E ss* + `H`|
|`FPOKE`|*ss* + `X`|
|`FREE`|*E ss* + `A`|
|`MEM$`|*E ss* + `U`|
|`ON ERR`|*E ss* + `F`|
|`PLAY`|*G* `U`|
|`REF`|*E ss* + `R`|
|`RESET`|*E ss* + `P`|
|`SOUND`|*E ss* + `G`|
|`SPECTRUM`|*G* `T`|
|`TIME`|*E ss* + `T`|
|`TIME$`|*E ss* + `Z`|
