# vul16-asm

Assembler for [Vulcan-16](https://github.com/zakki0925224/vul16-chisel)

## Calling convention register usage

| Register | Usage                             |
| -------- | --------------------------------- |
| `r0`     | Always zero (Hardware convention) |
| `r1`     | Return address                    |
| `r2`     | -                                 |
| `r3`     | -                                 |
| `r4`     | -                                 |
| `r5`     | -                                 |
| `r6`     | -                                 |
| `r7`     | -                                 |

## Virtual instructions

| Virtual instruction | Converted to real instruction | Description                                         |
| ------------------- | ----------------------------- | --------------------------------------------------- |
| `j #<label>`        | `jmp r1, <label offset>`      | Jump to label's top and save return address to `r1` |
| `ret`               | `jmpr, r0, r1, 0`             | Jump to return address `r1`                         |
| `nop`               | `add r0, r0, r0`              | No operation (does nothing)                         |

## Usage

```sh
cargo run -- -i <input assembly file> -o <output binary file>
```

## Assembly examples

See [vul16-chisel/src/test/resources/asm_tests](https://github.com/zakki0925224/vul16-chisel/tree/main/src/test/resources/asm_tests)
