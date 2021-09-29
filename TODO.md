# Functionality
- Conditionals - DONE
    - `>`
    - `<`
    - `>=`
    - `<=`
    - `==`
    - `and`
    - `or`
    - `not`

- IF Statement

- IO
    - print
    - scans

- flow-control
    - begin
    - all 

- list
    - creation
    - car
    - cdr
    - max
    - min
    - length
    - concatenation


- Garbage Collection

# Assembly 
- replace repeated instructions with loops  (on add function, where `Utils.list_fill` is used)
- remove zeroing of register

# Code
- Remove `to_imm` function
- Improve concatenation of blocks of routines (basically remove `join_asm_lines`)
- Find a way to load subroutines from afar
- Compiler optimizations 
    - removing unreachable code
    - pre-evaluate expressions, like `(+ 4 5 6 (- 5 9))` will always be 11, so it should be calculated at compile time
    - tail-recursive optimization
    - only emit builtin routines if needed 
