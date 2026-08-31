; Write an ARM32 Assembly subroutine (count_and_substitute) that returns the number of
; negative values contained in an array of signed 32-bit integers passed as an argument
; and replaces them with a value passed as an additional argument.

start
    ldr     r0, =array          ; load the array's address into r0
    mov     r1, #4              ; load the number of array elements into r1
    mov     r2, #3              ; value used to replace negative elements
    bl      count_and_substitute
    end

count_and_substitute
    str     r4, [sp, #-4]!      ; save r4 on the stack
    lsl     r1, r1, #2          ; multiply the number of elements by 4 to get the array size in bytes
    add     r1, r1, r0          ; calculate the address of the end of the array
    mov     r3, #0              ; initialize the counter to zero

loop
    cmp     r0, r1              ; compare the current address with the end address
    beq     loop_end            ; if they are equal, exit the loop
    ldr     r4, [r0], #4        ; load the current array element into r4 and advance to the next element
    cmp     r4, #0              ; compare the current element with zero
    strmi   r2, [r0, #-4]       ; if the element is negative, replace it with r2
    addmi   r3, r3, #1          ; if the element is negative, increment the counter
    b       loop

loop_end
    ldr     r4, [sp], #4        ; restore r4 from the stack
    mov     pc, lr              ; return to the caller

array dcd   4,-3,-2,1