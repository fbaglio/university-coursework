; Write an ARM32 Assembly subroutine that modifies an array of unsigned 32-bit integers
; of unspecified length, passed as an argument, by dividing each value by 4.

main
    ldr     r0, =array          ; load the array's address into r0
    mov     r1, #4              ; load the number of array elements into r1
    bl      divide_by_four
    mov     r0, r2
    end

divide_by_four
    lsl     r1, r1, #2          ; multiply the number of elements by 4 to get the array size in bytes
    add     r1, r1, r0          ; calculate the address of the end of the array

loop
    cmp     r0, r1              ; compare the current address with the end address
    moveq   pc, lr              ; if they are equal, return to the caller
    ldr     r2, [r0]            ; load the current array element into r2
    lsr     r2, r2, #2          ; divide the current element by 4
    str     r2, [r0], #4        ; store the result and advance to the next element
    b       loop

array dcd   12,20,16,8