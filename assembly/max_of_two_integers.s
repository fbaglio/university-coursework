; Write an ARM32 Assembly subroutine that takes two signed 32-bit integers
; as arguments and returns the greater of the two.

main
    mov     r1, #4              ; load the first integer into r1
    mov     r2, #5              ; load the second integer into r2
    bl      max
    end

max
    cmp     r1, r2              ; compare the two integers
    movlt   r1, r2              ; if r1 < r2, move r2 into r1
    mov     r0, r1              ; return the greater value in r0
    mov     pc, lr              ; return to the caller