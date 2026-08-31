main
    ldr     r0, =array
    mov     r1, #4
    mov     r2, #0xFFFFFFFF
    bl      count_and_substitute
    end

count_and_substitute
    str     r4, [sp, #-4]
    lsl     r1, r1, #2
    add     r1, r1, r0
    mov     r3, r0
    mov     r0, #0

loop
    cmp     r3, r1
    beq     loop_end
    ldr     r4, [r3], #4
    tsts    r4, #0x10
    strne   r2, [r3, #-4]
    addne   r0, r0, #1
    b       loop

loop_end
    ldr     r4, [sp], #4
    mov     pc, lr

array dcd   0x10, 0x80, 0x11, 0x30
