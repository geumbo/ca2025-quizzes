.data
passed_msg:  .string "All tests passed.\n"
failed_msg1: .string ": produces value "
failed_msg2: .string " but encodes back to "
failed_msg3: .string ": value "
failed_msg4: .string " <= previous_value "
newline:     .string "\n"

.text
main:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw s0, 8(sp)
    sw s1, 4(sp)
    sw s2, 0(sp)
    jal ra, test             # start test
    beq a0, x0, test_failed  # if !passed, goto test_failed
    la a0, passed_msg        # print passed_msg
    li a7, 4
    ecall
    mv a0, x0                # if passed, return 0
    j main_end               # goto main_end
test_failed: 
    li a0, 1                 # if failed, return 1
main_end:
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    lw s2, 0(sp)
    addi sp, sp, 16
    li a7, 10                # exit syscall
    ecall                    # exit
test:
    addi sp, sp, -32
    sw ra, 28(sp)
    sw s0, 24(sp)            # s0 = i
    sw s1, 20(sp)            # s1 = previous_value
    sw s2, 16(sp)            # s2 = passed
    sw s3, 12(sp)            # s3 = value
    sw s4, 8(sp)             # s4 = fl2
    li s1, -1                # previous_value = -1
    li s2, 1                 # passed = true
    mv s0, x0                # i = 0
test_loop:
    li t0, 256
    bge s0, t0, test_end     # if i >= 256, goto test_end
    andi a0, s0, 0xFF        # a0 = i (uint8)
    jal ra, decode           # call decode
    mv s3, a0                # s3 = value
    jal ra, encode           # call encode
    mv s4, a0                # s4 = fl2
    andi t0, s0, 0xFF        # t0 = f1 (uint8) 
    andi t1, s4, 0xFF        # t1 = f2 (uint8)
    beq  t0, t1, test_value  # if f1 == f2, goto test_value
    mv   a0, t0              # print f1
    li   a7, 34              # print_hex syscall
    ecall                    
    la   a0, failed_msg1     # print failed_msg1
    li   a7, 4
    ecall
    mv   a0, s3              # print value
    li   a7, 1
    ecall
    la   a0, failed_msg2     # print failed_msg2
    li   a7, 4
    ecall
    mv   a0, s4              # print f2
    li   a7, 34
    ecall
    la   a0, newline         # print newline
    li   a7, 4
    ecall
    mv   s2, x0              # passed = false
test_value:
    bgt s3, s1, update_previous # if value <= previous_value, goto update_previous
    andi a0, s0, 0xFF           # print i
    li   a7, 34
    ecall
    la   a0, failed_msg3       # print failed_msg3
    li   a7, 4
    ecall
    mv  a0, s3                 # print value
    li  a7, 1
    ecall
    la   a0, failed_msg4       # print failed_msg4
    li   a7, 4
    ecall
    mv  a0, s1                 # print previous_value
    li  a7, 1
    ecall
    la   a0, newline           # print newline
    li   a7, 4
    ecall
    mv  s2, x0                 # passed = false
update_previous:
    mv  s1, s3                 # previous_value = value
    addi s0, s0, 1             # i = i + 1
    j    test_loop             # goto test_loop
test_end:
    mv   a0, s2
    lw   ra, 28(sp)
    lw   s0, 24(sp)
    lw   s1, 20(sp)
    lw   s2, 16(sp)
    lw   s3, 12(sp)
    lw   s4, 8(sp)
    addi sp, sp, 32
    ret
clz:
    li t0, 32          # t0 = n = 32
    li t1, 16          # t1 = c = 16
    mv t2, a0          # t2 = x
clz_1:
    srl t3, t2, t1     # t3 = y = x >> c
    beq t3, x0, clz_2  # if y == 0, goto clz_2
    sub t0, t0, t1     # n = n - c
    mv t2, t3          # x = y
clz_2: 
    srli t1, t1, 1     # c = c >> 1
    bne t1, x0, clz_1  # if c != 0, goto clz_1
    sub a0, t0, t2     # return n - x
    ret
decode:
    andi t0, a0, 0x0f   # t0 = mantissa = x & 0x0f
    srli t1, a0, 4      # t1 = exponent = x >> 4
    li t2, 15           # t2 = 15
    sub t3, t2, t1      # t3 = offset = 15 - exponent
    li t4, 0x7FFF       # t4 = 0x7FFF
    srl t3, t4, t3      # t3 = offset = 07FFF >> (15 - exponent)
    slli t3, t3, 4      # t3 = offset = 07FFF >> (15 - exponent) << 4
    sll  t0, t0, t1     # mantissa << exponent
    add a0, t0, t3      # return (mantissa << exponent) + offset
    ret
encode:
    addi sp, sp, -16              
    sw ra, 12(sp)
    sw s0, 8(sp)
    sw s1, 4(sp)
    sw s2, 0(sp)
    mv s0, a0                        # s0 = a0 = value
    li t0, 16                        # value = 16
    bltu s0, t0, encode_return_value # if value < 16, return value
    # Calculate exponent = 31 - clz((value >> 4) + 1)
    srli a0, s0, 4                   # value >> 4
    addi a0, a0, 1                   # (value >> 4) + 1
    jal ra, clz                      # call clz
    li t0, 31
    sub s1, t0, a0                   # exponent = 31 - clz
    # if (exponent > 15) exponent = 15
    li t0, 15
    bge t0, s1, calc_overflow
    mv s1, t0                        # s1 = exponent
calc_overflow:
    # overflow = ((1u << exponent) - 1u) << 4   
    li   t0, 1
    sll  t0, t0, s1                  # t0 = 1 << exponent
    addi t0, t0, -1                  # t0 = (1 << exponent) - 1
    slli s2, t0, 4                   # s2 = overflow = ((1 << exponent) - 1) << 4
calc_mantissa:
    # mantissa = (value - overflow) >> exponent
    sub t0, s0, s2                   # t0 = mantissa = value - overflow
    srl t0, t0, s1                   # mantissa >> exp
    slli s1, s1, 4                   # exp << 4
    or a0, s1, t0                    # return (mantissa | (exp << 4))
    j encode_end
encode_return_value:
    mv a0, s0                        # return value (a0)
encode_end:
    lw ra, 12(sp)             
    lw s0, 8(sp)             
    lw s1, 4(sp)             
    lw s2, 0(sp)              
    addi sp, sp, 16             
    ret