.data
.equ BF16_SIGN_MASK, 0x8000
.equ BF16_EXP_MASK,  0x7F80
.equ BF16_MANT_MASK, 0x007F
.equ BF16_EXP_BIAS,  127
.equ BF16_NAN_BITS,  0x7FC0
.equ BF16_ZERO_BITS, 0x0000

test_header:       .string "\n=== bfloat16 Test Suite ===\n\n"
test_passed:       .string "\n=== ALL TESTS PASSED ===\n"
test_failed:       .string "\n=== TESTS FAILED ===\n"
basic_conv_msg:    .string "Testing basic conversions...\n"
basic_pass_msg:    .string "  Basic conversions: PASS\n"
fail_sign:         .string "FAIL: Sign mismatch\n"
fail_error:        .string "FAIL: Relative error too large\n"
special_val_msg:   .string "Testing special values...\n"
special_pass_msg:  .string "  Special values: PASS\n"
fail_pos_inf:      .string "FAIL: Positive infinity not detected\n"
fail_inf_as_nan:   .string "FAIL: Infinity detected as NaN\n"
fail_neg_inf:      .string "FAIL: Negative infinity not detected\n"
fail_nan:          .string "FAIL: NaN not detected\n"
fail_nan_as_inf:   .string "FAIL: NaN detected as infinity\n"
fail_zero:         .string "FAIL: Zero not detected\n"
fail_neg_zero:     .string "FAIL: Negative zero not detected\n"
comp_msg:          .string "Testing comparison operations...\n"
comp_pass_msg:     .string "  Comparisons: PASS\n"
fail_eq:           .string "FAIL: Equality test failed\n"
fail_neq:          .string "FAIL: Inequality test failed\n"
fail_lt:           .string "FAIL: Less than test failed\n"
fail_not_lt:       .string "FAIL: Not less than test failed\n"
fail_eq_not_lt:    .string "FAIL: Equal not less than test failed\n"
fail_gt:           .string "FAIL: Greater than test failed\n"
fail_not_gt:       .string "FAIL: Not greater than test failed\n"
fail_nan_eq:       .string "FAIL: NaN equality test failed\n"
fail_nan_lt:       .string "FAIL: NaN less than test failed\n"
fail_nan_gt:       .string "FAIL: NaN greater than test failed\n"
arith_msg:         .string "Testing arithmetic operations...\n"
arith_pass_msg:    .string "  Arithmetic: PASS\n"
fail_add:          .string "FAIL: Addition failed\n"
fail_sub:          .string "FAIL: Subtraction failed\n"
fail_mul:          .string "FAIL: Multiplication failed\n"
fail_div:          .string "FAIL: Division failed\n"
fail_sqrt_4:       .string "FAIL: sqrt(4) failed\n"
fail_sqrt_9:       .string "FAIL: sqrt(9) failed\n"
edge_msg:          .string "Testing edge cases...\n"
edge_pass_msg:     .string "  Edge cases: PASS\n"
fail_tiny:         .string "FAIL: Tiny value handling\n"
fail_overflow:     .string "FAIL: Overflow should produce infinity\n"
fail_underflow:    .string "FAIL: Underflow should produce zero or denormal\n"
rounding_msg:      .string "Testing rounding behavior...\n"
rounding_pass_msg: .string "  Rounding: PASS\n"
fail_exact:        .string "FAIL: Exact representation should be preserved\n"
fail_rounding:     .string "FAIL: Rounding error should be small\n"
skip_msg:          .string "  (skipped checks for zero or infinity)\n"

test_values:
    .word 0x00000000  # 0.0f
    .word 0x3F800000  # 1.0f
    .word 0xBF800000  # -1.0f
    .word 0x40000000  # 2.0f
    .word 0xC0000000  # -2.0f
    .word 0x3F000000  # 0.5f
    .word 0xBF000000  # -0.5f
    .word 0x40490FDB  # 3.14159f
    .word 0xC0490FDB  # -3.14159f
    .word 0x501502F9  # 1e10f
    .word 0xD01502F9  # -1e10f

.text
main:
    addi sp, sp, -8
    sw ra, 4(sp)
    sw s0, 0(sp)
    la a0, test_header
    jal ra, print_string
    li s0, 0                        # failed = 0
    jal ra, test_basic_conversions
    or s0, s0, a0                   # failed |= test_basic_conversions()
    jal ra, test_special_values
    or s0, s0, a0                   # failed |= test_special_values()
    jal ra, test_arithmetic
    or s0, s0, a0                   # failed |= test_arithmetic()
    jal ra, test_comparisons
    or s0, s0, a0                   # failed |= test_comparisons()
    jal ra, test_edge_cases
    or s0, s0, a0                   # failed |= test_edge_cases()
    jal ra, test_rounding
    or s0, s0, a0                   # failed |= test_rounding()
    beq s0, x0, all_passed          # if (!failed) goto all_passed
    la a0, test_failed
    jal ra, fail_with_message
    j main_end
all_passed:
    la a0, test_passed
    jal ra, print_string
    li a0, 0                        # return 0
main_end:
    lw ra, 4(sp)
    lw s0, 0(sp)
    addi sp, sp, 8
    li a7, 10                       # exit syscall
    ecall                           # exit
test_basic_conversions:
    addi sp, sp, -44
    sw ra, 40(sp)
    sw s0, 36(sp)                   # s0 = i
    sw s1, 32(sp)                   # s1 = &test_values[0]
    sw s2, 28(sp)                   # s2 = orig
    sw s3, 24(sp)                   # s3 = bf
    sw s4, 20(sp)                   # s4 = conv
    sw s5, 16(sp)                   # s5 = diff
    sw s6, 12(sp)                   # s6 = rel_error
    sw s7, 8(sp)                    # s7 = temp
    sw s8, 4(sp)                    # s8 = test_count
    la a0, basic_conv_msg
    jal ra, print_string
    li s0, 0                        # i = 0
    la s1, test_values              # &test_values[0]
    li s8, 11                       # s8 = test_count = 11
    li t6, 0x7FFF                   # mask for bf16 absolute value
test_basic_conversions_loop:
    # for (i = 0; i < test_count; i++)
    beq s0, s8, test_basic_conversions_passed
    slli t1, s0, 2                 # t1 = i * 4
    add t2, s1, t1                 # t2 = &test_values[i]
    lw s2, 0(t2)                   # s2 = orig = test_values[i]
    mv a0, s2                      # a0 = orig
    jal ra, f32_to_bf16            # bf = f32_to_bf16(orig)
    mv s3, a0                      # s3 = bf
    mv a0, s3                      # a0 = bf
    jal ra, bf16_to_f32            # conv = bf16_to_f32(bf)
    mv s4, a0                      # s4 = conv
    li t3, 0x7FFFFFFF
    and t3, s2, t3
    beq t3, x0, skip_check         # if (orig == 0.0f) skip check
    # Test 1: Sign check
    srli t1, s2, 31                # t1 = orig_sign
    srli t2, s4, 31                # t2 = conv_sign
    beq t1, t2, sign_ok            # if (orig_sign == conv_sign) goto sign_ok
    la a0, fail_sign
    jal ra, fail_with_message
    j test_basic_conversions_end
sign_ok:
    # Test 2: Relative error check
    # if (orig != 0.0f && !bf16_isinf(f32_to_bf16(orig)))
    mv a0, s3                      # a0 = bf
    jal ra, isinf
    bne a0, x0, skip_check         # if (bf16_isinf(bf)) skip check
    # diff = (conv - orig)
    mv a0, s4                      # a0 = conv
    jal ra, f32_to_bf16
    mv s7, a0                      # s7 = conv in bf16
    mv a0, s2                      # a0 = orig
    jal ra, f32_to_bf16
    mv a1, a0                      # a1 = orig in bf16
    mv a0, s7                      # a0 = conv in bf16
    jal ra, bf16_sub               # diff = bf16_sub(conv, orig)
    mv s5, a0                      # s5 = diff
    # abs(diff) - mask out sign bit
    and s5, s5, t6                 # diff = abs(diff)
    # rel_error = # rel_error = |diff| / orig
    mv a0, s2                      # a0 = orig
    jal ra, f32_to_bf16
    mv a1, a0                      # a1 = orig in bf16
    mv a0, s5                      # a0 = diff
    jal ra, bf16_div               # rel_error = bf16_div(diff, orig)
    mv s6, a0                      # s6 = rel_error
    # abs(rel_error) - mask out sign bit
    and s6, s6, t6                 # rel_error = abs(rel_error)
    li a0, 0x3C23D70A              # 0.01f
    jal ra, f32_to_bf16
    mv a1, a0                      # a1 = 0.01 in bf16
    mv a0, s6                      # a0 = rel_error
    jal ra, lt                     # if (bf16_lt(rel_error, 0.01)) return true
    bne a0, x0, skip_check
    la a0, fail_error
    jal ra, fail_with_message
    j test_basic_conversions_end
skip_check:
    addi s0, s0, 1                 # i++
    j test_basic_conversions_loop
test_basic_conversions_passed:
    la a0, basic_pass_msg
    jal ra, print_string
    li a0, 0                       # return 0
test_basic_conversions_end:
    lw ra, 40(sp)
    lw s0, 36(sp)
    lw s1, 32(sp)
    lw s2, 28(sp)
    lw s3, 24(sp)
    lw s4, 20(sp)
    lw s5, 16(sp)
    lw s6, 12(sp)
    lw s7, 8(sp)
    lw s8, 4(sp)
    addi sp, sp, 44
    ret
test_special_values:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw s0, 8(sp)
    sw s1, 4(sp)
    la a0, special_val_msg
    jal ra, print_string
    li s0, 0x7F80                # s0 = pos_inf_bits
    mv a0, s0                    # a0 = pos_inf_bits
    jal ra, isinf            
    bne a0, x0, pos_inf_ok       # if (bf16_isinf(pos_inf_bits)) return true
    la a0, fail_pos_inf
    jal ra, fail_with_message
    j test_special_values_end
pos_inf_ok:
    mv a0, s0                    # a0 = pos_inf_bits
    jal ra, isnan               
    beq a0, x0, pos_not_nan_ok   # if (!bf16_isnan(pos_inf_bits)) return true
    la a0, fail_inf_as_nan
    jal ra, fail_with_message
    j test_special_values_end
pos_not_nan_ok:
    li s0, 0xFF80                # s0 = neg_inf_bits
    mv a0, s0                    # a0 = neg_inf_bits
    jal ra, isinf                # if (bf16_isinf(neg_inf_bits)) return true
    bne a0, x0, neg_inf_ok
    la a0, fail_neg_inf
    jal ra, fail_with_message
    j test_special_values_end
neg_inf_ok:
    jal ra, nan                  # a0 = NaN()
    mv s0, a0                    # s0 = NaN()
    mv a0, s0                    # a0 = NaN()
    jal ra, isnan                
    bne a0, x0, nan_ok           # if (bf16_isnan(NaN())) return true
    la a0, fail_nan
    jal ra, fail_with_message
    j test_special_values_end
nan_ok:
    mv a0, s0                    # a0 = NaN()
    jal ra, isinf               
    beq a0, x0, nan_not_inf_ok   # if (!bf16_isinf(NaN())) return true
    la a0, fail_nan_as_inf
    jal ra, fail_with_message
    j test_special_values_end
nan_not_inf_ok:
    li a0, 0x00000000            # a0 = zero_bits
    jal ra, f32_to_bf16          # a0 = f32_to_bf16(0.0f)
    mv s0, a0                    # s0 = zero_bits
    mv a0, s0                    # a0 = zero_bits
    jal ra, iszero               
    bne a0, x0, zero_ok          # if (bf16_iszero(zero_bits)) return true
    la a0, fail_zero
    jal ra, fail_with_message
    j test_special_values_end
zero_ok:
    li a0, 0x80000000            # a0 = neg_zero_bits
    jal ra, f32_to_bf16          # a0 = f32_to_bf16
    mv s0, a0                    # s0 = neg_zero_bits
    mv a0, s0                    # a0 = neg_zero_bits
    jal ra, iszero
    bne a0, x0, neg_zero_ok      # if (bf16_iszero(neg_zero_bits)) return true
    la a0, fail_neg_zero
    jal ra, fail_with_message
    j test_special_values_end
neg_zero_ok:
    la a0, special_pass_msg
    jal ra, print_string
    li a0, 0                     # return 0
test_special_values_end:
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    addi sp, sp, 16
    ret
test_arithmetic:
    addi sp, sp, -24
    sw ra, 20(sp)
    sw s0, 16(sp)                # s0 = a
    sw s1, 12(sp)                # s1 = b
    sw s2, 8(sp)                 # s2 = c
    sw s3, 4(sp)                 # s3 = tol_small
    sw s4, 0(sp)                 # s4 = tol_large
    la a0, arith_msg
    jal ra, print_string
    li s3, 0x3C23D70A            # tolerance: 0.01f
    li s4, 0x3DCCCCCD            # tolerance: 0.1f
    # Test 1: Addition (1.0 + 2.0 = 3.0)
    li a0, 0x3F800000            # bf16_t a = f32_to_bf16(1.0f)
    jal ra, f32_to_bf16        
    mv s0, a0                    # s0 = a
    li a0, 0x40000000            # bf16_t b = f32_to_bf16(2.0f)
    jal ra, f32_to_bf16
    mv s1, a0                    # s1 = b
    mv a0, s0                    # a0 = a
    mv a1, s1                    # a1 = b
    jal ra, bf16_add             # c = bf16_add(a, b)
    mv s2, a0                    # s2 = c
    mv a0, s2                    # actual result in bf16
    li a1, 0x40400000            # expected = 3.0f
    mv a2, s3                    # tolerance = 0.01f
    jal ra, bf16_expect_close
    bne a0, x0, add_ok
    la a0, fail_add
    jal ra, fail_with_message
    j test_arithmetic_end
add_ok:
    # Test 2: Subtraction (2.0 - 1.0 = 1.0)
    mv a0, s1                    # a0 = b
    mv a1, s0                    # a1 = a
    jal ra, bf16_sub             # c = bf16_sub(b, a)
    mv s2, a0                    # s2 = c
    mv a0, s2
    li a1, 0x3F800000            # expected = 1.0f
    mv a2, s3                    # tolerance = 0.01f
    jal ra, bf16_expect_close
    bne a0, x0, sub_ok
    la a0, fail_sub
    jal ra, fail_with_message
    j test_arithmetic_end
sub_ok:
    # Test 3: Multiplication (3.0 * 4.0 = 12.0)
    li a0, 0x40400000            # a = f32_to_bf16(3.0f)
    jal ra, f32_to_bf16
    mv s0, a0                    # s0 = a
    li a0, 0x40800000            # b = f32_to_bf16(4.0f)
    jal ra, f32_to_bf16
    mv s1, a0                    # s1 = b
    mv a0, s0                    # a0 = a
    mv a1, s1                    # a1 = b
    jal ra, bf16_mul             # c = bf16_mul(a, b)
    mv s2, a0                    # s2 = c
    mv a0, s2
    li a1, 0x41400000            # expected = 12.0f
    mv a2, s4                    # tolerance = 0.1f
    jal ra, bf16_expect_close
    bne a0, x0, mul_ok
    la a0, fail_mul
    jal ra, fail_with_message
    j test_arithmetic_end
mul_ok:
    # Test 4: Division (10.0 / 2.0 = 5.0)
    li a0, 0x41200000            # a = f32_to_bf16(10.0f)
    jal ra, f32_to_bf16
    mv s0, a0                    # s0 = a
    li a0, 0x40000000            # b = f32_to_bf16(2.0f)
    jal ra, f32_to_bf16
    mv s1, a0                    # s1 = b
    mv a0, s0                    # a0 = a
    mv a1, s1                    # a1 = b
    jal ra, bf16_div             # c = bf16_div(a, b)
    mv s2, a0                    # s2 = c
    mv a0, s2
    li a1, 0x40A00000            # expected = 5.0f
    mv a2, s4                    # tolerance = 0.1f
    jal ra, bf16_expect_close
    bne a0, x0, div_ok
    la a0, fail_div
    jal ra, fail_with_message
    j test_arithmetic_end
div_ok:
    # Test 5: Square Root (sqrt(4.0) = 2.0)
    li a0, 0x40800000            # a = f32_to_bf16(4.0f)
    jal ra, f32_to_bf16
    mv s0, a0                    # s0 = a
    mv a0, s0                    # a0 = a
    jal ra, bf16_sqrt            # c = bf16_sqrt(a)
    mv s2, a0                    # s2 = c
    mv a0, s2
    li a1, 0x40000000            # expected = 2.0f
    mv a2, s3                    # tolerance = 0.01f
    jal ra, bf16_expect_close
    bne a0, x0, sqrt4_ok
    la a0, fail_sqrt_4
    jal ra, fail_with_message
    j test_arithmetic_end
sqrt4_ok:
    # Test 6: Square Root (sqrt(9.0) = 3.0)
    li a0, 0x41100000            # a = f32_to_bf16(9.0f)
    jal ra, f32_to_bf16
    mv s0, a0                    # s0 = a
    mv a0, s0                    # a0 = a
    jal ra, bf16_sqrt            # c = bf16_sqrt(a)
    mv s2, a0                    # s2 = c
    mv a0, s2
    li a1, 0x40400000            # expected = 3.0f
    mv a2, s3                    # tolerance = 0.01f
    jal ra, bf16_expect_close
    bne a0, x0, sqrt9_ok
    la a0, fail_sqrt_9
    jal ra, fail_with_message
    j test_arithmetic_end
sqrt9_ok:
    la a0, arith_pass_msg
    jal ra, print_string
    li a0, 0                     # return 0
test_arithmetic_end:
    lw ra, 20(sp)
    lw s0, 16(sp)
    lw s1, 12(sp)
    lw s2, 8(sp)
    lw s3, 4(sp)
    lw s4, 0(sp)
    addi sp, sp, 24
    ret
test_comparisons:
    addi sp, sp, -20
    sw ra, 16(sp)      
    sw s0, 12(sp)                # s0 = a
    sw s1, 8(sp)                 # s1 = b
    sw s2, 4(sp)                 # s2 = c
    sw s3, 0(sp)                 # s3 = nan_val
    la a0, comp_msg
    jal ra, print_string
    li a0, 0x3F800000            # bf16_t a = f32_to_bf16(1.0f)
    jal ra, f32_to_bf16
    mv s0, a0                    # s0 = a
    mv s2, a0                    # s2 = c
    li a0, 0x40000000            # bf16_t b = f32_to_bf16(2.0f)
    jal ra, f32_to_bf16
    mv s1, a0                    # s1 = b
    mv a0, s0                    # a0 = a
    mv a1, s2                    # a1 = c
    jal ra, eq                   
    bne a0, x0, eq_ok            # if (bf16_eq(a, c)) return true
    la a0, fail_eq
    jal ra, fail_with_message
    j test_comparisons_end
eq_ok:
    mv a0, s0                    # a0 = a
    mv a1, s1                    # a1 = b
    jal ra, eq
    beq a0, x0, neq_ok           # if (!bf16_eq(a, b)) return true
    la a0, fail_neq
    jal ra, fail_with_message
    j test_comparisons_end
neq_ok:
    mv a0, s0                    # a0 = a
    mv a1, s1                    # a1 = b
    jal ra, lt
    bne a0, x0, lt_ok            # if (bf16_lt(a, b)) return true
    la a0, fail_lt
    jal ra, fail_with_message
    j test_comparisons_end
lt_ok:
    mv a0, s1                    # a0 = b
    mv a1, s0                    # a1 = a
    jal ra, lt
    beq a0, x0, not_lt_ok        # if (!bf16_lt(b, a)) return true
    la a0, fail_not_lt
    jal ra, fail_with_message
    j test_comparisons_end
not_lt_ok:
    mv a0, s0                    # a0 = a
    mv a1, s2                    # a1 = c
    jal ra, lt
    beq a0, x0, eq_not_lt_ok     # if (!bf16_lt(a, c)) return true
    la a0, fail_eq_not_lt
    jal ra, fail_with_message
    j test_comparisons_end
eq_not_lt_ok:
    mv a0, s1                    # a0 = b
    mv a1, s0                    # a1 = a
    jal ra, gt
    bne a0, x0, gt_ok            # if (bf16_gt(b, a)) return true
    la a0, fail_gt
    jal ra, fail_with_message
    j test_comparisons_end
gt_ok:
    mv a0, s0                    # a0 = a
    mv a1, s1                    # a1 = b
    jal ra, gt
    beq a0, x0, not_gt_ok        # if (!bf16_gt(a, b)) return true
    la a0, fail_not_gt
    jal ra, fail_with_message
    j test_comparisons_end
not_gt_ok:
    jal ra, nan                  # a0 = NaN()
    mv s3, a0                    # s3 = nan_val
    mv a0, s0                    # a0 = a
    mv a1, s3                    # a1 = nan_val
    jal ra, eq
    beq a0, x0, nan_eq_ok        # if (!bf16_eq(a, nan_val)) return true
    la a0, fail_nan_eq
    jal ra, fail_with_message
    j test_comparisons_end
nan_eq_ok:
    mv a0, s0                    # a0 = a
    mv a1, s3                    # a1 = nan_val
    jal ra, lt
    beq a0, x0, nan_lt_ok        # if (!bf16_lt(a, nan_val)) return true
    la a0, fail_nan_lt
    jal ra, fail_with_message
    j test_comparisons_end
nan_lt_ok:
    mv a0, s0                    # a0 = a
    mv a1, s3                    # a1 = nan_val
    jal ra, gt
    beq a0, x0, nan_gt_ok        # if (!bf16_gt(a, nan_val)) return true
    la a0, fail_nan_gt
    jal ra, fail_with_message
    j test_comparisons_end
nan_gt_ok:
    la a0, comp_pass_msg
    jal ra, print_string
    li a0, 0                     # return 0
test_comparisons_end:
    lw ra, 16(sp)
    lw s0, 12(sp)
    lw s1, 8(sp)
    lw s2, 4(sp)
    lw s3, 0(sp)
    addi sp, sp, 20
    ret
test_edge_cases:
    addi sp, sp, -32
    sw ra, 28(sp)
    sw s0, 24(sp)                # s0 = bf_tiny
    sw s1, 20(sp)                # s1 = tiny_val
    sw s2, 16(sp)                # s2 = bf_huge
    sw s3, 12(sp)                # s3 = bf_huge2
    sw s4, 8(sp)                 # s4 = small
    sw s5, 4(sp)                 # s5 = smaller
    sw s6, 0(sp)                 # s6 = smaller_val
    la a0, edge_msg
    jal ra, print_string
    # Test 1: Tiny value handling (1e-45f)
    li a0, 0x00000001            # tiny_val = 1e-45f
    jal ra, f32_to_bf16
    mv s0, a0                    # s0 = bf_tiny
    mv a0, s0                    # a0 = bf_tiny
    jal ra, iszero
    bne a0, x0, test_overflow    # if (bf16_iszero(bf_tiny)) return true
    # else check |tiny_val| < 1e-37f
    mv a0, s0                    # a0 = bf_tiny
    jal ra, bf16_to_f32
    mv s1, a0                    # s1 = tiny_val
    li t0, 0x7FFFFFFF            # t0 = 0x7FFFFFFF
    and t1, s1, t0               # t1 = |tiny_val|
    li t2, 0x0C388000            # 1e-37f
    bltu t1, t2, test_overflow   # if (|tiny_val| < 1e-37f) return true
    la a0, fail_tiny
    jal ra, fail_with_message
    j test_edge_cases_end
test_overflow:
    # Test 2: Huge value handling (1e38f * 10.0f)
    li a0, 0x7e967699            # huge_val
    jal ra, f32_to_bf16
    mv s2, a0                    # s2 = bf_huge
    li a0, 0x41200000            # 10.0f
    jal ra, f32_to_bf16
    mv a1, a0                    # a1 = 10.0 in bf16
    mv a0, s2                    # a0 = bf_huge
    jal ra, bf16_mul
    mv s3, a0                    # s3 = bf_huge2
    mv a0, s3                    # a0 = bf_huge2
    jal ra, isinf
    bne a0, x0, test_underflow   # if (bf16_isinf(bf_huge2)) return true
    la a0, fail_overflow
    jal ra, fail_with_message
    j test_edge_cases_end
test_underflow:
    # Test 3: Underflow produces zero or denormal (1e-38f / 10.0f)
    li a0, 0x0C2EFA00             # smaller_val = 1e-38f
    jal ra, f32_to_bf16
    mv s4, a0                     # s4 = small
    li a0, 0x501502F9             # 1e10f
    jal ra, f32_to_bf16
    mv a1, a0                     # a1 = 1e10 in bf16
    mv a0, s4                     # a0 = small
    jal ra, bf16_div
    mv s5, a0                     # s5 = smaller
    mv a0, s5                     # a0 = smaller
    jal ra, iszero
    bne a0, x0, edge_cases_ok     # if (bf16_iszero(smaller)) return true
    # else check |smaller_val| < 1e-45f
    mv a0, s5                     # a0 = smaller
    jal ra, bf16_to_f32
    mv s6, a0                     # s6 = smaller_val
    li t0, 0x7FFFFFFF             # t0 = 0x7FFFFFFF
    and t1, s6, t0                # t1 = |smaller_val
    li t2, 0x00000001             # 1e-45f
    bltu t1, t2, edge_cases_ok    # if (|smaller_val| < 1e-45f) return true
    la a0, fail_underflow
    jal ra, fail_with_message
    j test_edge_cases_end
edge_cases_ok:
    la a0, edge_pass_msg
    jal ra, print_string
    li a0, 0
test_edge_cases_end:
    lw ra, 28(sp)
    lw s0, 24(sp)
    lw s1, 20(sp)
    lw s2, 16(sp)
    lw s3, 12(sp)
    lw s4, 8(sp)
    lw s5, 4(sp)
    lw s6, 0(sp)
    addi sp, sp, 32
    ret
test_rounding:
    addi sp, sp, -24
    sw ra, 20(sp)
    sw s0, 16(sp)                # s0 = bf_exact
    sw s1, 12(sp)                # s1 = back_exact
    sw s2, 8(sp)                 # s2 = bf
    sw s3, 4(sp)                 # s3 = back
    sw s4, 0(sp)                 # s4 = diff2
    la a0, rounding_msg
    jal ra, print_string
    li t6, 0x7FFF               # mask for bf16 absolute value
    # Test 1: Exact representation (1.5f)
    li a0, 0x3FC00000            # bf_exact = f32_to_bf16(1.5f)
    jal ra, f32_to_bf16
    mv s0, a0                    # s0 = bf_exact
    mv a0, s0                    # a0 = bf_exact
    jal ra, bf16_to_f32          # back_exact = bf16_to_f32
    mv s1, a0                    # s1 = back_exact
    li t0, 0x3FC00000            # expected = 1.5f
    bne s1, t0, fail_round_exact # if (back_exact != expected) fail
    # Test 2: Rounding (1.0001f)
    li a0, 0x3F800347            # bf = f32_to_bf16(1.0001f)
    jal ra, f32_to_bf16
    mv s2, a0                    # s2 = bf
    mv a0, s2                    # a0 = bf
    jal ra, bf16_to_f32          # back = bf16_to_f32
    mv s3, a0                    # s3 = back
    # diff2 = back - val
    mv a0, s3
    jal ra, f32_to_bf16
    mv s3, a0                    # s3 = back in bf16
    li a0, 0x3F800347            # expected = 1.0001f
    jal ra, f32_to_bf16
    mv a1, a0                    # a1 = expected in bf16
    mv a0, s3                    # a0 = back in bf16
    jal ra, bf16_sub
    mv s4, a0                    # s4 = diff2
    # abs(diff2) - mask out sign bit
    and s4, s4, t6               # diff2 = abs(diff2)
    li a0, 0x3A83126F            # 0.001f
    jal ra, f32_to_bf16
    mv a1, a0                    # a1 = 0.001 in bf16
    mv a0, s4                    # a0 = diff2
    jal ra, lt
    bne a0, x0, rounding_ok       # if (bf16_lt(diff2, 0.001)) return true
    la a0, fail_rounding
    jal ra, fail_with_message
    j test_rounding_end
fail_round_exact:
    la a0, fail_exact
    jal ra, fail_with_message
    j test_rounding_end
rounding_ok:
    la a0, rounding_pass_msg
    jal ra, print_string
    li a0, 0                     # return 0
test_rounding_end:
    lw ra, 20(sp)
    lw s0, 16(sp)
    lw s1, 12(sp)
    lw s2, 8(sp)
    lw s3, 4(sp)
    lw s4, 0(sp)
    addi sp, sp, 24
    ret
print_string:
    addi sp, sp, -4
    sw ra, 0(sp)
    li a7, 4
    ecall
    lw ra, 0(sp)
    addi sp, sp, 4
    ret
fail_with_message:
    mv t0, ra
    jal ra, print_string
    mv ra, t0
    li a0, 1
    ret
isnan:
    # a0 = bf16_t.bits
    li t0, BF16_EXP_MASK          # t0 = EXP_MASK
    and t1, a0, t0                # t1 = a.bits & EXP_MASK
    bne t1, t0, return_false      # if t1 != EXP_MASK, return false
    andi t1, a0, BF16_MANT_MASK   # t1 = a.bits & MANT_MASK
    beq t1, x0, return_false      # if t1 == 0, return false
    li a0, 1                      # return true
    ret
isinf:
    # a0 = bf16_t.bits
    li t0, BF16_EXP_MASK          # t0 = EXP_MASK
    and t1, a0, t0                # t1 = a.bits & EXP_MASK
    bne t1, t0, return_false      # if t1 != EXP_MASK, return false
    andi t1, a0, BF16_MANT_MASK   # t1 = a.bits & MANT_MASK
    bne t1, x0, return_false      # if t1 != 0, return false
    li a0, 1                      # return true
    ret
iszero:
    # a0 = bf16_t.bits
    li t0, 0x7FFF                 # t0 = 0x7FFF
    and t1, a0, t0                # t1 = a.bits & 0x7FFF
    bne t1, x0, return_false      # if t1 != 0, return false
    li a0, 1                      # return true
    ret
nan:
    # a0 = bf16_t.bits
    li a0, BF16_NAN_BITS          # return a0 = NAN_BITS
    ret
zero:
    # a0 = bf16_t.bits
    li a0, BF16_ZERO_BITS         # return a0 = ZERO_BITS
    ret
return_false:
    li a0, 0                      # return false
    ret
bf16_expect_close:
    # a0 = actual result (bf16 bits)
    # a1 = expected result (float32 bits)
    # a2 = tolerance (float32 bits)
    addi sp, sp, -16
    sw ra, 12(sp)
    sw s0, 8(sp)
    sw s1, 4(sp)
    sw s2, 0(sp)
    mv s0, a0                    # preserve actual bf16
    mv s1, a1                    # preserve expected f32 bits
    mv s2, a2                    # preserve tolerance f32 bits
    mv a0, s1
    jal ra, f32_to_bf16          # expected bf16
    mv s1, a0
    mv a0, s0
    mv a1, s1
    jal ra, bf16_sub             # diff = actual - expected
    mv s0, a0
    li t0, 0x7FFF
    and s0, s0, t0               # |diff|
    mv a0, s2
    jal ra, f32_to_bf16          # tolerance in bf16
    mv a1, a0
    mv a0, s0
    jal ra, lt                   # return diff < tolerance
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    lw s2, 0(sp)
    addi sp, sp, 16
    ret
eq:
    # a0 = bf16_t a.bits
    # a1 = bf16_t b.bits
    addi sp, sp, -16
    sw ra, 12(sp)
    sw s0, 8(sp)
    sw s1, 4(sp)
    mv s0, a0                   # s0 = a0 = a.bits
    mv s1, a1                   # s1 = a1 = b.bits
    jal ra, isnan               # call bf16_isnan(a0)
    bne a0, x0, compare_false   # if a is NaN, return false
    mv a0, s1                   # a0 = b.bits
    jal ra, isnan               # call bf16_isnan(a0)
    bne a0, x0, compare_false   # if b is NaN, return false
    mv a0, s0                   # a0 = a.bits
    jal ra, iszero              # call bf16_iszero(a0)
    beq a0, x0, check_bits      # if a is not zero, check bits
    mv a0, s1                   # a0 = b.bits
    jal ra, iszero              # call bf16_iszero(a0)
    bne a0, x0, compare_true    # if both are zero, return true
    j compare_false             # else return false
check_bits:
    bne  s0, s1, compare_false  # not equal if bits differ
    j compare_true              # else return true
lt:
    # a0 = bf16_t a.bits
    # a1 = bf16_t b.bits
    addi sp, sp, -16
    sw ra, 12(sp)
    sw s0, 8(sp)
    sw s1, 4(sp)
    mv s0, a0                   # s0 = a0 = a.bits
    mv s1, a1                   # s1 = a1 = b.bits
    jal ra, isnan               # call bf16_isnan(a0)
    bne a0, x0, compare_false   # if a is NaN, return false
    mv a0, s1                   # a0 = b.bits
    jal ra, isnan               # call bf16_isnan(a0)
    bne a0, x0, compare_false   # if b is NaN, return false
    mv a0, s0                   # a0 = a.bits
    jal ra, iszero              # call bf16_iszero(a0)
    beq a0, x0, lt_compare      # if a is not zero, continue to compare
    mv a0, s1                   # a0 = b.bits
    jal ra, iszero              # call bf16_iszero(a0)
    bne a0, x0, compare_false   # if both are zero, return false
    j lt_compare                # continue to compare
lt_compare:
    srli t0, s0, 15             # t0 = sign_a = a.bits >> 15
    andi t0, t0, 1              # t0 = sign_a = (a.bits >> 15) & 1
    srli t1, s1, 15             # t1 = sign_b = b.bits >> 15
    andi t1, t1, 1              # t1 = sign_b = (b.bits >> 15) & 1
    bne t0, t1, lt_diff_sign    # if sign_a != sign_b, check sign
    beq t0, x0, lt_positive     # if sign_a == 0, both are positive
lt_negative:
    bltu s1, s0, compare_true   # return a.bits > b.bits
    j compare_false
lt_positive:
    bltu s0, s1, compare_true   # return a.bits < b.bits
    j compare_false
lt_diff_sign:
    bltu t1, t0, compare_true   # return sign_a > sign_b
    j compare_false
compare_true:
    li a0, 1
    j compare_end
compare_false:
    li a0, 0
compare_end:
    lw ra, 12(sp)
    lw s0, 8(sp)
    lw s1, 4(sp)
    addi sp, sp, 16
    ret
gt:
    # a0 = bf16_t a.bits
    # a1 = bf16_t b.bits
    mv t0, a0 # t0 = a
    mv a0, a1 # a0 = b
    mv a1, t0 # a1 = a
    j lt      # return lt(b, a)
f32_to_bf16:
    srli t0, a0, 23          # t0 = f32bits >> 23
    andi t0, t0, 0xFF        # t1 = exponent
    li t1, 0xFF
    beq t0, t1, f32_special  # if exp == 0xFF, handle inf/nan
    srli t0, a0, 16          # t0 = f32bits >> 16
    andi t0, t0, 1           # t0 = (f32bits >> 16) & 1
    li t1, 0x7FFF            # t1 = 0x7FFF
    add t0, t0, t1           # t0 = ((f32bits >> 16) & 1) + 0x7FFF
    add a0, a0, t0           # a0 = f32bits + t0
    srli a0, a0, 16          # a0 = f32bits >> 16
    ret
f32_special:
    srli a0, a0, 16          # a0 = f32bits >> 16
    ret
bf16_to_f32:
    slli a0, a0, 16          # a0 = a0 << 16
    ret
bf16_add:
    # a0 = bf16_t a.bits
    # a1 = bf16_t b.bits
    addi sp, sp, -48
    sw ra, 44(sp)
    sw s0, 40(sp)               # s0 = sign_a
    sw s1, 36(sp)               # s1 = sign_b
    sw s2, 32(sp)               # s2 = exp_a
    sw s3, 28(sp)               # s3 = exp_b
    sw s4, 24(sp)               # s4 = mant_a
    sw s5, 20(sp)               # s5 = mant_b
    sw s6, 16(sp)               # s6 = result_sign
    sw s7, 12(sp)               # s7 = result_exp
    sw s8, 8(sp)                # s8 = result_mant
    sw s9, 4(sp)                # s9 = a.bits
    sw s10, 0(sp)               # s10 = b.bits
    mv s9, a0                   # s9 = a0 = a.bits
    mv s10, a1                  # s10 = b0 = b.bits
    # extract sign_a, sign_b
    srli s0, s9, 15             # s0 = sign_a = a.bits >> 15
    andi s0, s0, 1              # s0 = sign_a = (a.bits >> 15) & 1
    srli s1, s10, 15            # s1 = sign_b = b.bits >> 15
    andi s1, s1, 1              # s1 = sign_b = (b.bits >> 15) & 1
    # extract exp_a, exp_b
    srli s2, s9, 7              # s2 = exp_a = a.bits >> 7
    andi s2, s2, 0xFF           # s2 = exp_a = (a.bits >> 7) & 0xFF
    srli s3, s10, 7             # s3 = exp_b = b.bits >> 7
    andi s3, s3, 0xFF           # s3 = exp_b = (b.bits >> 7) & 0xFF
    # extract mant_a, mant_b
    andi s4, s9, 0x7F           # s4 = mant_a = a.bits & 0x7F
    andi s5, s10, 0x7F          # s5 = mant_b = b.bits & 0x7F
    # if (exp_a == 0xFF)
    li t0, 0xFF
    bne s2, t0, check_b_inf
    # if (mant_a) return a
    bne s4, x0, return_a
    # if (exp_b != 0xFF) return a 
    bne s3, t0, return_a
    # return (mant_b || sign_a == sign_b) ? b : BF16_NAN()
    bne s5, x0, return_b
    beq s0, s1, return_b
    jal ra, nan
    j add_end
check_b_inf:
    # if (exp_b == 0xFF) return b
    li t0, 0xFF
    beq s3, t0, return_b
    # if (!exp_a && !mant_a) return b
    or t0, s2, s4
    beq t0, x0, return_b
    # if (!exp_b && !mant_b) return a
    or t0, s3, s5
    beq t0, x0, return_a
    # if (exp_a) mant_a |= 0x80
    beq s2, x0, skip_norm_a
    ori s4, s4, 0x80                   # mant_a |= 0x80
skip_norm_a:
    # if (exp_b) mant_b |= 0x80
    beq s3, x0, skip_norm_b
    ori s5, s5, 0x80                   # mant_b |= 0x80
skip_norm_b:
    # exp_diff = exp_a - exp_b
    sub t0, s2, s3                     # t0 = exp_diff
    # if (exp_diff > 0)
    blt t0, x0, exp_diff_lt_0
    mv s7, s2                          # result_exp = exp_a
    li t1, 8
    blt t1, t0, return_a               # if (exp_diff > 8) return a
    srl s5, s5, t0                     # mant_b >>= exp_diff
    j after_align
exp_diff_lt_0:
    # else if (exp_diff < 0)
    beq t0, x0, exp_diff_eq_0
    mv s7, s3                          # result_exp = exp_b
    sub t0, x0, t0                     # t0 = -exp_diff
    li t1, 8
    blt t1, t0, return_b               # if (exp_diff < -8) return b
    srl s4, s4, t0                     # mant_a >>= -exp_diff
    j after_align
after_align:
    # if (sign_a == sign_b)
    bne s0, s1, diff_sign
    mv s6, s0                          # result_sign = sign_a
    add s8, s4, s5                     # result_mant = mant_a + mant_b
    # if (result_mant & 0x100)
    andi t0, s8, 0x100
    beq t0, x0, build_result
    srli s8, s8, 1                     # result_mant >>= 1
    addi s7, s7, 1                     # result_exp++
    # if (++result_exp >= 0xFF) return (result_sign << 15) | 0x7F80
    li t0, 0xFF
    blt s7, t0, build_result
    slli a0, s6, 15                    # t0 = result_sign << 15
    li t0, 0x7F80
    or a0, a0, t0                      # t0 = (result_sign << 15) | 0x7F80
    j add_end
diff_sign:
    # if (mant_a >= mant_b)
    bltu s4, s5, b_larger
    mv s6, s0                          # result_sign = sign_a
    sub s8, s4, s5                     # result_mant = mant_a - mant_b
    j after_sub
b_larger:
    mv s6, s1                          # result_sign = sign_b
    sub s8, s5, s4                     # result_mant = mant_b - mant_a
after_sub:
    # if (!result_mant) return BF16_ZERO
    beq s8, x0, return_zero
    mv a0, s8                          # a0 = result_mant
    jal ra, clz                        # shift_amount = clz(result_mant)
    addi t0, a0, -24                   # t0 = clz(result_mant) - 24
    ble s7, t0, return_zero            # if (result_exp <= shift_amount) return zero
    sub s7, s7, t0                     # result_exp -= shift_amount
    sll s8, s8, t0                     # result_mant <<= shift_amount
    j build_result
build_result:
    # return (result_sign << 15) | ((result_exp & 0xFF) << 7) | (result_mant & 0x7F)
    slli a0, s6, 15                    # a0 = result_sign << 15
    andi t0, s7, 0xFF                  # t0 = result_exp & 0xFF
    slli t0, t0, 7                     # t0 = (result_exp & 0xFF) << 7
    or a0, a0, t0                      # a0 |= t0
    andi t0, s8, 0x7F                  # t0 = result_mant & 0x7F
    or a0, a0, t0
    j add_end
exp_diff_eq_0:
    mv s7, s2                          # result_exp = exp_a
return_a:
    mv a0, s9                          # return a.bits
    j add_end
return_b:
    mv a0, s10                         # return b.bits
    j add_end
return_zero:
    jal ra, zero
add_end:
    lw ra, 44(sp)
    lw s0, 40(sp)
    lw s1, 36(sp)
    lw s2, 32(sp)
    lw s3, 28(sp)
    lw s4, 24(sp)
    lw s5, 20(sp)
    lw s6, 16(sp)
    lw s7, 12(sp)
    lw s8, 8(sp)
    lw s9, 4(sp)
    lw s10, 0(sp)
    addi sp, sp, 48
    ret
bf16_sub:
    li t0, BF16_SIGN_MASK
    xor a1, a1, t0               # b.bits ^= BF16_SIGN_MASK
    j bf16_add
bf16_mul:
    # a0 = bf16_t a.bits
    # a1 = bf16_t b.bits
    addi sp, sp, -52
    sw ra, 48(sp)
    sw s0, 44(sp)                # s0 = sign_a
    sw s1, 40(sp)                # s1 = sign_b
    sw s2, 36(sp)                # s2 = exp_a
    sw s3, 32(sp)                # s3 = exp_b
    sw s4, 28(sp)                # s4 = mant_a
    sw s5, 24(sp)                # s5 = mant_b
    sw s6, 20(sp)                # s6 = result_sign
    sw s7, 16(sp)                # s7 = result_exp
    sw s8, 12(sp)                # s8 = result_mant
    sw s9, 8(sp)                 # s9 = a.bits
    sw s10, 4(sp)                # s10 = b.bits
    sw s11, 0(sp)                # s11 = exp_adjust
    mv s9, a0                    # s9 = a0 = a.bits
    mv s10, a1                   # s10 = b0 = b.bits
    # extract sign_a, sign_b
    srli s0, s9, 15              # s0 = sign_a = a.bits >> 15
    andi s0, s0, 1               # s0 = sign_a = (a.bits >> 15) & 1
    srli s1, s10, 15             # s1 = sign_b = b.bits >> 15
    andi s1, s1, 1               # s1 = sign_b = (b.bits >> 15) & 1
    # extract exp_a, exp_b
    srli s2, s9, 7               # s2 = exp_a = a.bits >> 7
    andi s2, s2, 0xFF            # s2 = exp_a = (a.bits >> 7) & 0xFF
    srli s3, s10, 7              # s3 = exp_b = b.bits >> 7
    andi s3, s3, 0xFF            # s3 = exp_b = (b.bits >> 7) & 0xFF
    # extract mant_a, mant_b
    andi s4, s9, 0x7F            # s4 = mant_a = a.bits & 0x7F
    andi s5, s10, 0x7F           # s5 = mant_b = b.bits & 0x7F
    # result_sign = sign_a ^ sign_b
    xor s6, s0, s1               # s6 = result_sign
    # if (exp_a == 0xFF)
    li t0, 0xFF
    bne s2, t0, mul_check_b_inf
    # if (mant_a) return a
    bne s4, x0, mul_return_a
    # if (!exp_b && !mant_b) return return BF16_NAN()
    or t0, s3, s5
    beq t0, x0, mul_return_nan
    # return (result_sign << 15) | 0x7F80
    slli a0, s6, 15              # a0 = result_sign << 15
    li t0, 0x7F80
    or a0, a0, t0                # a0 = (result_sign << 15) | 0x7F80
    j mul_end
mul_check_b_inf:
    # if (exp_b == 0xFF)
    li t0, 0xFF
    bne s3, t0, mul_check_zero
    # if (mant_b) return b
    bne s5, x0, mul_return_b
    # if (!exp_a && !mant_a) return return BF16_NAN()
    or t0, s2, s4
    beq t0, x0, mul_return_nan
    # return (result_sign << 15) | 0x7F80
    slli a0, s6, 15              # a0 = result_sign << 15
    li t0, 0x7F80
    or a0, a0, t0                # a0 = (result_sign << 15) | 0x7F80
    j mul_end
mul_check_zero:
    # if ((!exp_a && !mant_a) || (!exp_b && !mant_b))
    or t0, s2, s4
    beq t0, x0, mul_return_sign_zero
    or t0, s3, s5
    beq t0, x0, mul_return_sign_zero
    li s11, 0                    # s11 = exp_adjust
    # if (!exp_a)
    bne s2, x0, mul_normalize_a_end
mul_normalize_a_loop:
    andi t0, s4, 0x80
    bne t0, x0, mul_normalize_a_set_exp
    slli s4, s4, 1               # mant_a <<= 1
    addi s11, s11, -1            # exp_adjust--
    j mul_normalize_a_loop
mul_normalize_a_set_exp:
    li s2, 1                     # exp_a = 1
    j mul_normalize_b_check
mul_normalize_a_end:
    ori s4, s4, 0x80             # mant_a |= 0x80
mul_normalize_b_check:
    # if (!exp_b)
    bne s3, x0, mul_normalize_b_end
mul_normalize_b_loop:
    andi t0, s5, 0x80
    bne t0, x0, mul_normalize_b_set_exp
    slli s5, s5, 1               # mant_b <<= 1
    addi s11, s11, -1            # exp_adjust--
    j mul_normalize_b_loop
mul_normalize_b_set_exp:
    li s3, 1                     # exp_b = 1
    j mul_multiply
mul_normalize_b_end:
    ori s5, s5, 0x80             # mant_b |= 0x80
mul_multiply:
    # result_mant = (uint32_t) mant_a * mant_b
    li s8, 0                     # s8 = result_mant = 0
    mv t0, s4                    # t0 = mant_a
    mv t1, s5                    # t1 = mant_b
mul_multiply_loop:
    beq t1, x0, mul_multiply_end
    andi t2, t1, 1               # t2 = t1 & 1
    beq t2, x0, mul_multiply_skip_add
    add s8, s8, t0               # result_mant += mant_a
mul_multiply_skip_add:
    slli t0, t0, 1               # mant_a <<= 1
    srli t1, t1, 1               # mant_b >>= 1
    j mul_multiply_loop
mul_multiply_end:
    # result_exp = (int32_t) exp_a + exp_b - BF16_EXP_BIAS + exp_adjust
    add s7, s2, s3               # result_exp = exp_a + exp_b
    addi s7, s7, -127            # result_exp -= BF16_EXP_BIAS
    add s7, s7, s11              # result_exp += exp_adjust
    # if (result_mant & 0x8000)
    li t0, 0x8000
    and t1, s8, t0
    beq t1, x0, mul_shift_7
    # result_mant = (result_mant >> 8) & 0x7F
    srli s8, s8, 8               # result_mant >>= 8
    andi s8, s8, 0x7F            # (result_mant >> 8) & 0x7F 
    addi s7, s7, 1               # result_exp++
    j mul_check_exp
mul_shift_7:
    # result_mant = (result_mant >> 7) & 0x7F
    srli s8, s8, 7               # result_mant >>= 7
    andi s8, s8, 0x7F            # (result_mant >> 7) & 0x7F
mul_check_exp:
    # if (result_exp >= 0xFF) return (result_sign << 15) | 0x7F80
    li t0, 0xFF
    blt s7, t0, mul_check_underflow
    slli a0, s6, 15              # a0 = result_sign << 15
    li t0, 0x7F80
    or a0, a0, t0                # a0 = (result_sign << 15) | 0x7F80
    j mul_end
mul_check_underflow:
    # if (result_exp <= 0)
    blt x0, s7, mul_build_result
    # if (result_exp < -6) return result_sign << 15
    li t0, -6
    blt s7, t0, mul_return_sign_zero
    # result_mant >>= (1 - result_exp)
    li t0, 1
    sub t0, t0, s7               # t0 = 1 - result
    srl s8, s8, t0               # result_mant >>= (1 - result_exp)
    li s7, 0                     # result_exp = 0
mul_build_result:
    # return (result_sign << 15) | ((result_exp & 0xFF) << 7) | (result_mant & 0x7F)
    slli a0, s6, 15              # a0 = result_sign << 15
    andi t0, s7, 0xFF            # t0 = result_exp & 0xFF
    slli t0, t0, 7               # t0 = (result_exp & 0xFF) << 7
    or a0, a0, t0                # a0 |= t0
    andi t0, s8, 0x7F            # t0 = result_mant & 0x7F
    or a0, a0, t0
    j mul_end
mul_return_a:
    mv a0, s9                    # return a.bits
    j mul_end
mul_return_b:
    mv a0, s10                   # return b.bits
    j mul_end
mul_return_nan:
    jal ra, nan
    j mul_end
mul_return_sign_zero:
    slli a0, s6, 15              # a0 = result_sign << 15
mul_end:
    lw ra, 48(sp)
    lw s0, 44(sp)
    lw s1, 40(sp)
    lw s2, 36(sp)
    lw s3, 32(sp)
    lw s4, 28(sp)
    lw s5, 24(sp)
    lw s6, 20(sp)
    lw s7, 16(sp)
    lw s8, 12(sp)
    lw s9, 8(sp)
    lw s10, 4(sp)
    lw s11, 0(sp)
    addi sp, sp, 52
    ret
bf16_div:
    # a0 = bf16_t a.bits
    # a1 = bf16_t b.bits
    addi sp, sp, -52
    sw ra, 48(sp)
    sw s0, 44(sp)                # s0 = sign_a
    sw s1, 40(sp)                # s1 = sign_b
    sw s2, 36(sp)                # s2 = exp_a
    sw s3, 32(sp)                # s3 = exp_b
    sw s4, 28(sp)                # s4 = mant_a
    sw s5, 24(sp)                # s5 = mant_b
    sw s6, 20(sp)                # s6 = result_sign
    sw s7, 16(sp)                # s7 = result_exp
    sw s8, 12(sp)                # s8 = quotient
    sw s9, 8(sp)                 # s9 = a.bits
    sw s10, 4(sp)                # s10 = b.bits
    sw s11, 0(sp)                # s11 = dividend
    mv s9, a0                    # s9 = a0 = a.bits
    mv s10, a1                   # s10 = b0 = b.bits
    # extract sign_a, sign_b
    srli s0, s9, 15              # s0 = sign_a = a.bits >> 15
    andi s0, s0, 1               # s0 = sign_a = (a.bits >> 15) & 1
    srli s1, s10, 15             # s1 = sign_b = b.bits >> 15
    andi s1, s1, 1               # s1 = sign_b = (b.bits >> 15) & 1
    # extract exp_a, exp_b
    srli s2, s9, 7               # s2 = exp_a = a.bits >> 7
    andi s2, s2, 0xFF            # s2 = exp_a = (a.bits >> 7) & 0xFF
    srli s3, s10, 7              # s3 = exp_b = b.bits >> 7
    andi s3, s3, 0xFF            # s3 = exp_b = (b.bits >> 7) & 0xFF
    # extract mant_a, mant_b
    andi s4, s9, 0x7F            # s4 = mant_a = a.bits & 0x7F
    andi s5, s10, 0x7F           # s5 = mant_b = b.bits & 0x7F
    # result_sign = sign_a ^ sign_b
    xor s6, s0, s1               # s6 = result_sign
    # if (exp_b == 0xFF)
    li t0, 0xFF
    bne s3, t0, div_check_b_zero
    # if (mant_b) return b
    bne s5, x0, div_return_b
    # Inf/Inf = NaN if (exp_a == 0xFF && !mant_a) return BF16_NAN()
    li t0, 0xFF
    bne s2, t0, div_b_inf_zero
    bne s4, x0, div_b_inf_zero
    j div_return_nan
div_b_inf_zero:
    # return result_sign << 15
    slli a0, s6, 15              # a0 = result_sign << 15
    j div_end
div_check_b_zero:
    # if (!exp_b && !mant_b) (b is zero)
    or t0, s3, s5
    bne t0, x0, div_check_a_inf
    # if (!exp_a && !mant_a) (a is zero) (0/0 = NaN)
    or t0, s2, s4
    beq t0, x0, div_return_nan
    # return (result_sign << 15) | 0x7F80
    slli a0, s6, 15              # a0 = result_sign << 15
    li t0, 0x7F80
    or a0, a0, t0                # a0 = (result_sign << 15) | 0x7F80
    j div_end
div_check_a_inf:
    # if (exp_a == 0xFF)
    li t0, 0xFF
    bne s2, t0, div_check_a_zero
    # if (mant_a) return a
    bne s4, x0, div_return_a
    # return (result_sign << 15) | 0x7F80
    slli a0, s6, 15              # a0 = result_sign << 15
    li t0, 0x7F80
    or a0, a0, t0                # a0 = (result_sign << 15) | 0x7F80
    j div_end
div_check_a_zero:
    # if (!exp_a && !mant_a) return result_sign << 15
    or t0, s2, s4
    bne t0, x0, div_normalize
    slli a0, s6, 15              # a0 = result_sign << 15
    j div_end
div_normalize:
    # if (exp_a) mant_a |= 0x80
    beq s2, x0, div_skip_norm_a
    ori s4, s4, 0x80             # mant_a |= 0x80
div_skip_norm_a:
    # if (exp_b) mant_b |= 0x80
    beq s3, x0, div_division
    ori s5, s5, 0x80             # mant_b |= 0x80
div_division:
    # dividend = (uint32_t) mant_a << 15
    slli s11, s4, 15             # s11 = dividend = mant_a << 15
    li s8, 0                     # s8 = quotient = 0
    # for (int i = 0; i < 16; i++)
    li t1, 0                     # i = 0
div_div_loop:
    li t0, 16
    beq t1, t0, div_div_end      # if (i == 16) loop end
    slli s8, s8, 1               # quotient <<= 1
    # if (dividend >= (divisor << (15 - i)))
    li t0, 15
    sub t0, t0, t1               # t0 = 15 - i
    sll t2, s5, t0               # t2 = divisor << (15 - i)
    bltu s11, t2, div_div_skip_sub
    # dividend -= (divisor << (15 - i));
    sub s11, s11, t2             # dividend -= t2
    ori s8, s8, 1                # quotient |= 1;
div_div_skip_sub:
    addi t1, t1, 1               # i++
    j div_div_loop
div_div_end:
    # result_exp = (int32_t) exp_a - exp_b + BF16_EXP_BIAS
    sub s7, s2, s3               # result_exp = exp_a - exp_b
    addi s7, s7, 127             # result_exp += BF16_EXP_BIAS
    # if (!exp_a) result_exp--;
    bne s2, x0, div_check_exp_b
    addi s7, s7, -1              # result_exp--
div_check_exp_b:
    # if (!exp_b) result_exp++;
    bne s3, x0, div_check_quotient
    addi s7, s7, 1               # result_exp++
div_check_quotient:
    # if (quotient & 0x8000)
    li t0, 0x8000
    and t1, s8, t0
    beq t1, x0, div_norm_loop
    srli s8, s8, 8               # quotient >>= 8
    j div_mask_quotient
div_norm_loop:
    # while (!(quotient & 0x8000) && result_exp > 1)
    li t0, 0x8000
    and t1, s8, t0
    bne t1, x0, div_shift_right
    li t0, 1
    bge t0, s7, div_shift_right
    slli s8, s8, 1               # quotient <<= 1
    addi s7, s7, -1              # result_exp--
    j div_norm_loop
div_shift_right:
    srli s8, s8, 8               # quotient >>= 8
div_mask_quotient:
    andi s8, s8, 0x7F            # quotient &= 0x7F
    # if (result_exp >= 0xFF) return (result_sign << 15) | 0x7F80
    li t0, 0xFF
    blt s7, t0, div_check_underflow
    slli a0, s6, 15              # a0 = result_sign << 15
    li t0, 0x7F80
    or a0, a0, t0                # a0 = (result_sign << 15) | 0x7F80
    j div_end
div_check_underflow:
    # if (result_exp <= 0) return result_sign << 15
    blt x0, s7, div_build_result
    slli a0, s6, 15              # a0 = result_sign << 15
    j div_end
div_build_result:
    # return (result_sign << 15) | ((result_exp & 0xFF) << 7) | (quotient & 0x7F)
    slli a0, s6, 15              # a0 = result_sign << 15
    andi t0, s7, 0xFF            # t0 = result_exp & 0xFF
    slli t0, t0, 7               # t0 = (result_exp & 0xFF) << 7
    or a0, a0, t0                # a0 |= (result_exp & 0xFF) << 7
    andi t0, s8, 0x7F            # t0 = quotient & 0x7F
    or a0, a0, t0                # a0 |= (quotient & 0x7F)
    j div_end
div_return_a:
    mv a0, s9                    # return a.bits
    j div_end
div_return_b:
    mv a0, s10                   # return b.bits
    j div_end
div_return_nan:
    jal ra, nan
div_end:
    lw ra, 48(sp)
    lw s0, 44(sp)
    lw s1, 40(sp)
    lw s2, 36(sp)
    lw s3, 32(sp)
    lw s4, 28(sp)
    lw s5, 24(sp)
    lw s6, 20(sp)
    lw s7, 16(sp)
    lw s8, 12(sp)
    lw s9, 8(sp)
    lw s10, 4(sp)
    lw s11, 0(sp)
    addi sp, sp, 52
    ret
bf16_sqrt:
    # a0 = bf16_t a.bits
    addi sp, sp, -40
    sw ra, 36(sp)
    sw s0, 32(sp)                # s0 = sign
    sw s1, 28(sp)                # s1 = exp
    sw s2, 24(sp)                # s2 = mant
    sw s3, 20(sp)                # s3 = new_exp
    sw s4, 16(sp)                # s4 = m (mantissa)
    sw s5, 12(sp)                # s5 = low
    sw s6, 8(sp)                 # s6 = high
    sw s7, 4(sp)                 # s7 = result
    sw s8, 0(sp)                 # s8 = a.bits
    mv s8, a0                    # s8 = a0 = a.bits
    # extract sign
    srli s0, s8, 15              # s0 = sign = a
    andi s0, s0, 1               # s0 = sign = (a.bits >> 15) & 1
    # extract exp
    srli s1, s8, 7               # s1 = exp = a
    andi s1, s1, 0xFF            # s1 = exp = (a.bits >> 7) & 0xFF
    # extract mant
    andi s2, s8, 0x7F            # s2 = mant = a & 0x7F
    # if (exp == 0xFF)
    li t0, 0xFF
    bne s1, t0, sqrt_check_zero
    # if (mant) return a
    bne s2, x0, sqrt_return_a
    # if (sign) return BF16_NAN()
    bne s0, x0, sqrt_return_nan
    # return a
    j sqrt_return_a
sqrt_check_zero:
    # if (!exp && !mant) return BF16_ZERO()
    or t0, s1, s2
    beq t0, x0, sqrt_return_zero
    # if (sign) return BF16_NAN()
    bne s0, x0, sqrt_return_nan
    # if (!exp) return BF16_ZERO()
    beq s1, x0, sqrt_return_zero
    addi t0, s1, -127            # t0 = e = exp - BF16_EXP_BIAS
    ori s4, s2, 0x80             # s4 = m = mant | 0x80
    # if (e & 1)
    andi t1, t0, 1               # t1 = e & 1
    beq t1, x0, sqrt_even_exp
    slli s4, s4, 1               # m <<= 1
    # new_exp = ((e - 1) >> 1) + BF16_EXP_BIAS
    addi t0, t0, -1              # t0 = e - 1
    srai s3, t0, 1               # s3 = (e - 1) >> 1
    addi s3, s3, 127             # s3 = new_exp = ((e - 1) >> 1) + BF16_EXP_BIAS
    j sqrt_binary_search
sqrt_even_exp:
    # new_exp = (e >> 1) + BF16_EXP_BIAS
    srai s3, t0, 1               # s3 = e >> 1
    addi s3, s3, 127             # s3 = new_exp = (e >> 1) + BF16_EXP_BIAS
sqrt_binary_search:
    li s5, 90                    # s5 = low = 90
    li s6, 256                   # s6 = high = 256
    li s7, 128                   # s7 = result = 128
sqrt_bsearch_loop:
    bltu s6, s5, sqrt_bsearch_end
    # mid = (low + high) >> 1
    add t0, s5, s6               # t0 = low + high
    srli t0, t0, 1               # t0 = mid = (low + high) >> 1
    # sq = (mid * mid) / 128
    li t1, 0                     # t1 = sq = 0
    mv t2, t0                    # t2 = multiplicand = mid
    mv t3, t0                    # t3 = multiplier = mid
sqrt_mul_loop:
    beq t3, x0, sqrt_mul_end     # if (multiplier == 0) end loop
    andi t4, t3, 1               # t4 = multiplier & 1
    beq t4, x0, sqrt_mul_skip_add
    add t1, t1, t2               # sq += multiplicand
sqrt_mul_skip_add:
    slli t2, t2, 1               # multiplicand <<= 1
    srli t3, t3, 1               # multiplier >>= 1
    j sqrt_mul_loop
sqrt_mul_end:
    srli t1, t1, 7               # sq /= 128
    # if (sq <= m)
    bltu s4, t1, sqrt_sq_gt_m
    mv s7, t0                    # result = mid
    addi s5, t0, 1               # low = mid + 1
    j sqrt_bsearch_loop
sqrt_sq_gt_m:
    addi s6, t0, -1              # high = mid - 1
    j sqrt_bsearch_loop
sqrt_bsearch_end:
    # if (result >= 256)
    li t0, 256
    blt s7, t0, sqrt_check_underflow
    srli s7, s7, 1               # result >>= 1
    addi s3, s3, 1               # new_exp++
    j sqrt_extract_mant
sqrt_check_underflow:
    # else if (result < 128)
    li t0, 128
    bge s7, t0, sqrt_extract_mant
sqrt_norm_loop:
    # while (result < 128 && new_exp > 1)
    li t0, 128
    bge s7, t0, sqrt_extract_mant
    li t0, 1
    bge t0, s3, sqrt_extract_mant
    slli s7, s7, 1               # result <<= 1
    addi s3, s3, -1              # new_exp--
    j sqrt_norm_loop
sqrt_extract_mant:
    # new_mant = result & 0x7F
    andi s7, s7, 0x7F            # new_mant = result & 0x7F
    # if (new_exp >= 0xFF) return 0x7F80
    li t0, 0xFF
    blt s3, t0, sqrt_check_exp_underflow
    li a0, 0x7F80
    j sqrt_end
sqrt_check_exp_underflow:
    # if (new_exp <= 0) return BF16_ZERO()
    blt x0, s3, sqrt_build_result
    j sqrt_return_zero
sqrt_build_result:
    # return ((new_exp & 0xFF) << 7) | new_mant
    andi t0, s3, 0xFF            # t0 = new_exp & 0xFF
    slli t0, t0, 7               # t0 = (new_exp & 0xFF) << 7
    or a0, t0, s7
    j sqrt_end
sqrt_return_a:
    mv a0, s8                    # return a.bits
    j sqrt_end
sqrt_return_nan:
    jal ra, nan
    j sqrt_end
sqrt_return_zero:
    jal ra, zero
sqrt_end:
    lw ra, 36(sp)
    lw s0, 32(sp)
    lw s1, 28(sp)
    lw s2, 24(sp)
    lw s3, 20(sp)
    lw s4, 16(sp)
    lw s5, 12(sp)
    lw s6, 8(sp)
    lw s7, 4(sp)
    lw s8, 0(sp)
    addi sp, sp, 40
    ret
clz:
    # if (x == 0) return 32
    bne  a0, x0, clz_start
    li   a0, 32
    ret                      
clz_start:
    li   a1, 0               # a1 = n = 0
    srli t0, a0, 16
    bne  t0, x0, check_8 
    addi a1, a1, 16          # n += 16
    slli a0, a0, 16          # x <<= 16
check_8:
    srli t0, a0, 24
    bne  t0, x0, check_4
    addi a1, a1, 8           # n += 8
    slli a0, a0, 8           # x <<= 8
check_4:
    srli t0, a0, 28
    bne  t0, x0, check_2
    addi a1, a1, 4           # n += 4
    slli a0, a0, 4           # x <<= 4
check_2:
    srli t0, a0, 30
    bne  t0, x0, check_1
    addi a1, a1, 2           # n += 2
    slli a0, a0, 2           # x <<= 2
check_1:
    blt a0, x0, clz_end      # if (x < 0), MSB is 1, we are done
    addi a1, a1, 1           # n += 1
clz_end:
    mv   a0, a1              # move result to a0 for return
    ret