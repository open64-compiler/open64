.text
.globl swap_frays
.type swap_frays, @function

    # rdi points to savearea for current stack pointer
    # rsi points to savearea for prior stack pointer

    swap_frays:

       push  %rbp
       push  %rbx
       push  %r12
       push  %r13
       push  %r14
       push  %r15
       mov   %rsp, (%rdi)
       mov   (%rsi), %rsp
       pop   %r15
       pop   %r14
       pop   %r13
       pop   %r12
       pop   %rbx
       pop   %rbp
       ret
