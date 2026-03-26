	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	___toka_panic_handler           ## -- Begin function __toka_panic_handler
	.p2align	4, 0x90
___toka_panic_handler:                  ## @__toka_panic_handler
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	movl	$0, -12(%rsp)
	.p2align	4, 0x90
LBB0_1:                                 ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	jmp	LBB0_1
	.cfi_endproc
                                        ## -- End function
	.globl	_panic                          ## -- Begin function panic
	.p2align	4, 0x90
_panic:                                 ## @panic
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	callq	___toka_panic_handler
	popq	%rax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_assert                         ## -- Begin function assert
	.p2align	4, 0x90
_assert:                                ## @assert
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	andb	$1, %dil
	movb	%dil, 7(%rsp)
	movq	%rsi, 8(%rsp)
	movl	$0, 20(%rsp)
	jne	LBB2_2
## %bb.1:                               ## %then
	movq	8(%rsp), %rdi
	callq	_panic
LBB2_2:                                 ## %ifcont
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_encap_String_clone             ## -- Begin function encap_String_clone
	.p2align	4, 0x90
_encap_String_clone:                    ## @encap_String_clone
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	callq	_String_c_str
	movq	(%rsp), %rcx
	movl	8(%rcx), %esi
	movq	%rax, %rdi
	callq	_String_from_with_len
	popq	%rsi
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_encap_String_drop              ## -- Begin function encap_String_drop
	.p2align	4, 0x90
_encap_String_drop:                     ## @encap_String_drop
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -16(%rsp)
	movl	$0, -4(%rsp)
	cmpq	$0, (%rdi)
	je	LBB4_2
## %bb.1:                               ## %then
	movq	-16(%rsp), %rax
	movq	$0, (%rax)
	movq	-16(%rsp), %rax
	movl	$0, 8(%rax)
	movq	-16(%rsp), %rax
	movl	$0, 12(%rax)
LBB4_2:                                 ## %ifcont
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_encap_SplitIterator_drop       ## -- Begin function encap_SplitIterator_drop
	.p2align	4, 0x90
_encap_SplitIterator_drop:              ## @encap_SplitIterator_drop
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_encap_SplitIterator_clone      ## -- Begin function encap_SplitIterator_clone
	.p2align	4, 0x90
_encap_SplitIterator_clone:             ## @encap_SplitIterator_clone
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, %rax
	movq	%rsi, -8(%rsp)
	movq	(%rsi), %rcx
	movq	%rcx, -32(%rsp)
	movl	8(%rsi), %edx
	movl	%edx, -24(%rsp)
	movl	12(%rsi), %edx
	movl	%edx, -20(%rsp)
	movzbl	16(%rsi), %edx
	movb	%dl, -16(%rsp)
	movq	-24(%rsp), %rsi
	movq	%rsi, 8(%rdi)
	movq	%rcx, (%rdi)
	movb	%dl, 16(%rdi)
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_SplitIterator_next             ## -- Begin function SplitIterator_next
	.p2align	4, 0x90
_SplitIterator_next:                    ## @SplitIterator_next
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movl	$0, -12(%rbp)
	movl	12(%rdi), %eax
	cmpl	8(%rdi), %eax
	jl	LBB7_3
## %bb.1:                               ## %then
	movq	-8(%rbp), %rax
	movl	8(%rax), %ecx
	incl	%ecx
	movl	%ecx, 12(%rax)
	leaq	L___unnamed_1(%rip), %rdi
	callq	_String_from
	jmp	LBB7_2
LBB7_3:                                 ## %else
	movq	-8(%rbp), %rax
	movl	12(%rax), %ecx
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rax
	movq	%rax, %rsp
	movl	%ecx, -16(%rdx)
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rcx
	movq	%rcx, %rsp
	movb	$0, -16(%rsi)
	movl	-16(%rdx), %esi
	movq	%rsp, %rdi
	leaq	-16(%rdi), %rdx
	movq	%rdx, %rsp
	movl	%esi, -16(%rdi)
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rsp
	movl	$0, -16(%rsi)
	.p2align	4, 0x90
LBB7_4:                                 ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	movl	(%rdx), %esi
	movq	-8(%rbp), %rdi
	cmpl	8(%rdi), %esi
	jge	LBB7_7
## %bb.5:                               ## %while_loop
                                        ##   in Loop: Header=BB7_4 Depth=1
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rsp
	movl	$0, -16(%rsi)
	movslq	(%rdx), %rsi
	movq	-8(%rbp), %rdi
	movq	(%rdi), %r8
	movzbl	(%r8,%rsi), %esi
	cmpb	16(%rdi), %sil
	je	LBB7_6
## %bb.11:                              ## %else14
                                        ##   in Loop: Header=BB7_4 Depth=1
	incl	(%rdx)
	jmp	LBB7_4
LBB7_6:                                 ## %then13
	movb	$1, (%rcx)
LBB7_7:                                 ## %while_after
	movl	(%rdx), %edi
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rsi
	movq	%rsi, %rsp
	movl	%edi, -16(%rdx)
	subl	(%rax), %edi
	movq	%rsp, %r8
	leaq	-16(%r8), %rdx
	movq	%rdx, %rsp
	movl	%edi, -16(%r8)
	movq	%rsp, %rdi
	leaq	-16(%rdi), %rsp
	movl	$0, -16(%rdi)
	cmpb	$1, (%rcx)
	jne	LBB7_9
## %bb.8:                               ## %then19
	movl	(%rsi), %ecx
	incl	%ecx
	movq	-8(%rbp), %rsi
	movl	%ecx, 12(%rsi)
	jmp	LBB7_10
LBB7_9:                                 ## %else23
	movq	-8(%rbp), %rcx
	movl	8(%rcx), %esi
	incl	%esi
	movl	%esi, 12(%rcx)
LBB7_10:                                ## %ifcont29
	movq	-8(%rbp), %rcx
	movslq	(%rax), %rdi
	addq	(%rcx), %rdi
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movq	%rdi, -16(%rax)
	movl	(%rdx), %esi
	callq	_String_from_with_len
LBB7_2:                                 ## %then
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_SplitIterator_is_done          ## -- Begin function SplitIterator_is_done
	.p2align	4, 0x90
_SplitIterator_is_done:                 ## @SplitIterator_is_done
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	movl	12(%rdi), %eax
	cmpl	8(%rdi), %eax
	setg	%al
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_from                    ## -- Begin function String_from
	.p2align	4, 0x90
_String_from:                           ## @String_from
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, 16(%rsp)
	callq	_strlen
	movq	%rax, (%rsp)
	leaq	1(%rax), %rdi
	callq	_malloc
	movq	%rax, 8(%rsp)
	movq	16(%rsp), %rsi
	movq	(%rsp), %rdx
	movq	%rax, %rdi
	callq	_memcpy
	movq	(%rsp), %rax
	movq	8(%rsp), %rcx
	movb	$0, (%rcx,%rax)
	movq	8(%rsp), %rax
	movq	%rax, 24(%rsp)
	movl	(%rsp), %edx
	movl	%edx, 32(%rsp)
	leal	1(%rdx), %ecx
	movl	%ecx, 36(%rsp)
                                        ## kill: def $edx killed $edx killed $rdx
	addq	$40, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_from_with_len           ## -- Begin function String_from_with_len
	.p2align	4, 0x90
_String_from_with_len:                  ## @String_from_with_len
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
                                        ## kill: def $esi killed $esi def $rsi
	movq	%rdi, 24(%rsp)
	movl	%esi, 36(%rsp)
	movl	%esi, 12(%rsp)
	leal	1(%rsi), %edi
	callq	_malloc
	movq	%rax, 16(%rsp)
	movq	24(%rsp), %rsi
	movl	12(%rsp), %edx
	movq	%rax, %rdi
	callq	_memcpy
	movslq	12(%rsp), %rax
	movq	16(%rsp), %rcx
	movb	$0, (%rcx,%rax)
	movq	16(%rsp), %rax
	movq	%rax, 40(%rsp)
	movl	12(%rsp), %edx
	movl	%edx, 48(%rsp)
	leal	1(%rdx), %ecx
	movl	%ecx, 52(%rsp)
                                        ## kill: def $edx killed $edx killed $rdx
	addq	$56, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_from_take               ## -- Begin function String_from_take
	.p2align	4, 0x90
_String_from_take:                      ## @String_from_take
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, 16(%rsp)
	leaq	L___unnamed_2(%rip), %rsi
	movl	$23, %edx
	movl	$2, %edi
	callq	_write
	movq	16(%rsp), %rdi
	callq	_strlen
	movq	%rax, (%rsp)
	leaq	L___unnamed_3(%rip), %rsi
	movl	$29, %edx
	movl	$2, %edi
	callq	_write
	movq	(%rsp), %rdi
	incq	%rdi
	callq	_malloc
	movq	%rax, 8(%rsp)
	movq	16(%rsp), %rsi
	movq	(%rsp), %rdx
	movq	%rax, %rdi
	callq	_memcpy
	movq	(%rsp), %rax
	movq	8(%rsp), %rcx
	movb	$0, (%rcx,%rax)
	leaq	L___unnamed_4(%rip), %rsi
	movl	$30, %edx
	movl	$2, %edi
	callq	_write
	movq	8(%rsp), %rax
	movq	%rax, 24(%rsp)
	movl	(%rsp), %edx
	movl	%edx, 32(%rsp)
	leal	1(%rdx), %ecx
	movl	%ecx, 36(%rsp)
                                        ## kill: def $edx killed $edx killed $rdx
	addq	$40, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_from_int                ## -- Begin function String_from_int
	.p2align	4, 0x90
_String_from_int:                       ## @String_from_int
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movl	%edi, 4(%rsp)
	movl	$16, %edi
	callq	_malloc
	movq	%rax, 8(%rsp)
	movl	4(%rsp), %edx
	leaq	L___unnamed_5(%rip), %rsi
	movq	%rax, %rdi
	xorl	%eax, %eax
	callq	_sprintf
	movq	8(%rsp), %rdi
	callq	_strlen
	movq	%rax, %rdx
	movq	%rax, 32(%rsp)
	movq	8(%rsp), %rax
	movq	%rax, 16(%rsp)
	movl	%edx, 24(%rsp)
	movl	$16, 28(%rsp)
                                        ## kill: def $edx killed $edx killed $rdx
	movl	$16, %ecx
	addq	$40, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_from_i64                ## -- Begin function String_from_i64
	.p2align	4, 0x90
_String_from_i64:                       ## @String_from_i64
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, 8(%rsp)
	movl	$32, %edi
	callq	_malloc
	movq	%rax, (%rsp)
	movq	8(%rsp), %rdx
	leaq	L___unnamed_6(%rip), %rsi
	movq	%rax, %rdi
	xorl	%eax, %eax
	callq	_sprintf
	movq	(%rsp), %rdi
	callq	_strlen
	movq	%rax, %rdx
	movq	%rax, 32(%rsp)
	movq	(%rsp), %rax
	movq	%rax, 16(%rsp)
	movl	%edx, 24(%rsp)
	movl	$32, 28(%rsp)
                                        ## kill: def $edx killed $edx killed $rdx
	movl	$32, %ecx
	addq	$40, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_from_f64                ## -- Begin function String_from_f64
	.p2align	4, 0x90
_String_from_f64:                       ## @String_from_f64
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movsd	%xmm0, 8(%rsp)
	movl	$32, %edi
	callq	_malloc
	movq	%rax, (%rsp)
	movsd	8(%rsp), %xmm0                  ## xmm0 = mem[0],zero
	leaq	L___unnamed_7(%rip), %rsi
	movq	%rax, %rdi
	movb	$1, %al
	callq	_sprintf
	movq	(%rsp), %rdi
	callq	_strlen
	movq	%rax, %rdx
	movq	%rax, 32(%rsp)
	movq	(%rsp), %rax
	movq	%rax, 16(%rsp)
	movl	%edx, 24(%rsp)
	movl	$32, 28(%rsp)
                                        ## kill: def $edx killed $edx killed $rdx
	movl	$32, %ecx
	addq	$40, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_len                     ## -- Begin function String_len
	.p2align	4, 0x90
_String_len:                            ## @String_len
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	movl	8(%rdi), %eax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_capacity                ## -- Begin function String_capacity
	.p2align	4, 0x90
_String_capacity:                       ## @String_capacity
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	movl	12(%rdi), %eax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_c_str                   ## -- Begin function String_c_str
	.p2align	4, 0x90
_String_c_str:                          ## @String_c_str
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	movq	(%rdi), %rax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_push_str                ## -- Begin function String_push_str
	.p2align	4, 0x90
_String_push_str:                       ## @String_push_str
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset %rbx, -24
	movq	%rdi, -16(%rbp)
	movq	%rsi, -40(%rbp)
	movq	%rsi, %rdi
	callq	_strlen
	movq	%rax, -24(%rbp)
	movq	-16(%rbp), %rcx
	addl	8(%rcx), %eax
	movl	%eax, -28(%rbp)
	movl	$0, -44(%rbp)
	cmpl	12(%rcx), %eax
	jl	LBB18_5
## %bb.1:                               ## %then
	movq	-16(%rbp), %rax
	movl	12(%rax), %eax
	movl	-24(%rbp), %ecx
	leal	1(%rcx,%rax,2), %esi
	movq	%rsp, %rax
	leaq	-16(%rax), %rbx
	movq	%rbx, %rsp
	movl	%esi, -16(%rax)
	movq	-16(%rbp), %rax
	movq	(%rax), %rdi
	callq	_realloc
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rcx
	movq	%rcx, %rsp
	movq	%rax, -16(%rdx)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	cmpq	$0, -16(%rdx)
	je	LBB18_2
## %bb.4:                               ## %else
	movq	(%rcx), %rax
	movq	-16(%rbp), %rcx
	movq	%rax, (%rcx)
	movl	(%rbx), %eax
	movq	-16(%rbp), %rcx
	movl	%eax, 12(%rcx)
LBB18_5:                                ## %ifcont16
	movq	-16(%rbp), %rax
	movl	8(%rax), %edi
	addq	(%rax), %rdi
	movq	%rsp, %rbx
	leaq	-16(%rbx), %rsp
	movq	%rdi, -16(%rbx)
	movq	-40(%rbp), %rsi
	movq	-24(%rbp), %rdx
	callq	_memcpy
	movl	-28(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	%eax, 8(%rcx)
	movq	-24(%rbp), %rax
	movq	-16(%rbx), %rcx
	movb	$0, (%rcx,%rax)
	movl	$1, %eax
	jmp	LBB18_3
LBB18_2:                                ## %then9
	xorl	%eax, %eax
LBB18_3:                                ## %then9
	leaq	-8(%rbp), %rsp
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_push_char               ## -- Begin function String_push_char
	.p2align	4, 0x90
_String_push_char:                      ## @String_push_char
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset %rbx, -24
	movq	%rdi, -24(%rbp)
	movb	%sil, -9(%rbp)
	movl	8(%rdi), %eax
	incl	%eax
	movl	%eax, -28(%rbp)
	movl	$0, -32(%rbp)
	cmpl	12(%rdi), %eax
	jl	LBB19_5
## %bb.1:                               ## %then
	movq	-24(%rbp), %rax
	movl	12(%rax), %eax
	leal	2(%rax,%rax), %esi
	movq	%rsp, %rax
	leaq	-16(%rax), %rbx
	movq	%rbx, %rsp
	movl	%esi, -16(%rax)
	movq	-24(%rbp), %rax
	movq	(%rax), %rdi
	callq	_realloc
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rcx
	movq	%rcx, %rsp
	movq	%rax, -16(%rdx)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	cmpq	$0, -16(%rdx)
	je	LBB19_2
## %bb.4:                               ## %else
	movq	(%rcx), %rax
	movq	-24(%rbp), %rcx
	movq	%rax, (%rcx)
	movl	(%rbx), %eax
	movq	-24(%rbp), %rcx
	movl	%eax, 12(%rcx)
LBB19_5:                                ## %ifcont14
	movq	-24(%rbp), %rax
	movq	(%rax), %rcx
	movl	8(%rax), %eax
	leaq	(%rcx,%rax), %rdx
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rsp
	movq	%rdx, -16(%rsi)
	movzbl	-9(%rbp), %edx
	movb	%dl, (%rcx,%rax)
	movq	-16(%rsi), %rax
	movb	$0, 1(%rax)
	movl	-28(%rbp), %eax
	movq	-24(%rbp), %rcx
	movl	%eax, 8(%rcx)
	movl	$1, %eax
	jmp	LBB19_3
LBB19_2:                                ## %then7
	xorl	%eax, %eax
LBB19_3:                                ## %then7
	leaq	-8(%rbp), %rsp
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_eq                      ## -- Begin function String_eq
	.p2align	4, 0x90
_String_eq:                             ## @String_eq
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	subq	$32, %rsp
	.cfi_offset %rbx, -32
	.cfi_offset %r14, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movl	$0, -36(%rbp)
	movl	8(%rdi), %ebx
	movq	%rsi, %rdi
	callq	_String_len
	cmpl	%eax, %ebx
	je	LBB20_3
LBB20_1:                                ## %then
	xorl	%eax, %eax
	jmp	LBB20_2
LBB20_3:                                ## %else
	movq	%rsp, %rax
	leaq	-16(%rax), %rbx
	movq	%rbx, %rsp
	movl	$0, -16(%rax)
	movq	-24(%rbp), %rdi
	callq	_String_c_str
	movq	%rsp, %rcx
	leaq	-16(%rcx), %r14
	movq	%r14, %rsp
	movq	%rax, -16(%rcx)
	movq	-32(%rbp), %rdi
	callq	_String_c_str
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rcx
	movq	%rcx, %rsp
	movq	%rax, -16(%rdx)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	.p2align	4, 0x90
LBB20_4:                                ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	movl	(%rbx), %eax
	movq	-24(%rbp), %rdx
	cmpl	8(%rdx), %eax
	jge	LBB20_7
## %bb.5:                               ## %while_loop
                                        ##   in Loop: Header=BB20_4 Depth=1
	movq	(%r14), %rax
	movzbl	(%rax), %eax
	movl	(%rbx), %edx
	addq	%rax, %rdx
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movq	%rdx, -16(%rax)
	movq	(%rcx), %rdx
	movzbl	(%rdx), %edx
	movl	(%rbx), %esi
	addq	%rdx, %rsi
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rsp
	movq	%rsi, -16(%rdx)
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rsp
	movl	$0, -16(%rsi)
	movq	-16(%rax), %rax
	cmpq	-16(%rdx), %rax
	jne	LBB20_1
## %bb.6:                               ## %else17
                                        ##   in Loop: Header=BB20_4 Depth=1
	incl	(%rbx)
	jmp	LBB20_4
LBB20_7:                                ## %while_else
	movb	$1, %al
LBB20_2:                                ## %then
	leaq	-16(%rbp), %rsp
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_clear                   ## -- Begin function String_clear
	.p2align	4, 0x90
_String_clear:                          ## @String_clear
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -16(%rsp)
	movl	$0, 8(%rdi)
	movl	$0, -4(%rsp)
	movq	-16(%rsp), %rax
	cmpq	$0, (%rax)
	je	LBB21_2
## %bb.1:                               ## %then
	movq	-16(%rsp), %rax
	movq	(%rax), %rax
	movb	$0, (%rax)
LBB21_2:                                ## %ifcont
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_unsafe_forget           ## -- Begin function String_unsafe_forget
	.p2align	4, 0x90
_String_unsafe_forget:                  ## @String_unsafe_forget
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	movq	$0, (%rdi)
	movq	-8(%rsp), %rax
	movl	$0, 8(%rax)
	movq	-8(%rsp), %rax
	movl	$0, 12(%rax)
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_is_whitespace           ## -- Begin function String_is_whitespace
	.p2align	4, 0x90
_String_is_whitespace:                  ## @String_is_whitespace
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movb	%dil, -1(%rbp)
	movl	$0, -8(%rbp)
	cmpb	$32, %dil
	je	LBB23_1
## %bb.3:                               ## %else
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	cmpb	$9, -1(%rbp)
	je	LBB23_1
## %bb.4:                               ## %else4
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	cmpb	$10, -1(%rbp)
	je	LBB23_1
## %bb.5:                               ## %else10
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	cmpb	$13, -1(%rbp)
	jne	LBB23_6
LBB23_1:                                ## %then
	movb	$1, %al
LBB23_2:                                ## %then
	movq	%rbp, %rsp
	popq	%rbp
	retq
LBB23_6:                                ## %else16
	xorl	%eax, %eax
	jmp	LBB23_2
	.cfi_endproc
                                        ## -- End function
	.globl	_String_trim                    ## -- Begin function String_trim
	.p2align	4, 0x90
_String_trim:                           ## @String_trim
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset %rbx, -24
	movq	%rdi, -32(%rbp)
	movl	$0, -12(%rbp)
	movl	8(%rdi), %eax
	decl	%eax
	movl	%eax, -16(%rbp)
	callq	_String_c_str
	movq	%rax, -24(%rbp)
	movl	$0, -36(%rbp)
	.p2align	4, 0x90
LBB24_1:                                ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	movl	-12(%rbp), %eax
	movq	-32(%rbp), %rcx
	cmpl	8(%rcx), %eax
	jge	LBB24_4
## %bb.2:                               ## %while_loop
                                        ##   in Loop: Header=BB24_1 Depth=1
	movslq	-12(%rbp), %rax
	movq	-24(%rbp), %rcx
	movzbl	(%rcx,%rax), %eax
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movb	%al, -16(%rcx)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movzbl	-16(%rcx), %edi
	callq	_String_is_whitespace
	testb	$1, %al
	je	LBB24_4
## %bb.3:                               ## %else
                                        ##   in Loop: Header=BB24_1 Depth=1
	incl	-12(%rbp)
	jmp	LBB24_1
LBB24_4:                                ## %while_after
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	.p2align	4, 0x90
LBB24_5:                                ## %while_cond5
                                        ## =>This Inner Loop Header: Depth=1
	movl	-16(%rbp), %eax
	cmpl	-12(%rbp), %eax
	jl	LBB24_8
## %bb.6:                               ## %while_loop7
                                        ##   in Loop: Header=BB24_5 Depth=1
	movslq	-16(%rbp), %rax
	movq	-24(%rbp), %rcx
	movzbl	(%rcx,%rax), %eax
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movb	%al, -16(%rcx)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movzbl	-16(%rcx), %edi
	callq	_String_is_whitespace
	testb	$1, %al
	je	LBB24_8
## %bb.7:                               ## %else14
                                        ##   in Loop: Header=BB24_5 Depth=1
	decl	-16(%rbp)
	jmp	LBB24_5
LBB24_8:                                ## %while_after19
	movl	-16(%rbp), %eax
	subl	-12(%rbp), %eax
	incl	%eax
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rbx
	movq	%rbx, %rsp
	movl	%eax, -16(%rcx)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	cmpl	$0, -16(%rcx)
	jg	LBB24_11
## %bb.9:                               ## %then24
	leaq	L___unnamed_8(%rip), %rdi
	callq	_String_from
	jmp	LBB24_10
LBB24_11:                               ## %else26
	movq	-32(%rbp), %rdi
	callq	_String_c_str
	movl	-12(%rbp), %edi
	addq	%rax, %rdi
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movq	%rdi, -16(%rax)
	movl	(%rbx), %esi
	callq	_String_from_with_len
LBB24_10:                               ## %then24
	leaq	-8(%rbp), %rsp
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_is_empty                ## -- Begin function String_is_empty
	.p2align	4, 0x90
_String_is_empty:                       ## @String_is_empty
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	cmpl	$0, 8(%rdi)
	sete	%al
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_substring               ## -- Begin function String_substring
	.p2align	4, 0x90
_String_substring:                      ## @String_substring
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset %rbx, -24
	movq	%rdi, -24(%rbp)
	movl	%esi, -36(%rbp)
	movl	%edx, -32(%rbp)
	movl	%esi, -16(%rbp)
	movl	%edx, -12(%rbp)
	movl	$0, -28(%rbp)
	testl	%esi, %esi
	jns	LBB26_2
## %bb.1:                               ## %then
	movq	-24(%rbp), %rax
	movl	8(%rax), %eax
	addl	%eax, -16(%rbp)
LBB26_2:                                ## %ifcont
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	cmpl	$0, -12(%rbp)
	jns	LBB26_4
## %bb.3:                               ## %then3
	movq	-24(%rbp), %rax
	movl	8(%rax), %eax
	addl	%eax, -12(%rbp)
LBB26_4:                                ## %ifcont8
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	cmpl	$0, -16(%rbp)
	jns	LBB26_6
## %bb.5:                               ## %then12
	movl	$0, -16(%rbp)
LBB26_6:                                ## %ifcont14
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movl	-16(%rbp), %eax
	movq	-24(%rbp), %rcx
	cmpl	8(%rcx), %eax
	jle	LBB26_8
## %bb.7:                               ## %then19
	movq	-24(%rbp), %rax
	movl	8(%rax), %eax
	movl	%eax, -16(%rbp)
LBB26_8:                                ## %ifcont23
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	cmpl	$0, -12(%rbp)
	jns	LBB26_10
## %bb.9:                               ## %then27
	movl	$0, -12(%rbp)
LBB26_10:                               ## %ifcont29
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movl	-12(%rbp), %eax
	movq	-24(%rbp), %rcx
	cmpl	8(%rcx), %eax
	jle	LBB26_12
## %bb.11:                              ## %then35
	movq	-24(%rbp), %rax
	movl	8(%rax), %eax
	movl	%eax, -12(%rbp)
LBB26_12:                               ## %ifcont39
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movl	-16(%rbp), %eax
	cmpl	-12(%rbp), %eax
	jl	LBB26_15
## %bb.13:                              ## %then42
	leaq	L___unnamed_9(%rip), %rdi
	callq	_String_from
	jmp	LBB26_14
LBB26_15:                               ## %else43
	movl	-12(%rbp), %eax
	subl	-16(%rbp), %eax
	movq	%rsp, %rbx
	leaq	-16(%rbx), %rsp
	movl	%eax, -16(%rbx)
	movq	-24(%rbp), %rdi
	callq	_String_c_str
	movl	-16(%rbp), %edi
	addq	%rax, %rdi
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movq	%rdi, -16(%rax)
	movl	-16(%rbx), %esi
	callq	_String_from_with_len
LBB26_14:                               ## %then42
	leaq	-8(%rbp), %rsp
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_trim_start              ## -- Begin function String_trim_start
	.p2align	4, 0x90
_String_trim_start:                     ## @String_trim_start
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset %rbx, -24
	movq	%rdi, -24(%rbp)
	movl	$0, -12(%rbp)
	callq	_String_c_str
	movq	%rax, -32(%rbp)
	movl	$0, -36(%rbp)
	.p2align	4, 0x90
LBB27_1:                                ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	movl	-12(%rbp), %eax
	movq	-24(%rbp), %rcx
	cmpl	8(%rcx), %eax
	jge	LBB27_4
## %bb.2:                               ## %while_loop
                                        ##   in Loop: Header=BB27_1 Depth=1
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movslq	-12(%rbp), %rax
	movq	-32(%rbp), %rcx
	movzbl	(%rcx,%rax), %edi
	callq	_String_is_whitespace
	testb	$1, %al
	je	LBB27_4
## %bb.3:                               ## %else
                                        ##   in Loop: Header=BB27_1 Depth=1
	incl	-12(%rbp)
	jmp	LBB27_1
LBB27_4:                                ## %while_after
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movl	-12(%rbp), %eax
	movq	-24(%rbp), %rcx
	cmpl	8(%rcx), %eax
	jl	LBB27_7
## %bb.5:                               ## %then6
	leaq	L___unnamed_10(%rip), %rdi
	callq	_String_from
	jmp	LBB27_6
LBB27_7:                                ## %else8
	movq	-24(%rbp), %rax
	movl	8(%rax), %eax
	subl	-12(%rbp), %eax
	movq	%rsp, %rbx
	leaq	-16(%rbx), %rsp
	movl	%eax, -16(%rbx)
	movq	-24(%rbp), %rdi
	callq	_String_c_str
	movl	-12(%rbp), %edi
	addq	%rax, %rdi
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movq	%rdi, -16(%rax)
	movl	-16(%rbx), %esi
	callq	_String_from_with_len
LBB27_6:                                ## %then6
	leaq	-8(%rbp), %rsp
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_trim_end                ## -- Begin function String_trim_end
	.p2align	4, 0x90
_String_trim_end:                       ## @String_trim_end
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -32(%rbp)
	movl	8(%rdi), %eax
	decl	%eax
	movl	%eax, -4(%rbp)
	callq	_String_c_str
	movq	%rax, -16(%rbp)
	movl	$0, -20(%rbp)
	cmpl	$0, -4(%rbp)
	js	LBB28_4
	.p2align	4, 0x90
LBB28_2:                                ## %while_loop
                                        ## =>This Inner Loop Header: Depth=1
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movslq	-4(%rbp), %rax
	movq	-16(%rbp), %rcx
	movzbl	(%rcx,%rax), %edi
	callq	_String_is_whitespace
	testb	$1, %al
	je	LBB28_4
## %bb.3:                               ## %else
                                        ##   in Loop: Header=BB28_2 Depth=1
	decl	-4(%rbp)
	cmpl	$0, -4(%rbp)
	jns	LBB28_2
LBB28_4:                                ## %while_after
	movl	-4(%rbp), %ecx
	incl	%ecx
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rax
	movq	%rax, %rsp
	movl	%ecx, -16(%rdx)
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movl	$0, -16(%rcx)
	cmpl	$0, -16(%rdx)
	jg	LBB28_7
## %bb.5:                               ## %then5
	leaq	L___unnamed_11(%rip), %rdi
	callq	_String_from
	jmp	LBB28_6
LBB28_7:                                ## %else7
	movq	-16(%rbp), %rdi
	movl	(%rax), %esi
	callq	_String_from_with_len
LBB28_6:                                ## %then5
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_split                   ## -- Begin function String_split
	.p2align	4, 0x90
_String_split:                          ## @String_split
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, %rax
	movq	%rsi, -8(%rsp)
	movl	%edx, -36(%rsp)
	movq	(%rsi), %rcx
	movq	%rcx, -32(%rsp)
	movl	8(%rsi), %esi
	movl	%esi, -24(%rsp)
	movl	$0, -20(%rsp)
	movb	%dl, -16(%rsp)
	movq	-24(%rsp), %rsi
	movq	%rsi, 8(%rdi)
	movb	%dl, 16(%rdi)
	movq	%rcx, (%rdi)
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_at                      ## -- Begin function String_at
	.p2align	4, 0x90
_String_at:                             ## @String_at
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -16(%rbp)
	movl	%esi, -4(%rbp)
	movl	$0, -20(%rbp)
	testl	%esi, %esi
	js	LBB30_1
## %bb.2:                               ## %lor.rhs
	movl	-4(%rbp), %eax
	movq	-16(%rbp), %rcx
	cmpl	8(%rcx), %eax
	setge	%al
	testb	%al, %al
	je	LBB30_6
LBB30_4:                                ## %then
	xorl	%eax, %eax
	jmp	LBB30_5
LBB30_1:
	movb	$1, %al
	testb	%al, %al
	jne	LBB30_4
LBB30_6:                                ## %else
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movq	%rax, -16(%rcx)
	movslq	-4(%rbp), %rcx
	movzbl	(%rax,%rcx), %eax
LBB30_5:                                ## %then
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_find_char               ## -- Begin function String_find_char
	.p2align	4, 0x90
_String_find_char:                      ## @String_find_char
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movb	%sil, -1(%rbp)
	movl	$0, -8(%rbp)
	movq	(%rdi), %rax
	movq	%rax, -16(%rbp)
	movl	$0, -28(%rbp)
	.p2align	4, 0x90
LBB31_1:                                ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	movl	-8(%rbp), %eax
	movq	-24(%rbp), %rcx
	cmpl	8(%rcx), %eax
	jge	LBB31_5
## %bb.2:                               ## %while_loop
                                        ##   in Loop: Header=BB31_1 Depth=1
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movslq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movzbl	(%rcx,%rax), %eax
	cmpb	-1(%rbp), %al
	je	LBB31_3
## %bb.4:                               ## %else
                                        ##   in Loop: Header=BB31_1 Depth=1
	incl	-8(%rbp)
	jmp	LBB31_1
LBB31_5:                                ## %while_else
	movl	$-1, %eax
	jmp	LBB31_6
LBB31_3:                                ## %then
	movl	-8(%rbp), %eax
LBB31_6:                                ## %while_else
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_starts_with             ## -- Begin function String_starts_with
	.p2align	4, 0x90
_String_starts_with:                    ## @String_starts_with
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	subq	$32, %rsp
	.cfi_offset %rbx, -48
	.cfi_offset %r12, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movq	%rdi, -48(%rbp)
	movq	%rsi, -40(%rbp)
	movl	$0, -52(%rbp)
	movq	%rsi, %rdi
	callq	_String_len
	movq	-48(%rbp), %rcx
	cmpl	8(%rcx), %eax
	jle	LBB32_3
LBB32_1:                                ## %then
	xorl	%eax, %eax
	jmp	LBB32_2
LBB32_3:                                ## %else
	movq	%rsp, %rax
	leaq	-16(%rax), %rbx
	movq	%rbx, %rsp
	movl	$0, -16(%rax)
	movq	-48(%rbp), %rax
	movq	(%rax), %rax
	movq	%rsp, %rcx
	leaq	-16(%rcx), %r14
	movq	%r14, %rsp
	movq	%rax, -16(%rcx)
	movq	-40(%rbp), %rdi
	callq	_String_c_str
	movq	%rsp, %rcx
	leaq	-16(%rcx), %r15
	movq	%r15, %rsp
	movq	%rax, -16(%rcx)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	.p2align	4, 0x90
LBB32_4:                                ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	movl	(%rbx), %r12d
	movq	-40(%rbp), %rdi
	callq	_String_len
	cmpl	%eax, %r12d
	jge	LBB32_7
## %bb.5:                               ## %while_loop
                                        ##   in Loop: Header=BB32_4 Depth=1
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movslq	(%rbx), %rax
	movq	(%r14), %rcx
	movzbl	(%rcx,%rax), %ecx
	movq	(%r15), %rdx
	cmpb	(%rdx,%rax), %cl
	jne	LBB32_1
## %bb.6:                               ## %else10
                                        ##   in Loop: Header=BB32_4 Depth=1
	incl	(%rbx)
	jmp	LBB32_4
LBB32_7:                                ## %while_else
	movb	$1, %al
LBB32_2:                                ## %then
	leaq	-32(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_ends_with               ## -- Begin function String_ends_with
	.p2align	4, 0x90
_String_ends_with:                      ## @String_ends_with
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movq	%rdi, -56(%rbp)
	movq	%rsi, -48(%rbp)
	movl	$0, -60(%rbp)
	movq	%rsi, %rdi
	callq	_String_len
	movq	-56(%rbp), %rcx
	cmpl	8(%rcx), %eax
	jle	LBB33_3
LBB33_1:                                ## %then
	xorl	%eax, %eax
	jmp	LBB33_2
LBB33_3:                                ## %else
	movq	%rsp, %rax
	leaq	-16(%rax), %rbx
	movq	%rbx, %rsp
	movl	$0, -16(%rax)
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	movq	%rsp, %rcx
	leaq	-16(%rcx), %r14
	movq	%r14, %rsp
	movq	%rax, -16(%rcx)
	movq	-48(%rbp), %rdi
	callq	_String_c_str
	movq	%rsp, %rcx
	leaq	-16(%rcx), %r15
	movq	%r15, %rsp
	movq	%rax, -16(%rcx)
	movq	-56(%rbp), %rax
	movl	8(%rax), %r13d
	movq	-48(%rbp), %rdi
	callq	_String_len
	subl	%eax, %r13d
	movq	%rsp, %rax
	leaq	-16(%rax), %r12
	movq	%r12, %rsp
	movl	%r13d, -16(%rax)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	.p2align	4, 0x90
LBB33_4:                                ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	movl	(%rbx), %r13d
	movq	-48(%rbp), %rdi
	callq	_String_len
	cmpl	%eax, %r13d
	jge	LBB33_7
## %bb.5:                               ## %while_loop
                                        ##   in Loop: Header=BB33_4 Depth=1
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movslq	(%rbx), %rax
	movl	(%r12), %ecx
	addl	%eax, %ecx
	movq	(%r14), %rdx
	movslq	%ecx, %rcx
	movzbl	(%rdx,%rcx), %ecx
	movq	(%r15), %rdx
	cmpb	(%rdx,%rax), %cl
	jne	LBB33_1
## %bb.6:                               ## %else14
                                        ##   in Loop: Header=BB33_4 Depth=1
	incl	(%rbx)
	jmp	LBB33_4
LBB33_7:                                ## %while_else
	movb	$1, %al
LBB33_2:                                ## %then
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_contains                ## -- Begin function String_contains
	.p2align	4, 0x90
_String_contains:                       ## @String_contains
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset %rbx, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -16(%rbp)
	movl	$0, -28(%rbp)
	movq	%rsi, %rdi
	callq	_String_len
	testl	%eax, %eax
	je	LBB34_1
## %bb.3:                               ## %else
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movq	-16(%rbp), %rdi
	callq	_String_len
	movq	-24(%rbp), %rcx
	cmpl	8(%rcx), %eax
	jle	LBB34_5
## %bb.4:                               ## %then6
	xorl	%eax, %eax
	jmp	LBB34_2
LBB34_1:                                ## %then
	movb	$1, %al
	jmp	LBB34_2
LBB34_5:                                ## %else7
	movq	-24(%rbp), %rdi
	callq	_String_c_str
	movq	%rax, %rbx
	movq	-16(%rbp), %rdi
	callq	_String_c_str
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_strstr
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movq	%rax, -16(%rcx)
	testq	%rax, %rax
	setne	%al
LBB34_2:                                ## %then
	leaq	-8(%rbp), %rsp
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_to_upper                ## -- Begin function String_to_upper
	.p2align	4, 0x90
_String_to_upper:                       ## @String_to_upper
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -40(%rbp)
	callq	_String_c_str
	movq	-40(%rbp), %rcx
	movl	8(%rcx), %esi
	movq	%rax, %rdi
	callq	_String_from_with_len
	movq	%rax, -24(%rbp)
	movl	%edx, -16(%rbp)
	movl	%ecx, -12(%rbp)
	movl	$0, -4(%rbp)
	leaq	-24(%rbp), %rdi
	callq	_String_c_str
	movq	%rax, -32(%rbp)
	movl	$0, -44(%rbp)
	jmp	LBB35_1
	.p2align	4, 0x90
LBB35_7:                                ## %ifcont
                                        ##   in Loop: Header=BB35_1 Depth=1
	incl	-4(%rbp)
LBB35_1:                                ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	movl	-4(%rbp), %eax
	cmpl	-16(%rbp), %eax
	jge	LBB35_8
## %bb.2:                               ## %while_loop
                                        ##   in Loop: Header=BB35_1 Depth=1
	movslq	-4(%rbp), %rax
	movq	-32(%rbp), %rcx
	movzbl	(%rcx,%rax), %ecx
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rax
	movq	%rax, %rsp
	movb	%cl, -16(%rdx)
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movl	$0, -16(%rcx)
	cmpb	$97, -16(%rdx)
	jl	LBB35_3
## %bb.4:                               ## %land.rhs
                                        ##   in Loop: Header=BB35_1 Depth=1
	cmpb	$123, (%rax)
	setl	%cl
	testb	%cl, %cl
	je	LBB35_7
	jmp	LBB35_6
	.p2align	4, 0x90
LBB35_3:                                ##   in Loop: Header=BB35_1 Depth=1
	xorl	%ecx, %ecx
	testb	%cl, %cl
	je	LBB35_7
LBB35_6:                                ## %then
                                        ##   in Loop: Header=BB35_1 Depth=1
	movzbl	(%rax), %eax
	addb	$-32, %al
	movslq	-4(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	jmp	LBB35_7
LBB35_8:                                ## %while_else
	movq	-24(%rbp), %rax
	movl	-16(%rbp), %edx
	movl	-12(%rbp), %ecx
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_to_lower                ## -- Begin function String_to_lower
	.p2align	4, 0x90
_String_to_lower:                       ## @String_to_lower
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -40(%rbp)
	callq	_String_c_str
	movq	-40(%rbp), %rcx
	movl	8(%rcx), %esi
	movq	%rax, %rdi
	callq	_String_from_with_len
	movq	%rax, -24(%rbp)
	movl	%edx, -16(%rbp)
	movl	%ecx, -12(%rbp)
	movl	$0, -4(%rbp)
	leaq	-24(%rbp), %rdi
	callq	_String_c_str
	movq	%rax, -32(%rbp)
	movl	$0, -44(%rbp)
	jmp	LBB36_1
	.p2align	4, 0x90
LBB36_7:                                ## %ifcont
                                        ##   in Loop: Header=BB36_1 Depth=1
	incl	-4(%rbp)
LBB36_1:                                ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	movl	-4(%rbp), %eax
	cmpl	-16(%rbp), %eax
	jge	LBB36_8
## %bb.2:                               ## %while_loop
                                        ##   in Loop: Header=BB36_1 Depth=1
	movslq	-4(%rbp), %rax
	movq	-32(%rbp), %rcx
	movzbl	(%rcx,%rax), %ecx
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rax
	movq	%rax, %rsp
	movb	%cl, -16(%rdx)
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movl	$0, -16(%rcx)
	cmpb	$65, -16(%rdx)
	jl	LBB36_3
## %bb.4:                               ## %land.rhs
                                        ##   in Loop: Header=BB36_1 Depth=1
	cmpb	$91, (%rax)
	setl	%cl
	testb	%cl, %cl
	je	LBB36_7
	jmp	LBB36_6
	.p2align	4, 0x90
LBB36_3:                                ##   in Loop: Header=BB36_1 Depth=1
	xorl	%ecx, %ecx
	testb	%cl, %cl
	je	LBB36_7
LBB36_6:                                ## %then
                                        ##   in Loop: Header=BB36_1 Depth=1
	movzbl	(%rax), %eax
	addb	$32, %al
	movslq	-4(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	jmp	LBB36_7
LBB36_8:                                ## %while_else
	movq	-24(%rbp), %rax
	movl	-16(%rbp), %edx
	movl	-12(%rbp), %ecx
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_replace                 ## -- Begin function String_replace
	.p2align	4, 0x90
_String_replace:                        ## @String_replace
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -40(%rbp)
	movb	%sil, -2(%rbp)
	movb	%dl, -1(%rbp)
	callq	_String_c_str
	movq	-40(%rbp), %rcx
	movl	8(%rcx), %esi
	movq	%rax, %rdi
	callq	_String_from_with_len
	movq	%rax, -24(%rbp)
	movl	%edx, -16(%rbp)
	movl	%ecx, -12(%rbp)
	movl	$0, -8(%rbp)
	leaq	-24(%rbp), %rdi
	callq	_String_c_str
	movq	%rax, -32(%rbp)
	movl	$0, -44(%rbp)
	jmp	LBB37_1
	.p2align	4, 0x90
LBB37_4:                                ## %ifcont
                                        ##   in Loop: Header=BB37_1 Depth=1
	incl	-8(%rbp)
LBB37_1:                                ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	movl	-8(%rbp), %eax
	cmpl	-16(%rbp), %eax
	jge	LBB37_5
## %bb.2:                               ## %while_loop
                                        ##   in Loop: Header=BB37_1 Depth=1
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movslq	-8(%rbp), %rax
	movq	-32(%rbp), %rcx
	movzbl	(%rcx,%rax), %eax
	cmpb	-2(%rbp), %al
	jne	LBB37_4
## %bb.3:                               ## %then
                                        ##   in Loop: Header=BB37_1 Depth=1
	movzbl	-1(%rbp), %eax
	movslq	-8(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movb	%al, (%rdx,%rcx)
	jmp	LBB37_4
LBB37_5:                                ## %while_else
	movq	-24(%rbp), %rax
	movl	-16(%rbp), %edx
	movl	-12(%rbp), %ecx
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_find_str                ## -- Begin function String_find_str
	.p2align	4, 0x90
_String_find_str:                       ## @String_find_str
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset %rbx, -24
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
	movl	$0, -28(%rbp)
	movq	%rsi, %rdi
	callq	_String_len
	testl	%eax, %eax
	je	LBB38_1
## %bb.3:                               ## %else
	movq	-16(%rbp), %rdi
	callq	_String_c_str
	movq	%rax, %rbx
	movq	-24(%rbp), %rdi
	callq	_String_c_str
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_strstr
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rcx
	movq	%rcx, %rsp
	movq	%rax, -16(%rdx)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	cmpq	$0, -16(%rdx)
	je	LBB38_4
## %bb.5:                               ## %else9
	movq	(%rcx), %rbx
	movq	-16(%rbp), %rdi
	callq	_String_c_str
	subq	%rax, %rbx
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movq	%rbx, -16(%rax)
	movl	%ebx, %eax
	jmp	LBB38_2
LBB38_1:                                ## %then
	xorl	%eax, %eax
	jmp	LBB38_2
LBB38_4:                                ## %then8
	movl	$-1, %eax
LBB38_2:                                ## %then
	leaq	-8(%rbp), %rsp
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_replace_str             ## -- Begin function String_replace_str
	.p2align	4, 0x90
_String_replace_str:                    ## @String_replace_str
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$56, %rsp
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movq	%rdi, -56(%rbp)
	movq	%rsi, -48(%rbp)
	movq	%rdx, -80(%rbp)
	movl	$0, -84(%rbp)
	movq	%rsi, %rdi
	callq	_String_len
	testl	%eax, %eax
	je	LBB39_1
## %bb.2:                               ## %else
	leaq	L___unnamed_12(%rip), %rdi
	callq	_String_from
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rdi
	movq	%rdi, -64(%rbp)                 ## 8-byte Spill
	movq	%rdi, %rsp
	movl	%ecx, -4(%rsi)
	movl	%edx, -8(%rsi)
	movq	%rax, -16(%rsi)
	movq	%rsp, %rax
	leaq	-16(%rax), %r14
	movq	%r14, %rsp
	movl	$0, -16(%rax)
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	movq	%rsp, %rcx
	leaq	-16(%rcx), %r15
	movq	%r15, %rsp
	movq	%rax, -16(%rcx)
	movq	-48(%rbp), %rdi
	callq	_String_c_str
	movq	%rsp, %rcx
	leaq	-16(%rcx), %r12
	movq	%r12, %rsp
	movq	%rax, -16(%rcx)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	jmp	LBB39_3
	.p2align	4, 0x90
LBB39_13:                               ## %else38
                                        ##   in Loop: Header=BB39_3 Depth=1
	movslq	(%r14), %rax
	movq	(%r15), %rcx
	movzbl	(%rcx,%rax), %esi
	movq	-64(%rbp), %rdi                 ## 8-byte Reload
	callq	_String_push_char
	incl	(%r14)
LBB39_3:                                ## %while_cond
                                        ## =>This Loop Header: Depth=1
                                        ##     Child Loop BB39_7 Depth 2
	movl	(%r14), %eax
	movq	-56(%rbp), %rcx
	cmpl	8(%rcx), %eax
	jge	LBB39_14
## %bb.4:                               ## %while_loop
                                        ##   in Loop: Header=BB39_3 Depth=1
	movq	%rsp, %rax
	leaq	-16(%rax), %r13
	movq	%r13, %rsp
	movb	$1, -16(%rax)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movl	(%r14), %ebx
	movq	-48(%rbp), %rdi
	callq	_String_len
	addl	%ebx, %eax
	movq	-56(%rbp), %rcx
	cmpl	8(%rcx), %eax
	jle	LBB39_6
## %bb.5:                               ## %then13
                                        ##   in Loop: Header=BB39_3 Depth=1
	movb	$0, (%r13)
	jmp	LBB39_11
	.p2align	4, 0x90
LBB39_6:                                ## %else14
                                        ##   in Loop: Header=BB39_3 Depth=1
	movq	%r13, -72(%rbp)                 ## 8-byte Spill
	movq	%rsp, %rax
	leaq	-16(%rax), %rbx
	movq	%rbx, %rsp
	movl	$0, -16(%rax)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	.p2align	4, 0x90
LBB39_7:                                ## %while_cond15
                                        ##   Parent Loop BB39_3 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movl	(%rbx), %r13d
	movq	-48(%rbp), %rdi
	callq	_String_len
	cmpl	%eax, %r13d
	jge	LBB39_10
## %bb.8:                               ## %while_loop20
                                        ##   in Loop: Header=BB39_7 Depth=2
	movl	(%r14), %eax
	addl	(%rbx), %eax
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movl	%eax, -16(%rcx)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movslq	-16(%rcx), %rax
	movq	(%r15), %rcx
	movzbl	(%rcx,%rax), %eax
	movslq	(%rbx), %rcx
	movq	(%r12), %rdx
	cmpb	(%rdx,%rcx), %al
	jne	LBB39_9
## %bb.16:                              ## %else25
                                        ##   in Loop: Header=BB39_7 Depth=2
	incl	(%rbx)
	jmp	LBB39_7
	.p2align	4, 0x90
LBB39_9:                                ## %then24
                                        ##   in Loop: Header=BB39_3 Depth=1
	movq	-72(%rbp), %rax                 ## 8-byte Reload
	movb	$0, (%rax)
LBB39_10:                               ## %while_after
                                        ##   in Loop: Header=BB39_3 Depth=1
	movq	-72(%rbp), %r13                 ## 8-byte Reload
LBB39_11:                               ## %ifcont29
                                        ##   in Loop: Header=BB39_3 Depth=1
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	cmpb	$1, (%r13)
	jne	LBB39_13
## %bb.12:                              ## %then32
                                        ##   in Loop: Header=BB39_3 Depth=1
	movq	-80(%rbp), %rdi
	callq	_String_c_str
	movq	-64(%rbp), %rdi                 ## 8-byte Reload
	movq	%rax, %rsi
	callq	_String_push_str
	movl	(%r14), %ebx
	movq	-48(%rbp), %rdi
	callq	_String_len
	addl	%ebx, %eax
	movl	%eax, (%r14)
	jmp	LBB39_3
LBB39_14:                               ## %while_else43
	movq	-64(%rbp), %rcx                 ## 8-byte Reload
	movq	(%rcx), %rax
	movl	8(%rcx), %edx
	movl	12(%rcx), %ecx
	jmp	LBB39_15
LBB39_1:                                ## %then
	movq	-56(%rbp), %rdi
	callq	_encap_String_clone
LBB39_15:                               ## %while_else43
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_repeat                  ## -- Begin function String_repeat
	.p2align	4, 0x90
_String_repeat:                         ## @String_repeat
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	subq	$32, %rsp
	.cfi_offset %rbx, -32
	.cfi_offset %r14, -24
	movq	%rdi, -32(%rbp)
	movl	%esi, -20(%rbp)
	movl	$0, -36(%rbp)
	testl	%esi, %esi
	jg	LBB40_2
## %bb.1:                               ## %then
	leaq	L___unnamed_13(%rip), %rdi
	callq	_String_from
	jmp	LBB40_6
LBB40_2:                                ## %else
	leaq	L___unnamed_14(%rip), %rdi
	callq	_String_from
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rbx
	movq	%rbx, %rsp
	movl	%ecx, -4(%rsi)
	movl	%edx, -8(%rsi)
	movq	%rax, -16(%rsi)
	movq	%rsp, %rax
	leaq	-16(%rax), %r14
	movq	%r14, %rsp
	movl	$0, -16(%rax)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	.p2align	4, 0x90
LBB40_3:                                ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	movl	(%r14), %eax
	cmpl	-20(%rbp), %eax
	jge	LBB40_5
## %bb.4:                               ## %while_loop
                                        ##   in Loop: Header=BB40_3 Depth=1
	movq	-32(%rbp), %rdi
	callq	_String_c_str
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_String_push_str
	incl	(%r14)
	jmp	LBB40_3
LBB40_5:                                ## %while_else
	movq	(%rbx), %rax
	movl	8(%rbx), %edx
	movl	12(%rbx), %ecx
LBB40_6:                                ## %while_else
	leaq	-16(%rbp), %rsp
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_String_reverse                 ## -- Begin function String_reverse
	.p2align	4, 0x90
_String_reverse:                        ## @String_reverse
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	subq	$16, %rsp
	.cfi_offset %rbx, -32
	.cfi_offset %r14, -24
	movq	%rdi, -24(%rbp)
	movl	$0, -28(%rbp)
	cmpl	$1, 8(%rdi)
	jg	LBB41_2
## %bb.1:                               ## %then
	movq	-24(%rbp), %rdi
	callq	_encap_String_clone
	jmp	LBB41_6
LBB41_2:                                ## %else
	movq	-24(%rbp), %rdi
	callq	_String_c_str
	movq	-24(%rbp), %rcx
	movl	8(%rcx), %esi
	movq	%rax, %rdi
	callq	_String_from_with_len
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rbx
	movq	%rbx, %rsp
	movl	%ecx, -4(%rsi)
	movl	%edx, -8(%rsi)
	movq	%rax, -16(%rsi)
	movq	%rsp, %rax
	leaq	-16(%rax), %r14
	movq	%r14, %rsp
	movl	$0, -16(%rax)
	movq	%rbx, %rdi
	callq	_String_c_str
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rcx
	movq	%rcx, %rsp
	movq	%rax, -16(%rdx)
	movq	-24(%rbp), %rax
	movq	(%rax), %rdx
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rax
	movq	%rax, %rsp
	movq	%rdx, -16(%rsi)
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rsp
	movl	$0, -16(%rdx)
	.p2align	4, 0x90
LBB41_3:                                ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	movl	(%r14), %edx
	movq	-24(%rbp), %rsi
	cmpl	8(%rsi), %edx
	jge	LBB41_5
## %bb.4:                               ## %while_loop
                                        ##   in Loop: Header=BB41_3 Depth=1
	movq	-24(%rbp), %rdx
	movslq	(%r14), %rsi
	movl	%esi, %edi
	notl	%edi
	addl	8(%rdx), %edi
	movq	(%rax), %rdx
	movslq	%edi, %rdi
	movzbl	(%rdx,%rdi), %edx
	movq	(%rcx), %rdi
	movb	%dl, (%rdi,%rsi)
	incl	(%r14)
	jmp	LBB41_3
LBB41_5:                                ## %while_else
	movq	(%rbx), %rax
	movl	8(%rbx), %edx
	movl	12(%rbx), %ecx
LBB41_6:                                ## %while_else
	leaq	-16(%rbp), %rsp
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_Hash_String_hash               ## -- Begin function Hash_String_hash
	.p2align	4, 0x90
_Hash_String_hash:                      ## @Hash_String_hash
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -32(%rbp)
	movq	$5381, -16(%rbp)                ## imm = 0x1505
	movl	$0, -4(%rbp)
	movq	(%rdi), %rax
	movq	%rax, -24(%rbp)
	movl	$0, -36(%rbp)
	.p2align	4, 0x90
LBB42_1:                                ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	movl	-4(%rbp), %eax
	movq	-32(%rbp), %rcx
	cmpl	8(%rcx), %eax
	jge	LBB42_3
## %bb.2:                               ## %while_loop
                                        ##   in Loop: Header=BB42_1 Depth=1
	movslq	-4(%rbp), %rax
	movq	-24(%rbp), %rcx
	movzbl	(%rcx,%rax), %eax
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movq	%rax, -16(%rcx)
	movq	-16(%rbp), %rcx
	movq	%rcx, %rdx
	shlq	$5, %rdx
	addq	%rcx, %rdx
	addq	%rax, %rdx
	movq	%rdx, -16(%rbp)
	incl	-4(%rbp)
	jmp	LBB42_1
LBB42_3:                                ## %while_else
	movq	-16(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_args_impl                      ## -- Begin function args_impl
	.p2align	4, 0x90
_args_impl:                             ## @args_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	__NSGetArgc
	movq	%rax, (%rsp)
	movl	(%rax), %eax
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_get_arg_impl                   ## -- Begin function get_arg_impl
	.p2align	4, 0x90
_get_arg_impl:                          ## @get_arg_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$72, %rsp
	.cfi_def_cfa_offset 80
	movl	%edi, 4(%rsp)
	callq	__NSGetArgv
	movq	%rax, 64(%rsp)
	movq	(%rax), %rax
	movq	%rax, 56(%rsp)
	movq	%rax, 48(%rsp)
	movq	(%rax), %rax
	movq	%rax, 40(%rsp)
	movl	4(%rsp), %ecx
	leaq	(%rax,%rcx,8), %rdx
	movq	%rdx, 32(%rsp)
	movq	%rdx, 24(%rsp)
	movq	(%rax,%rcx,8), %rdi
	movq	%rdi, 16(%rsp)
	movq	%rdi, 8(%rsp)
	callq	_String_from
	addq	$72, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_opendir_impl                   ## -- Begin function opendir_impl
	.p2align	4, 0x90
_opendir_impl:                          ## @opendir_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	callq	_opendir
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_readdir_impl                   ## -- Begin function readdir_impl
	.p2align	4, 0x90
_readdir_impl:                          ## @readdir_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	callq	_readdir
	movq	%rax, -8(%rbp)
	movl	$0, -12(%rbp)
	cmpq	_ADDR0(%rip), %rax
	jne	LBB46_3
## %bb.1:                               ## %then
	leaq	L___unnamed_15(%rip), %rdi
	callq	_String_from
	jmp	LBB46_2
LBB46_3:                                ## %else
	movq	-8(%rbp), %rax
	leaq	21(%rax), %rcx
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rsp
	movq	%rcx, -16(%rdx)
	movzbl	21(%rax), %edi
	callq	_String_from
LBB46_2:                                ## %then
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_closedir_impl                  ## -- Begin function closedir_impl
	.p2align	4, 0x90
_closedir_impl:                         ## @closedir_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	callq	_closedir
	popq	%rax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_mkdir_impl                     ## -- Begin function mkdir_impl
	.p2align	4, 0x90
_mkdir_impl:                            ## @mkdir_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	movl	$493, %esi                      ## imm = 0x1ED
	callq	_mkdir
	testl	%eax, %eax
	sete	%al
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_rmdir_impl                     ## -- Begin function rmdir_impl
	.p2align	4, 0x90
_rmdir_impl:                            ## @rmdir_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	callq	_rmdir
	testl	%eax, %eax
	sete	%al
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_path_exists                    ## -- Begin function path_exists
	.p2align	4, 0x90
_path_exists:                           ## @path_exists
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	xorl	%esi, %esi
	callq	_access
	testl	%eax, %eax
	sete	%al
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_stat_impl                      ## -- Begin function stat_impl
	.p2align	4, 0x90
_stat_impl:                             ## @stat_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -16(%rbp)
	movl	$144, %edi
	callq	_malloc
	movq	%rax, -8(%rbp)
	movl	$0, -20(%rbp)
	testq	%rax, %rax
	je	LBB51_1
## %bb.3:                               ## %else
	movq	-16(%rbp), %rdi
	movq	-8(%rbp), %rsi
	callq	_stat
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movl	%eax, -16(%rcx)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	cmpl	$0, -16(%rcx)
	je	LBB51_5
## %bb.4:                               ## %then4
	movq	-8(%rbp), %rdi
	callq	_free
LBB51_1:                                ## %then
	movq	_ADDR0(%rip), %rax
	jmp	LBB51_2
LBB51_5:                                ## %else6
	movq	-8(%rbp), %rax
LBB51_2:                                ## %then
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_stat_free_impl                 ## -- Begin function stat_free_impl
	.p2align	4, 0x90
_stat_free_impl:                        ## @stat_free_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	callq	_free
	popq	%rax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_stat_get_mode_impl             ## -- Begin function stat_get_mode_impl
	.p2align	4, 0x90
_stat_get_mode_impl:                    ## @stat_get_mode_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	leaq	8(%rdi), %rax
	movq	%rax, -16(%rsp)
	movq	%rax, -24(%rsp)
	movzwl	8(%rdi), %eax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_stat_get_size_impl             ## -- Begin function stat_get_size_impl
	.p2align	4, 0x90
_stat_get_size_impl:                    ## @stat_get_size_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	leaq	72(%rdi), %rax
	movq	%rax, -16(%rsp)
	movq	%rax, -24(%rsp)
	movq	72(%rdi), %rax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_stat_get_mtime_impl            ## -- Begin function stat_get_mtime_impl
	.p2align	4, 0x90
_stat_get_mtime_impl:                   ## @stat_get_mtime_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	leaq	40(%rdi), %rax
	movq	%rax, -16(%rsp)
	movq	%rax, -24(%rsp)
	movq	40(%rdi), %rax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_stat_is_dir_impl               ## -- Begin function stat_is_dir_impl
	.p2align	4, 0x90
_stat_is_dir_impl:                      ## @stat_is_dir_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	movl	%edi, -4(%rsp)
	andl	$61440, %edi                    ## imm = 0xF000
	cmpl	$16384, %edi                    ## imm = 0x4000
	sete	%al
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_stat_is_file_impl              ## -- Begin function stat_is_file_impl
	.p2align	4, 0x90
_stat_is_file_impl:                     ## @stat_is_file_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	movl	%edi, -4(%rsp)
	andl	$61440, %edi                    ## imm = 0xF000
	cmpl	$32768, %edi                    ## imm = 0x8000
	sete	%al
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_getcwd_impl                    ## -- Begin function getcwd_impl
	.p2align	4, 0x90
_getcwd_impl:                           ## @getcwd_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movl	$1024, %edi                     ## imm = 0x400
	callq	_malloc
	movq	%rax, (%rsp)
	movl	$1024, %esi                     ## imm = 0x400
	movq	%rax, %rdi
	callq	_getcwd
	movq	%rax, 16(%rsp)
	movl	$0, 12(%rsp)
	cmpq	_ADDR0(%rip), %rax
	jne	LBB58_2
## %bb.1:                               ## %then
	movq	(%rsp), %rdi
	callq	_free
	leaq	L___unnamed_16(%rip), %rdi
	callq	_String_from
	addq	$24, %rsp
	retq
LBB58_2:                                ## %else
	movq	(%rsp), %rdi
	callq	_String_from_take
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_chdir_impl                     ## -- Begin function chdir_impl
	.p2align	4, 0x90
_chdir_impl:                            ## @chdir_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	callq	_chdir
	testl	%eax, %eax
	sete	%al
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_getenv_impl                    ## -- Begin function getenv_impl
	.p2align	4, 0x90
_getenv_impl:                           ## @getenv_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 16(%rsp)
	callq	_String_c_str
	movq	%rax, %rdi
	callq	_getenv
	movq	%rax, (%rsp)
	movl	$0, 12(%rsp)
	testq	%rax, %rax
	je	LBB60_1
## %bb.3:                               ## %else
	movq	(%rsp), %rdi
	jmp	LBB60_2
LBB60_1:                                ## %then
	leaq	L___unnamed_17(%rip), %rdi
LBB60_2:                                ## %then
	callq	_String_from
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_setenv_impl                    ## -- Begin function setenv_impl
	.p2align	4, 0x90
_setenv_impl:                           ## @setenv_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -16
	movq	%rdi, 8(%rsp)
	movq	%rsi, (%rsp)
	callq	_String_c_str
	movq	%rax, %rbx
	movq	(%rsp), %rdi
	callq	_String_c_str
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movl	$1, %edx
	callq	_setenv
	testl	%eax, %eax
	sete	%al
	addq	$16, %rsp
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_unsetenv_impl                  ## -- Begin function unsetenv_impl
	.p2align	4, 0x90
_unsetenv_impl:                         ## @unsetenv_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	callq	_String_c_str
	movq	%rax, %rdi
	callq	_unsetenv
	testl	%eax, %eax
	sete	%al
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_get_last_error                 ## -- Begin function get_last_error
	.p2align	4, 0x90
_get_last_error:                        ## @get_last_error
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	___error
	movq	%rax, (%rsp)
	movl	(%rax), %eax
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_net_socket_impl                ## -- Begin function net_socket_impl
	.p2align	4, 0x90
_net_socket_impl:                       ## @net_socket_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$2, %edi
	movl	$1, %esi
	xorl	%edx, %edx
	callq	_socket
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_net_setsockopt_reuse_impl      ## -- Begin function net_setsockopt_reuse_impl
	.p2align	4, 0x90
_net_setsockopt_reuse_impl:             ## @net_setsockopt_reuse_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movl	%edi, 12(%rsp)
	movl	$4, %edi
	callq	_malloc
	movq	%rax, 16(%rsp)
	movl	$1, (%rax)
	movl	12(%rsp), %edi
	movq	16(%rsp), %rcx
	movl	$65535, %esi                    ## imm = 0xFFFF
	movl	$4, %edx
	movl	$4, %r8d
	callq	_setsockopt
	movl	%eax, 8(%rsp)
	movq	16(%rsp), %rdi
	callq	_free
	movl	8(%rsp), %eax
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_fill_sockaddr_in               ## -- Begin function fill_sockaddr_in
	.p2align	4, 0x90
_fill_sockaddr_in:                      ## @fill_sockaddr_in
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$80, %rsp
	movq	%rdi, -72(%rbp)
	movq	%rsi, -24(%rbp)
	movl	%edx, -12(%rbp)
	movq	%rdi, -8(%rbp)
	movb	$16, (%rdi)
	movq	-8(%rbp), %rax
	leaq	1(%rax), %rcx
	movq	%rcx, -64(%rbp)
	movb	$2, 1(%rax)
	movl	-12(%rbp), %edi
	callq	_htons
	movw	%ax, -14(%rbp)
	movq	-8(%rbp), %rcx
	leaq	2(%rcx), %rdx
	movq	%rdx, -56(%rbp)
	movw	%ax, 2(%rcx)
	movq	-8(%rbp), %rdx
	addq	$4, %rdx
	movq	%rdx, -48(%rbp)
	movq	-24(%rbp), %rsi
	movq	%rsi, -40(%rbp)
	movl	$2, %edi
	callq	_inet_pton
	movl	%eax, -32(%rbp)
	movl	$0, -28(%rbp)
	cmpl	$1, %eax
	je	LBB66_3
## %bb.1:                               ## %then
	xorl	%eax, %eax
	jmp	LBB66_2
LBB66_3:                                ## %else
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rax
	movq	%rax, %rsp
	movl	$8, -16(%rcx)
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movl	$0, -16(%rcx)
	cmpl	$15, (%rax)
	jg	LBB66_6
	.p2align	4, 0x90
LBB66_5:                                ## %while_loop
                                        ## =>This Inner Loop Header: Depth=1
	movq	-8(%rbp), %rcx
	movslq	(%rax), %rdx
	leaq	(%rcx,%rdx), %rsi
	movq	%rsp, %rdi
	leaq	-16(%rdi), %rsp
	movq	%rsi, -16(%rdi)
	movb	$0, (%rcx,%rdx)
	incl	(%rax)
	cmpl	$15, (%rax)
	jle	LBB66_5
LBB66_6:                                ## %while_else
	movb	$1, %al
LBB66_2:                                ## %then
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_net_bind_impl                  ## -- Begin function net_bind_impl
	.p2align	4, 0x90
_net_bind_impl:                         ## @net_bind_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset %rbx, -24
	movl	%edi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movl	%edx, -20(%rbp)
	movl	$32, %edi
	callq	_malloc
	movq	%rax, -16(%rbp)
	movl	$0, -36(%rbp)
	testq	%rax, %rax
	je	LBB67_1
## %bb.3:                               ## %else
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movq	-16(%rbp), %rdi
	movq	-32(%rbp), %rsi
	movl	-20(%rbp), %edx
	callq	_fill_sockaddr_in
	testb	$1, %al
	jne	LBB67_5
## %bb.4:                               ## %then3
	movq	-16(%rbp), %rdi
	callq	_free
LBB67_1:                                ## %then
	movl	$-1, %eax
	jmp	LBB67_2
LBB67_5:                                ## %else5
	movl	-24(%rbp), %edi
	movq	-16(%rbp), %rsi
	movl	$16, %edx
	callq	_bind
	movq	%rsp, %rbx
	leaq	-16(%rbx), %rsp
	movl	%eax, -16(%rbx)
	movq	-16(%rbp), %rdi
	callq	_free
	movl	-16(%rbx), %eax
LBB67_2:                                ## %then
	leaq	-8(%rbp), %rsp
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_net_listen_impl                ## -- Begin function net_listen_impl
	.p2align	4, 0x90
_net_listen_impl:                       ## @net_listen_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	%edi, 4(%rsp)
	movl	$128, %esi
	callq	_listen
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_net_connect_impl               ## -- Begin function net_connect_impl
	.p2align	4, 0x90
_net_connect_impl:                      ## @net_connect_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset %rbx, -24
	movl	%edi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movl	%edx, -20(%rbp)
	movl	$32, %edi
	callq	_malloc
	movq	%rax, -16(%rbp)
	movl	$0, -36(%rbp)
	testq	%rax, %rax
	je	LBB69_1
## %bb.3:                               ## %else
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movq	-16(%rbp), %rdi
	movq	-32(%rbp), %rsi
	movl	-20(%rbp), %edx
	callq	_fill_sockaddr_in
	testb	$1, %al
	jne	LBB69_5
## %bb.4:                               ## %then3
	movq	-16(%rbp), %rdi
	callq	_free
	movl	$-3, %eax
	jmp	LBB69_2
LBB69_1:                                ## %then
	movl	$-2, %eax
	jmp	LBB69_2
LBB69_5:                                ## %else5
	movl	-24(%rbp), %edi
	movq	-16(%rbp), %rsi
	movl	$16, %edx
	callq	_connect
	movq	%rsp, %rbx
	leaq	-16(%rbx), %rsp
	movl	%eax, -16(%rbx)
	movq	-16(%rbp), %rdi
	callq	_free
	movl	-16(%rbx), %eax
LBB69_2:                                ## %then
	leaq	-8(%rbp), %rsp
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_net_accept_impl                ## -- Begin function net_accept_impl
	.p2align	4, 0x90
_net_accept_impl:                       ## @net_accept_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movl	%edi, 4(%rsp)
	movl	$4, %edi
	callq	_malloc
	movq	%rax, 8(%rsp)
	movl	$16, (%rax)
	movl	$16, %edi
	callq	_malloc
	movq	%rax, 16(%rsp)
	movl	4(%rsp), %edi
	movq	8(%rsp), %rdx
	movq	%rax, %rsi
	callq	_accept
	movl	%eax, (%rsp)
	movq	8(%rsp), %rdi
	callq	_free
	movq	16(%rsp), %rdi
	callq	_free
	movl	(%rsp), %eax
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_net_close_impl                 ## -- Begin function net_close_impl
	.p2align	4, 0x90
_net_close_impl:                        ## @net_close_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	%edi, 4(%rsp)
	callq	_close
	popq	%rax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_net_read_impl                  ## -- Begin function net_read_impl
	.p2align	4, 0x90
_net_read_impl:                         ## @net_read_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movl	%edi, 4(%rsp)
	movq	%rsi, 16(%rsp)
	movq	%rdx, 8(%rsp)
	callq	_read
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_net_write_impl                 ## -- Begin function net_write_impl
	.p2align	4, 0x90
_net_write_impl:                        ## @net_write_impl
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movl	%edi, 4(%rsp)
	movq	%rsi, 16(%rsp)
	movq	%rdx, 8(%rsp)
	callq	_write
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_args                           ## -- Begin function args
	.p2align	4, 0x90
_args:                                  ## @args
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	_args_impl
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_get_arg                        ## -- Begin function get_arg
	.p2align	4, 0x90
_get_arg:                               ## @get_arg
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	%edi, 4(%rsp)
	callq	_get_arg_impl
	popq	%rsi
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_current_dir                    ## -- Begin function current_dir
	.p2align	4, 0x90
_current_dir:                           ## @current_dir
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	leaq	L___unnamed_18(%rip), %rsi
	movl	$40, %edx
	movl	$2, %edi
	callq	_write
	callq	_getcwd_impl
	movq	%rax, 8(%rsp)
	movl	%edx, 16(%rsp)
	movl	%ecx, 20(%rsp)
	leaq	L___unnamed_19(%rip), %rsi
	movl	$33, %edx
	movl	$2, %edi
	callq	_write
	movq	8(%rsp), %rax
	movl	16(%rsp), %edx
	movl	20(%rsp), %ecx
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_set_current_dir                ## -- Begin function set_current_dir
	.p2align	4, 0x90
_set_current_dir:                       ## @set_current_dir
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	callq	_String_c_str
	movq	%rax, %rdi
	callq	_chdir_impl
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_var                            ## -- Begin function var
	.p2align	4, 0x90
_var:                                   ## @var
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 16(%rsp)
	movl	12(%rdi), %eax
	movl	8(%rdi), %ecx
	movq	(%rdi), %rdx
	movq	%rdx, (%rsp)
	movl	%ecx, 8(%rsp)
	movl	%eax, 12(%rsp)
	movq	%rsp, %rdi
	callq	_getenv_impl
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_set_var                        ## -- Begin function set_var
	.p2align	4, 0x90
_set_var:                               ## @set_var
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	movq	%rdi, 48(%rsp)
	movq	%rsi, 40(%rsp)
	movq	(%rdi), %rax
	movl	8(%rdi), %ecx
	movl	12(%rdi), %edx
	movl	%edx, 36(%rsp)
	movl	%ecx, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	(%rsi), %rax
	movl	12(%rsi), %ecx
	movl	8(%rsi), %edx
	movl	%edx, 16(%rsp)
	movl	%ecx, 20(%rsp)
	movq	%rax, 8(%rsp)
	leaq	24(%rsp), %rdi
	leaq	8(%rsp), %rsi
	callq	_setenv_impl
	addq	$56, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_remove_var                     ## -- Begin function remove_var
	.p2align	4, 0x90
_remove_var:                            ## @remove_var
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 16(%rsp)
	movl	12(%rdi), %eax
	movl	8(%rdi), %ecx
	movq	(%rdi), %rdx
	movq	%rdx, (%rsp)
	movl	%ecx, 8(%rsp)
	movl	%eax, 12(%rsp)
	movq	%rsp, %rdi
	callq	_unsetenv_impl
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_encap_File_drop                ## -- Begin function encap_File_drop
	.p2align	4, 0x90
_encap_File_drop:                       ## @encap_File_drop
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 8(%rsp)
	movl	$0, 20(%rsp)
	cmpb	$1, 9(%rdi)
	jne	LBB81_1
## %bb.2:                               ## %land.rhs
	movq	8(%rsp), %rax
	movzbl	8(%rax), %eax
	testb	$1, %al
	jne	LBB81_5
LBB81_4:
	xorl	%eax, %eax
	testb	%al, %al
	jne	LBB81_7
	jmp	LBB81_8
LBB81_1:
	xorl	%eax, %eax
	testb	$1, %al
	je	LBB81_4
LBB81_5:                                ## %land.rhs2
	movq	8(%rsp), %rax
	movq	(%rax), %rax
	cmpq	_ADDR0(%rip), %rax
	setne	%al
	testb	%al, %al
	je	LBB81_8
LBB81_7:                                ## %then
	movq	8(%rsp), %rax
	movq	(%rax), %rdi
	callq	_fclose
	movq	8(%rsp), %rax
	movb	$0, 8(%rax)
	movq	_ADDR0(%rip), %rax
	movq	8(%rsp), %rcx
	movq	%rax, (%rcx)
LBB81_8:                                ## %ifcont
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_encap_File_clone               ## -- Begin function encap_File_clone
	.p2align	4, 0x90
_encap_File_clone:                      ## @encap_File_clone
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	movq	(%rdi), %rax
	movq	%rax, -24(%rsp)
	movzbl	8(%rdi), %edx
	movb	%dl, -16(%rsp)
	movb	$0, -15(%rsp)
	xorl	%ecx, %ecx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_File_open                      ## -- Begin function File_open
	.p2align	4, 0x90
_File_open:                             ## @File_open
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	subq	$32, %rsp
	.cfi_offset %rbx, -48
	.cfi_offset %r12, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movq	%rdi, %rbx
	movq	%rsi, -64(%rbp)
	movq	%rdx, -48(%rbp)
	movq	%rsi, %rdi
	callq	_String_c_str
	movq	%rax, %r14
	movq	-48(%rbp), %rdi
	callq	_String_c_str
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	_fopen
	movq	%rax, -40(%rbp)
	movl	$0, -52(%rbp)
	cmpq	_ADDR0(%rip), %rax
	je	LBB83_1
## %bb.3:                               ## %else
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movq	-40(%rbp), %rcx
	movq	%rcx, -16(%rax)
	movw	$257, -8(%rax)                  ## imm = 0x101
	movq	%rsp, %rax
	leaq	-16(%rax), %rdi
	movq	%rdi, %rsp
	movq	%rcx, -16(%rax)
	movw	$257, -8(%rax)                  ## imm = 0x101
	movq	%rsp, %rdx
	leaq	-32(%rdx), %rsp
	movb	$1, -32(%rdx)
	movq	%rcx, -31(%rdx)
	movw	$257, -23(%rdx)                 ## imm = 0x101
	movq	-32(%rdx), %rcx
	movq	-24(%rdx), %rsi
	movzbl	-16(%rdx), %edx
	movq	%rsp, %r8
	leaq	-32(%r8), %rsp
	movb	%dl, -16(%r8)
	movq	%rsi, -24(%r8)
	movq	%rcx, -32(%r8)
	movb	$0, -7(%rax)
	movq	-32(%r8), %r14
	movq	-24(%r8), %r15
	movzbl	-16(%r8), %r12d
	callq	_encap_File_drop
	movb	%r12b, 16(%rbx)
	movq	%r15, 8(%rbx)
	movq	%r14, (%rbx)
	jmp	LBB83_2
LBB83_1:                                ## %then
	leaq	L___unnamed_20(%rip), %rdi
	callq	_String_from
	movq	%rsp, %rsi
	leaq	-32(%rsi), %rsp
	movb	$0, -32(%rsi)
	movl	%edx, -23(%rsi)
	movl	%ecx, -19(%rsi)
	movq	%rax, -31(%rsi)
	movq	-24(%rsi), %rax
	movzbl	-16(%rsi), %ecx
	movq	-32(%rsi), %rdx
	movq	%rdx, (%rbx)
	movb	%cl, 16(%rbx)
	movq	%rax, 8(%rbx)
LBB83_2:                                ## %then
	movq	%rbx, %rax
	leaq	-32(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_File_close                     ## -- Begin function File_close
	.p2align	4, 0x90
_File_close:                            ## @File_close
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 8(%rsp)
	movl	$0, 20(%rsp)
	cmpb	$1, 8(%rdi)
	jne	LBB84_2
## %bb.1:                               ## %then
	movq	8(%rsp), %rax
	movq	(%rax), %rdi
	callq	_fclose
	movq	8(%rsp), %rax
	movb	$0, 8(%rax)
	movq	_ADDR0(%rip), %rax
	movq	8(%rsp), %rcx
	movq	%rax, (%rcx)
LBB84_2:                                ## %ifcont
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_File_read_to_string            ## -- Begin function File_read_to_string
	.p2align	4, 0x90
_File_read_to_string:                   ## @File_read_to_string
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movq	%rdi, %rbx
	movq	%rsi, -48(%rbp)
	movl	$0, -52(%rbp)
	cmpb	$0, 8(%rsi)
	jne	LBB85_4
## %bb.1:                               ## %then
	leaq	L___unnamed_21(%rip), %rdi
	callq	_String_from
	movq	%rsp, %rsi
	leaq	-32(%rsi), %rsp
	movb	$0, -32(%rsi)
	jmp	LBB85_2
LBB85_4:                                ## %else
	movq	-48(%rbp), %rax
	movq	(%rax), %rdi
	xorl	%esi, %esi
	movl	$2, %edx
	callq	_fseek
	movq	-48(%rbp), %rax
	movq	(%rax), %rdi
	callq	_ftell
	movq	%rsp, %r15
	leaq	-16(%r15), %r14
	movq	%r14, %rsp
	movq	%rax, -16(%r15)
	movq	-48(%rbp), %rax
	movq	(%rax), %rdi
	callq	_rewind
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	cmpq	$0, -16(%r15)
	jg	LBB85_6
## %bb.5:                               ## %then7
	leaq	L___unnamed_22(%rip), %rdi
	callq	_String_from
	movq	%rsp, %rsi
	leaq	-32(%rsi), %rsp
	movb	$1, -32(%rsi)
LBB85_2:                                ## %then
	movl	%edx, -23(%rsi)
	movl	%ecx, -19(%rsi)
	movq	%rax, -31(%rsi)
	movq	-24(%rsi), %rax
	movzbl	-16(%rsi), %ecx
	movq	-32(%rsi), %rdx
	movq	%rdx, (%rbx)
	movb	%cl, 16(%rbx)
	movq	%rax, 8(%rbx)
	jmp	LBB85_3
LBB85_6:                                ## %else12
	movq	(%r14), %rdi
	incq	%rdi
	callq	_malloc
	movq	%rsp, %r15
	leaq	-16(%r15), %rsp
	movq	%rax, -16(%r15)
	movq	(%r14), %rdx
	movq	-48(%rbp), %rcx
	movq	(%rcx), %rcx
	movl	$1, %esi
	movq	%rax, %rdi
	callq	_fread
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movq	%rax, -16(%rcx)
	movq	-16(%r15), %rdx
	movb	$0, (%rdx,%rax)
	movq	-16(%r15), %rdi
	movl	-16(%rcx), %esi
	callq	_String_from_with_len
	movq	%rsp, %r12
	leaq	-16(%r12), %r14
	movq	%r14, %rsp
	movl	%ecx, -4(%r12)
	movl	%edx, -8(%r12)
	movq	%rax, -16(%r12)
	movq	-16(%r15), %rdi
	callq	_free
	movq	-16(%r12), %rax
	movq	-8(%r12), %rcx
	movq	%rsp, %rdx
	leaq	-32(%rdx), %rsp
	movb	$1, -32(%rdx)
	movq	%rcx, -23(%rdx)
	movq	%rax, -31(%rdx)
	movzbl	-16(%rdx), %r15d
	movq	-32(%rdx), %r12
	movq	-24(%rdx), %r13
	movq	%rsp, %rax
	leaq	-32(%rax), %rsp
	movq	%r13, -24(%rax)
	movq	%r12, -32(%rax)
	movb	%r15b, -16(%rax)
	movq	%r14, %rdi
	callq	_encap_String_drop
	movb	%r15b, 16(%rbx)
	movq	%r13, 8(%rbx)
	movq	%r12, (%rbx)
LBB85_3:                                ## %then
	movq	%rbx, %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_File_write                     ## -- Begin function File_write
	.p2align	4, 0x90
_File_write:                            ## @File_write
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
	.cfi_def_cfa_offset 48
	.cfi_offset %rbx, -16
	movq	%rdi, 16(%rsp)
	movq	%rsi, 8(%rsp)
	movl	$0, 28(%rsp)
	cmpb	$0, 8(%rdi)
	jne	LBB86_3
## %bb.1:                               ## %then
	xorl	%eax, %eax
	jmp	LBB86_2
LBB86_3:                                ## %else
	movq	8(%rsp), %rdi
	callq	_String_c_str
	movq	%rax, %rbx
	movq	8(%rsp), %rdi
	callq	_String_len
	movl	%eax, %edx
	movq	16(%rsp), %rax
	movq	(%rax), %rcx
	movl	$1, %esi
	movq	%rbx, %rdi
	callq	_fwrite
LBB86_2:                                ## %then
	addq	$32, %rsp
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_File_is_eof                    ## -- Begin function File_is_eof
	.p2align	4, 0x90
_File_is_eof:                           ## @File_is_eof
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 8(%rsp)
	movl	$0, 20(%rsp)
	cmpb	$0, 8(%rdi)
	jne	LBB87_2
## %bb.1:                               ## %then
	movb	$1, %al
	addq	$24, %rsp
	retq
LBB87_2:                                ## %else
	movq	8(%rsp), %rax
	movq	(%rax), %rdi
	callq	_feof
	testl	%eax, %eax
	setne	%al
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_File_read                      ## -- Begin function File_read
	.p2align	4, 0x90
_File_read:                             ## @File_read
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset %rbx, -24
	movq	%rdi, %rbx
	movq	%rsi, -32(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -16(%rbp)
	movl	$0, -36(%rbp)
	cmpb	$0, 8(%rsi)
	jne	LBB88_3
## %bb.1:                               ## %then
	leaq	L___unnamed_23(%rip), %rdi
	callq	_String_from
	movq	%rsp, %rsi
	leaq	-32(%rsi), %rsp
	movb	$0, -32(%rsi)
	movl	%edx, -23(%rsi)
	movl	%ecx, -19(%rsi)
	movq	%rax, -31(%rsi)
	movq	-24(%rsi), %rax
	movzbl	-16(%rsi), %ecx
	movq	-32(%rsi), %rdx
	movq	%rdx, (%rbx)
	movb	%cl, 16(%rbx)
	movq	%rax, 8(%rbx)
	jmp	LBB88_2
LBB88_3:                                ## %else
	movq	-24(%rbp), %rdi
	movq	-16(%rbp), %rdx
	movq	-32(%rbp), %rax
	movq	(%rax), %rcx
	movl	$1, %esi
	callq	_fread
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movq	%rax, -16(%rcx)
	movq	%rsp, %rcx
	leaq	-32(%rcx), %rsp
	movb	$1, -32(%rcx)
	movq	%rax, -31(%rcx)
	movq	-32(%rcx), %rax
	movq	-24(%rcx), %rdx
	movzbl	-16(%rcx), %ecx
	movb	%cl, 16(%rbx)
	movq	%rdx, 8(%rbx)
	movq	%rax, (%rbx)
LBB88_2:                                ## %then
	movq	%rbx, %rax
	leaq	-8(%rbp), %rsp
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_File_write_buf                 ## -- Begin function File_write_buf
	.p2align	4, 0x90
_File_write_buf:                        ## @File_write_buf
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, 24(%rsp)
	movq	%rsi, 16(%rsp)
	movq	%rdx, 8(%rsp)
	movl	$0, 36(%rsp)
	cmpb	$0, 8(%rdi)
	jne	LBB89_2
## %bb.1:                               ## %then
	xorl	%eax, %eax
	addq	$40, %rsp
	retq
LBB89_2:                                ## %else
	movq	16(%rsp), %rdi
	movq	8(%rsp), %rdx
	movq	24(%rsp), %rax
	movq	(%rax), %rcx
	movl	$1, %esi
	callq	_fwrite
	addq	$40, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_File_read_line                 ## -- Begin function File_read_line
	.p2align	4, 0x90
_File_read_line:                        ## @File_read_line
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movq	%rdi, %rbx
	movq	%rsi, -48(%rbp)
	movl	$0, -52(%rbp)
	cmpb	$0, 8(%rsi)
	jne	LBB90_3
## %bb.1:                               ## %then
	leaq	L___unnamed_24(%rip), %rdi
	callq	_String_from
	movq	%rsp, %rsi
	leaq	-32(%rsi), %rsp
	movb	$0, -32(%rsi)
	movl	%edx, -23(%rsi)
	movl	%ecx, -19(%rsi)
	movq	%rax, -31(%rsi)
	movq	-24(%rsi), %rax
	movzbl	-16(%rsi), %ecx
	movq	-32(%rsi), %rdx
	movq	%rdx, (%rbx)
	movb	%cl, 16(%rbx)
	movq	%rax, 8(%rbx)
	jmp	LBB90_2
LBB90_3:                                ## %else
	leaq	L___unnamed_25(%rip), %rdi
	callq	_String_from
	movq	%rsp, %rsi
	leaq	-16(%rsi), %r14
	movq	%r14, %rsp
	movl	%ecx, -4(%rsi)
	movl	%edx, -8(%rsi)
	movq	%rax, -16(%rsi)
	movq	%rsp, %rax
	leaq	-16(%rax), %r15
	movq	%r15, %rsp
	movb	$0, -16(%rax)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	jmp	LBB90_4
	.p2align	4, 0x90
LBB90_6:                                ## %then5
                                        ##   in Loop: Header=BB90_4 Depth=1
	movb	$1, (%r15)
LBB90_4:                                ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	cmpb	$0, (%r15)
	jne	LBB90_9
## %bb.5:                               ## %while_loop
                                        ##   in Loop: Header=BB90_4 Depth=1
	movq	-48(%rbp), %rax
	movq	(%rax), %rdi
	callq	_fgetc
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rcx
	movq	%rcx, %rsp
	movl	%eax, -16(%rdx)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	cmpl	$-1, -16(%rdx)
	je	LBB90_6
## %bb.7:                               ## %else6
                                        ##   in Loop: Header=BB90_4 Depth=1
	movl	(%rcx), %esi
	movq	%rsp, %r12
	leaq	-16(%r12), %rsp
	movb	%sil, -16(%r12)
	movq	%r14, %rdi
	callq	_String_push_char
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	cmpb	$10, -16(%r12)
	jne	LBB90_4
## %bb.8:                               ## %then9
                                        ##   in Loop: Header=BB90_4 Depth=1
	movb	$1, (%r15)
	jmp	LBB90_4
LBB90_9:                                ## %while_else
	movq	(%r14), %rax
	movq	8(%r14), %rcx
	movq	%rsp, %rdx
	leaq	-32(%rdx), %rsp
	movb	$1, -32(%rdx)
	movq	%rcx, -23(%rdx)
	movq	%rax, -31(%rdx)
	movzbl	-16(%rdx), %r15d
	movq	-32(%rdx), %r12
	movq	-24(%rdx), %r13
	movq	%rsp, %rax
	leaq	-32(%rax), %rsp
	movq	%r13, -24(%rax)
	movq	%r12, -32(%rax)
	movb	%r15b, -16(%rax)
	movq	%r14, %rdi
	callq	_encap_String_drop
	movb	%r15b, 16(%rbx)
	movq	%r13, 8(%rbx)
	movq	%r12, (%rbx)
LBB90_2:                                ## %then
	movq	%rbx, %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_File_flush                     ## -- Begin function File_flush
	.p2align	4, 0x90
_File_flush:                            ## @File_flush
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 8(%rsp)
	movl	$0, 20(%rsp)
	cmpb	$1, 8(%rdi)
	jne	LBB91_2
## %bb.1:                               ## %then
	movq	8(%rsp), %rax
	movq	(%rax), %rdi
	callq	_fflush
LBB91_2:                                ## %ifcont
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_File_seek                      ## -- Begin function File_seek
	.p2align	4, 0x90
_File_seek:                             ## @File_seek
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rdi, 24(%rsp)
	movq	%rsi, 16(%rsp)
	movl	%edx, 12(%rsp)
	movl	$0, 36(%rsp)
	cmpb	$0, 8(%rdi)
	jne	LBB92_2
## %bb.1:                               ## %then
	movl	$-1, %eax
	addq	$40, %rsp
	retq
LBB92_2:                                ## %else
	movq	24(%rsp), %rax
	movq	(%rax), %rdi
	movq	16(%rsp), %rsi
	movl	12(%rsp), %edx
	callq	_fseek
	addq	$40, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_File_tell                      ## -- Begin function File_tell
	.p2align	4, 0x90
_File_tell:                             ## @File_tell
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 8(%rsp)
	movl	$0, 20(%rsp)
	cmpb	$0, 8(%rdi)
	jne	LBB93_2
## %bb.1:                               ## %then
	movq	$-1, %rax
	addq	$24, %rsp
	retq
LBB93_2:                                ## %else
	movq	8(%rsp), %rax
	movq	(%rax), %rdi
	callq	_ftell
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_Result_M_File_String_is_ok     ## -- Begin function Result_M_File_String_is_ok
	.p2align	4, 0x90
_Result_M_File_String_is_ok:            ## @Result_M_File_String_is_ok
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	movq	9(%rdi), %rax
	movq	1(%rdi), %rcx
	movzbl	(%rdi), %edx
	movb	%dl, -32(%rsp)
	movq	%rcx, -31(%rsp)
	movq	%rax, -23(%rsp)
	cmpb	$1, %dl
	jne	LBB94_1
## %bb.2:                               ## %case_Ok
	movb	$1, %al
	retq
LBB94_1:                                ## %match_default
	xorl	%eax, %eax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_Result_M_File_String_is_err    ## -- Begin function Result_M_File_String_is_err
	.p2align	4, 0x90
_Result_M_File_String_is_err:           ## @Result_M_File_String_is_err
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	callq	_Result_M_File_String_is_ok
	xorb	$1, %al
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_Result_M_File_String_unwrap    ## -- Begin function Result_M_File_String_unwrap
	.p2align	4, 0x90
_Result_M_File_String_unwrap:           ## @Result_M_File_String_unwrap
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -32(%rbp)
	movq	9(%rdi), %rax
	movq	1(%rdi), %rcx
	movzbl	(%rdi), %edx
	movb	%dl, -24(%rbp)
	movq	%rcx, -23(%rbp)
	movq	%rax, -15(%rbp)
	cmpb	$1, %dl
	jne	LBB96_1
## %bb.2:                               ## %case_Ok
	movq	-23(%rbp), %rax
	movzbl	-15(%rbp), %edx
	movzbl	-14(%rbp), %ecx
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rsp
	movb	%cl, -7(%rsi)
	movb	%dl, -8(%rsi)
	movq	%rax, -16(%rsi)
	jmp	LBB96_3
LBB96_1:                                ## %match_default
	movl	$1, %edi
	callq	_exit
	movq	-32(%rbp), %rdi
	callq	_Result_M_File_String_unwrap
LBB96_3:                                ## %case_Ok
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_Result_M_File_String_unwrap_err ## -- Begin function Result_M_File_String_unwrap_err
	.p2align	4, 0x90
_Result_M_File_String_unwrap_err:       ## @Result_M_File_String_unwrap_err
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -32(%rbp)
	movq	9(%rdi), %rax
	movq	1(%rdi), %rcx
	movzbl	(%rdi), %edx
	movb	%dl, -24(%rbp)
	movq	%rcx, -23(%rbp)
	movq	%rax, -15(%rbp)
	testb	%dl, %dl
	je	LBB97_2
## %bb.1:                               ## %match_default
	movl	$1, %edi
	callq	_exit
	movq	-32(%rbp), %rdi
	callq	_Result_M_File_String_unwrap_err
	jmp	LBB97_3
LBB97_2:                                ## %case_Err
	movq	-23(%rbp), %rax
	movl	-15(%rbp), %edx
	movl	-11(%rbp), %ecx
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rsp
	movl	%ecx, -4(%rsi)
	movl	%edx, -8(%rsi)
	movq	%rax, -16(%rsi)
LBB97_3:                                ## %case_Err
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_Result_M_String_String_is_ok   ## -- Begin function Result_M_String_String_is_ok
	.p2align	4, 0x90
_Result_M_String_String_is_ok:          ## @Result_M_String_String_is_ok
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	movq	9(%rdi), %rax
	movq	1(%rdi), %rcx
	movzbl	(%rdi), %edx
	movb	%dl, -32(%rsp)
	movq	%rcx, -31(%rsp)
	movq	%rax, -23(%rsp)
	cmpb	$1, %dl
	jne	LBB98_1
## %bb.2:                               ## %case_Ok
	movb	$1, %al
	retq
LBB98_1:                                ## %match_default
	xorl	%eax, %eax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_Result_M_String_String_is_err  ## -- Begin function Result_M_String_String_is_err
	.p2align	4, 0x90
_Result_M_String_String_is_err:         ## @Result_M_String_String_is_err
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	callq	_Result_M_String_String_is_ok
	xorb	$1, %al
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_Result_M_String_String_unwrap  ## -- Begin function Result_M_String_String_unwrap
	.p2align	4, 0x90
_Result_M_String_String_unwrap:         ## @Result_M_String_String_unwrap
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -32(%rbp)
	movq	9(%rdi), %rax
	movq	1(%rdi), %rcx
	movzbl	(%rdi), %edx
	movb	%dl, -24(%rbp)
	movq	%rcx, -23(%rbp)
	movq	%rax, -15(%rbp)
	cmpb	$1, %dl
	jne	LBB100_1
## %bb.2:                               ## %case_Ok
	movq	-23(%rbp), %rax
	movl	-15(%rbp), %edx
	movl	-11(%rbp), %ecx
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rsp
	movl	%ecx, -4(%rsi)
	movl	%edx, -8(%rsi)
	movq	%rax, -16(%rsi)
	jmp	LBB100_3
LBB100_1:                               ## %match_default
	movl	$1, %edi
	callq	_exit
	movq	-32(%rbp), %rdi
	callq	_Result_M_String_String_unwrap
LBB100_3:                               ## %case_Ok
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_Result_M_String_String_unwrap_err ## -- Begin function Result_M_String_String_unwrap_err
	.p2align	4, 0x90
_Result_M_String_String_unwrap_err:     ## @Result_M_String_String_unwrap_err
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -32(%rbp)
	movq	9(%rdi), %rax
	movq	1(%rdi), %rcx
	movzbl	(%rdi), %edx
	movb	%dl, -24(%rbp)
	movq	%rcx, -23(%rbp)
	movq	%rax, -15(%rbp)
	testb	%dl, %dl
	je	LBB101_2
## %bb.1:                               ## %match_default
	movl	$1, %edi
	callq	_exit
	movq	-32(%rbp), %rdi
	callq	_Result_M_String_String_unwrap_err
	jmp	LBB101_3
LBB101_2:                               ## %case_Err
	movq	-23(%rbp), %rax
	movl	-15(%rbp), %edx
	movl	-11(%rbp), %ecx
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rsp
	movl	%ecx, -4(%rsi)
	movl	%edx, -8(%rsi)
	movq	%rax, -16(%rsi)
LBB101_3:                               ## %case_Err
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_Result_M_u64_String_is_ok      ## -- Begin function Result_M_u64_String_is_ok
	.p2align	4, 0x90
_Result_M_u64_String_is_ok:             ## @Result_M_u64_String_is_ok
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	movq	9(%rdi), %rax
	movq	1(%rdi), %rcx
	movzbl	(%rdi), %edx
	movb	%dl, -32(%rsp)
	movq	%rcx, -31(%rsp)
	movq	%rax, -23(%rsp)
	cmpb	$1, %dl
	jne	LBB102_1
## %bb.2:                               ## %case_Ok
	movb	$1, %al
	retq
LBB102_1:                               ## %match_default
	xorl	%eax, %eax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_Result_M_u64_String_is_err     ## -- Begin function Result_M_u64_String_is_err
	.p2align	4, 0x90
_Result_M_u64_String_is_err:            ## @Result_M_u64_String_is_err
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	callq	_Result_M_u64_String_is_ok
	xorb	$1, %al
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_Result_M_u64_String_unwrap     ## -- Begin function Result_M_u64_String_unwrap
	.p2align	4, 0x90
_Result_M_u64_String_unwrap:            ## @Result_M_u64_String_unwrap
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	9(%rdi), %rax
	movq	1(%rdi), %rcx
	movzbl	(%rdi), %edx
	movb	%dl, -32(%rbp)
	movq	%rcx, -31(%rbp)
	movq	%rax, -23(%rbp)
	cmpb	$1, %dl
	jne	LBB104_1
## %bb.2:                               ## %case_Ok
	movq	-31(%rbp), %rax
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movq	%rax, -16(%rcx)
	jmp	LBB104_3
LBB104_1:                               ## %match_default
	movl	$1, %edi
	callq	_exit
	movq	-8(%rbp), %rdi
	callq	_Result_M_u64_String_unwrap
LBB104_3:                               ## %case_Ok
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_Result_M_u64_String_unwrap_err ## -- Begin function Result_M_u64_String_unwrap_err
	.p2align	4, 0x90
_Result_M_u64_String_unwrap_err:        ## @Result_M_u64_String_unwrap_err
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -32(%rbp)
	movq	9(%rdi), %rax
	movq	1(%rdi), %rcx
	movzbl	(%rdi), %edx
	movb	%dl, -24(%rbp)
	movq	%rcx, -23(%rbp)
	movq	%rax, -15(%rbp)
	testb	%dl, %dl
	je	LBB105_2
## %bb.1:                               ## %match_default
	movl	$1, %edi
	callq	_exit
	movq	-32(%rbp), %rdi
	callq	_Result_M_u64_String_unwrap_err
	jmp	LBB105_3
LBB105_2:                               ## %case_Err
	movq	-23(%rbp), %rax
	movl	-15(%rbp), %edx
	movl	-11(%rbp), %ecx
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rsp
	movl	%ecx, -4(%rsi)
	movl	%edx, -8(%rsi)
	movq	%rax, -16(%rsi)
LBB105_3:                               ## %case_Err
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_stdin                          ## -- Begin function stdin
	.p2align	4, 0x90
_stdin:                                 ## @stdin
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	leaq	L___unnamed_26(%rip), %rsi
	xorl	%edi, %edi
	callq	_fdopen
	movq	%rax, 16(%rsp)
	movq	%rax, (%rsp)
	testq	%rax, %rax
	setne	%dl
	setne	8(%rsp)
	movb	$0, 9(%rsp)
	xorl	%ecx, %ecx
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_stdout                         ## -- Begin function stdout
	.p2align	4, 0x90
_stdout:                                ## @stdout
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	leaq	L___unnamed_27(%rip), %rsi
	movl	$1, %edi
	callq	_fdopen
	movq	%rax, 16(%rsp)
	movq	%rax, (%rsp)
	testq	%rax, %rax
	setne	%dl
	setne	8(%rsp)
	movb	$0, 9(%rsp)
	xorl	%ecx, %ecx
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_stderr                         ## -- Begin function stderr
	.p2align	4, 0x90
_stderr:                                ## @stderr
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	leaq	L___unnamed_28(%rip), %rsi
	movl	$2, %edi
	callq	_fdopen
	movq	%rax, 16(%rsp)
	movq	%rax, (%rsp)
	testq	%rax, %rax
	setne	%dl
	setne	8(%rsp)
	movb	$0, 9(%rsp)
	xorl	%ecx, %ecx
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_print                          ## -- Begin function print
	.p2align	4, 0x90
_print:                                 ## @print
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$48, %rsp
	.cfi_def_cfa_offset 64
	.cfi_offset %rbx, -16
	movq	%rdi, 40(%rsp)
	callq	_stdout
	movq	%rax, 24(%rsp)
	andb	$1, %dl
	movb	%dl, 32(%rsp)
	andb	$1, %cl
	movb	%cl, 33(%rsp)
	movq	40(%rsp), %rax
	movl	8(%rax), %ecx
	movl	12(%rax), %edx
	movq	(%rax), %rax
	movq	%rax, 8(%rsp)
	movl	%edx, 20(%rsp)
	movl	%ecx, 16(%rsp)
	leaq	24(%rsp), %rbx
	leaq	8(%rsp), %rsi
	movq	%rbx, %rdi
	callq	_File_write
	movq	%rbx, %rdi
	callq	_encap_File_drop
	addq	$48, %rsp
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_exists                         ## -- Begin function exists
	.p2align	4, 0x90
_exists:                                ## @exists
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, 16(%rsp)
	callq	_String_c_str
	leaq	L___unnamed_29(%rip), %rsi
	movq	%rax, %rdi
	callq	_fopen
	movq	%rax, (%rsp)
	movl	$0, 12(%rsp)
	testq	%rax, %rax
	je	LBB110_2
## %bb.1:                               ## %then
	movq	(%rsp), %rdi
	callq	_fclose
	movb	$1, %al
	addq	$24, %rsp
	retq
LBB110_2:                               ## %else
	xorl	%eax, %eax
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_remove_file                    ## -- Begin function remove_file
	.p2align	4, 0x90
_remove_file:                           ## @remove_file
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	callq	_String_c_str
	movq	%rax, %rdi
	callq	_remove
	testl	%eax, %eax
	sete	%al
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_rename_file                    ## -- Begin function rename_file
	.p2align	4, 0x90
_rename_file:                           ## @rename_file
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -16
	movq	%rdi, 8(%rsp)
	movq	%rsi, (%rsp)
	callq	_String_c_str
	movq	%rax, %rbx
	movq	(%rsp), %rdi
	callq	_String_c_str
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_rename
	testl	%eax, %eax
	sete	%al
	addq	$16, %rsp
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$72, %rsp
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	callq	_args
	movl	%eax, -44(%rbp)
	movl	$0, -60(%rbp)
	cmpl	$1, %eax
	jg	LBB113_3
## %bb.1:                               ## %then
	leaq	L___unnamed_30(%rip), %rdi
	leaq	L___unnamed_31(%rip), %rsi
	xorl	%eax, %eax
	callq	_printf
LBB113_2:                               ## %while_else
	xorl	%eax, %eax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
LBB113_3:                               ## %else
	movq	%rsp, %rax
	leaq	-16(%rax), %r15
	movq	%r15, %rsp
	movl	$1, -16(%rax)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movq	%r15, -56(%rbp)                 ## 8-byte Spill
	jmp	LBB113_4
	.p2align	4, 0x90
LBB113_8:                               ## %ifcont10
                                        ##   in Loop: Header=BB113_4 Depth=1
	movq	%r15, %rdi
	callq	_File_close
	movq	%r15, %rdi
	callq	_encap_File_drop
LBB113_9:                               ## %ifcont13
                                        ##   in Loop: Header=BB113_4 Depth=1
	movq	-56(%rbp), %r15                 ## 8-byte Reload
	incl	(%r15)
	movq	%r14, %rdi
	callq	_encap_String_drop
	movq	%rbx, %rdi
	callq	_encap_String_drop
	movq	%r13, %rdi
	callq	_encap_String_drop
LBB113_4:                               ## %while_cond
                                        ## =>This Inner Loop Header: Depth=1
	movl	(%r15), %eax
	cmpl	-44(%rbp), %eax
	jge	LBB113_2
## %bb.5:                               ## %while_loop
                                        ##   in Loop: Header=BB113_4 Depth=1
	movl	(%r15), %edi
	callq	_get_arg
	movq	%rsp, %rsi
	leaq	-16(%rsi), %r13
	movq	%r13, %rsp
	movl	%ecx, -4(%rsi)
	movl	%edx, -8(%rsi)
	movq	%rax, -16(%rsi)
	leaq	L___unnamed_32(%rip), %rdi
	leaq	L___unnamed_33(%rip), %rsi
	xorl	%eax, %eax
	callq	_printf
	movl	(%r15), %esi
	leaq	L___unnamed_34(%rip), %rdi
	xorl	%eax, %eax
	callq	_printf
	leaq	L___unnamed_35(%rip), %rdi
	leaq	L___unnamed_36(%rip), %rsi
	xorl	%eax, %eax
	callq	_printf
	movq	%r13, %rdi
	callq	_String_c_str
	leaq	L___unnamed_37(%rip), %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	callq	_printf
	leaq	L___unnamed_38(%rip), %rdi
	leaq	L___unnamed_39(%rip), %rsi
	xorl	%eax, %eax
	callq	_printf
	movq	%r13, %rdi
	callq	_String_c_str
	movq	%rax, %rdi
	callq	_String_from
	movq	%rsp, %r15
	leaq	-16(%r15), %rbx
	movq	%rbx, %rsp
	movl	%ecx, -4(%r15)
	movl	%edx, -8(%r15)
	movq	%rax, -16(%r15)
	leaq	L___unnamed_40(%rip), %rdi
	callq	_String_from
	movq	%rsp, %r12
	leaq	-16(%r12), %r14
	movq	%r14, %rsp
	movl	%ecx, -4(%r12)
	movl	%edx, -8(%r12)
	movq	%rax, -16(%r12)
	leaq	L___unnamed_41(%rip), %rdi
	leaq	L___unnamed_42(%rip), %rsi
	xorl	%eax, %eax
	callq	_printf
	movq	-16(%r15), %rax
	movq	-8(%r15), %rcx
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rsi
	movq	%rsi, %rsp
	movq	%rcx, -8(%rdx)
	movq	%rax, -16(%rdx)
	movq	-16(%r12), %rax
	movq	-8(%r12), %rcx
	movq	%rsp, %rdi
	leaq	-16(%rdi), %rdx
	movq	%rdx, %rsp
	movq	%rcx, -8(%rdi)
	movq	%rax, -16(%rdi)
	leaq	-104(%rbp), %rdi
	callq	_File_open
	movq	-104(%rbp), %rax
	movq	-96(%rbp), %rcx
	movzbl	-88(%rbp), %edx
	movq	%rsp, %rsi
	leaq	-32(%rsi), %r15
	movq	%r15, %rsp
	movb	%dl, -16(%rsi)
	movq	%rcx, -24(%rsi)
	movq	%rax, -32(%rsi)
	leaq	L___unnamed_43(%rip), %rdi
	leaq	L___unnamed_44(%rip), %rsi
	xorl	%eax, %eax
	callq	_printf
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movq	%r15, %rdi
	callq	_Result_M_File_String_is_ok
	testb	$1, %al
	je	LBB113_9
## %bb.6:                               ## %then6
                                        ##   in Loop: Header=BB113_4 Depth=1
	movq	%r15, %rdi
	callq	_Result_M_File_String_unwrap
	movq	%rsp, %rsi
	leaq	-16(%rsi), %r15
	movq	%r15, %rsp
	andb	$1, %cl
	movb	%cl, -7(%rsi)
	andb	$1, %dl
	movb	%dl, -8(%rsi)
	movq	%rax, -16(%rsi)
	leaq	-80(%rbp), %rdi
	movq	%r15, %rsi
	callq	_File_read_to_string
	movq	-80(%rbp), %rax
	movq	-72(%rbp), %rcx
	movzbl	-64(%rbp), %edx
	movq	%rsp, %rsi
	leaq	-32(%rsi), %r12
	movq	%r12, %rsp
	movb	%dl, -16(%rsi)
	movq	%rcx, -24(%rsi)
	movq	%rax, -32(%rsi)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	$0, -16(%rax)
	movq	%r12, %rdi
	callq	_Result_M_String_String_is_ok
	testb	$1, %al
	je	LBB113_8
## %bb.7:                               ## %then8
                                        ##   in Loop: Header=BB113_4 Depth=1
	movq	%r12, %rdi
	callq	_Result_M_String_String_unwrap
	movq	%rsp, %rsi
	leaq	-16(%rsi), %r12
	movq	%r12, %rsp
	movl	%ecx, -4(%rsi)
	movl	%edx, -8(%rsi)
	movq	%rax, -16(%rsi)
	leaq	L___unnamed_45(%rip), %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	callq	_printf
	leaq	L___unnamed_46(%rip), %rdi
	leaq	L___unnamed_47(%rip), %rsi
	xorl	%eax, %eax
	callq	_printf
	movq	%r12, %rdi
	callq	_encap_String_drop
	jmp	LBB113_8
	.cfi_endproc
                                        ## -- End function
	.globl	_ADDR0                          ## @ADDR0
.zerofill __DATA,__common,_ADDR0,8,3
	.section	__DATA,__data
	.globl	_LIMITS                         ## @LIMITS
	.p2align	4, 0x0
_LIMITS:
	.byte	0                               ## 0x0
	.byte	255                             ## 0xff
	.space	2
	.long	8                               ## 0x8
	.quad	-2147483648                     ## 0xffffffff80000000
	.long	2147483647                      ## 0x7fffffff
	.long	32                              ## 0x20
	.quad	0                               ## 0x0
	.quad	-1                              ## 0xffffffffffffffff
	.long	64                              ## 0x40
	.space	4
	.long	0                               ## 0x0
	.space	4
	.quad	-1                              ## 0xffffffffffffffff
	.long	8                               ## 0x8
	.space	4
	.quad	0x380fffffff9fdba8              ## double 1.17549435E-38
	.quad	0x3e8000000102f4fd              ## double 1.1920929000000001E-7
	.long	2143289344                      ## 0x7fc00000
	.long	2139095040                      ## 0x7f800000
	.quad	2147483648                      ## 0x80000000

	.section	__DATA,__const
	.globl	__VTable_String_encap           ## @_VTable_String_encap
	.p2align	3, 0x0
__VTable_String_encap:
	.quad	_encap_String_clone
	.quad	_encap_String_drop

	.globl	__VTable_SplitIterator_encap    ## @_VTable_SplitIterator_encap
	.p2align	3, 0x0
__VTable_SplitIterator_encap:
	.quad	_encap_SplitIterator_clone
	.quad	_encap_SplitIterator_drop

	.section	__TEXT,__cstring,cstring_literals
L___unnamed_1:                          ## @0
	.space	1

L___unnamed_2:                          ## @1
	.asciz	"DEBUG: from_take entry\n"

L___unnamed_3:                          ## @2
	.asciz	"DEBUG: from_take strlen done\n"

L___unnamed_4:                          ## @3
	.asciz	"DEBUG: from_take free skipped\n"

L___unnamed_5:                          ## @4
	.asciz	"%d"

L___unnamed_6:                          ## @5
	.asciz	"%lld"

L___unnamed_7:                          ## @6
	.asciz	"%.6f"

L___unnamed_8:                          ## @7
	.space	1

L___unnamed_9:                          ## @8
	.space	1

L___unnamed_10:                         ## @9
	.space	1

L___unnamed_11:                         ## @10
	.space	1

L___unnamed_12:                         ## @11
	.space	1

L___unnamed_13:                         ## @12
	.space	1

L___unnamed_14:                         ## @13
	.space	1

	.section	__DATA,__const
	.globl	__VTable_String_Hash            ## @_VTable_String_Hash
	.p2align	3, 0x0
__VTable_String_Hash:
	.quad	_Hash_String_hash

	.section	__TEXT,__cstring,cstring_literals
L___unnamed_15:                         ## @14
	.space	1

L___unnamed_16:                         ## @15
	.space	1

L___unnamed_17:                         ## @16
	.space	1

L___unnamed_18:                         ## @17
	.asciz	"DEBUG: env::current_dir calling getcwd_impl\n"

L___unnamed_19:                         ## @18
	.asciz	"DEBUG: env::current_dir returned\n"

	.section	__DATA,__const
	.globl	__VTable_File_encap             ## @_VTable_File_encap
	.p2align	3, 0x0
__VTable_File_encap:
	.quad	_encap_File_clone
	.quad	_encap_File_drop

	.section	__TEXT,__cstring,cstring_literals
L___unnamed_20:                         ## @19
	.asciz	"Failed to open file"

L___unnamed_21:                         ## @20
	.asciz	"File not open"

L___unnamed_22:                         ## @21
	.space	1

L___unnamed_23:                         ## @22
	.asciz	"File not open"

L___unnamed_24:                         ## @23
	.asciz	"File not open"

L___unnamed_25:                         ## @24
	.space	1

L___unnamed_26:                         ## @25
	.asciz	"r"

L___unnamed_27:                         ## @26
	.asciz	"w"

L___unnamed_28:                         ## @27
	.asciz	"w"

L___unnamed_29:                         ## @28
	.asciz	"r"

L___unnamed_30:                         ## @29
	.asciz	"%s"

L___unnamed_31:                         ## @30
	.asciz	"Usage: cat <file1> <file2> ...\n"

L___unnamed_32:                         ## @31
	.asciz	"%s"

L___unnamed_33:                         ## @32
	.asciz	"DEBUG: get_arg("

L___unnamed_34:                         ## @33
	.asciz	"%d"

L___unnamed_35:                         ## @34
	.asciz	"%s"

L___unnamed_36:                         ## @35
	.asciz	") -> '"

L___unnamed_37:                         ## @36
	.asciz	"%s"

L___unnamed_38:                         ## @37
	.asciz	"%s"

L___unnamed_39:                         ## @38
	.asciz	"'\n"

L___unnamed_40:                         ## @39
	.asciz	"r"

L___unnamed_41:                         ## @40
	.asciz	"%s"

L___unnamed_42:                         ## @41
	.asciz	"DEBUG: about to open file\n"

L___unnamed_43:                         ## @42
	.asciz	"%s"

L___unnamed_44:                         ## @43
	.asciz	"DEBUG: file opened\n"

L___unnamed_45:                         ## @44
	.asciz	"%s"

L___unnamed_46:                         ## @45
	.asciz	"%s"

L___unnamed_47:                         ## @46
	.asciz	"\n"

.subsections_via_symbols
