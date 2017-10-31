	.file ""
	.section __TEXT,__literal16,16byte_literals
	.align	4
_caml_negf_mask:
	.quad	0x8000000000000000
	.quad	0
	.align	4
_caml_absf_mask:
	.quad	0x7fffffffffffffff
	.quad	-1
	.data
	.globl	_camlSnd__data_begin
_camlSnd__data_begin:
	.text
	.globl	_camlSnd__code_begin
_camlSnd__code_begin:
	nop
	.data
	.quad	1792
	.globl	_camlSnd
_camlSnd:
	.quad	1
	.data
	.globl	_camlSnd__gc_roots
_camlSnd__gc_roots:
	.quad	_camlSnd
	.quad	0
	.text
	.align	4
	.globl	_camlSnd__second_1207
_camlSnd__second_1207:
	.cfi_startproc
L100:
	movq	8(%rax), %rax
	ret
	.cfi_endproc
	.data
	.quad	3063
_camlSnd__1:
	.quad	_camlSnd__second_1207
	.quad	3
	.text
	.align	4
	.globl	_camlSnd__entry
_camlSnd__entry:
	.cfi_startproc
L101:
	movq	_camlSnd__1@GOTPCREL(%rip), %rax
	movq	_camlSnd@GOTPCREL(%rip), %rbx
	movq	%rax, (%rbx)
	movq	$1, %rax
	ret
	.cfi_endproc
	.data
	.text
	nop
	.globl	_camlSnd__code_end
_camlSnd__code_end:
	.data
				/* relocation table start */
	.align	3
				/* relocation table end */
	.data
	.globl	_camlSnd__data_end
_camlSnd__data_end:
	.long	0
	.globl	_camlSnd__frametable
_camlSnd__frametable:
	.quad	0
