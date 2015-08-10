	.global user_allocate
	.global user_deallocate
	.code64

user_allocate:
	movq	$0x10,%rax
	int	$0x31
	ret

user_deallocate:
	movq	$0x11,%rax
	int	$0x31
	ret
