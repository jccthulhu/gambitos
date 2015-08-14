	.global user_allocate
	.global user_deallocate
	.global user_vm_map_page
	.global get_pml4t
	.code64

user_allocate:
	movq	$0x10,%rax
	int	$0x31
	ret

user_deallocate:
	movq	$0x11,%rax
	int	$0x31
	ret

user_vm_map_page:
	movq	$0x12,%rax
	int	$0x31
	ret

get_pml4t:
	movq	%cr3,%rax
	ret
