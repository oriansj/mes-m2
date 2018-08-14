

:_start
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____%ebp,%eax
	add____$i8,%eax !4
	movzbl_(%eax),%eax
	add____$i8,%eax !3
	shl____$i8,%eax !0x02
	add____%ebp,%eax
	mov____%eax,0x32 &environ
	mov____%ebp,%eax
	add____$i8,%eax !8
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !4
	movzbl_(%eax),%eax
	push___%eax
					# main(); 
	call32 %main
	add____$i8,%esp !0x0
	test___%eax,%eax
	mov____%eax,%ebx
	mov____$i32,%eax %1
	int____$0x80
	hlt
	leave
	ret

:environ
!0x00 !0x00 !0x00 !0x00
