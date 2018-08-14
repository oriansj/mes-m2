

:strlen
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____$i32,%eax %0x0
	mov____%eax,0x8(%ebp) !0x-4
					# while (s[i]) ...
	jmp32  %_strlen_3_continue
:_strlen_3_loop
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0x-4
	add____$i8,(%eax) !0x1
	pop____%eax
	test___%eax,%eax
:_strlen_3_continue
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	test___%al,%al
	je32   %_strlen_3_break
	jmp32  %_strlen_3_loop
:_strlen_3_break
	mov____0x8(%ebp),%eax !0x-4
	leave
	ret


:eputs
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %strlen
	add____$i8,%esp !0x4
	mov____%eax,0x8(%ebp) !0x-4
					# write(2, s, i); 
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x2
	push___%eax
	call32 %write
	add____$i8,%esp !0xc
	test___%eax,%eax
	mov____$i32,%eax %0x0
	leave
	ret


:oputs
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %strlen
	add____$i8,%esp !0x4
	mov____%eax,0x8(%ebp) !0x-4
					# write(1, s, i); 
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x1
	push___%eax
	call32 %write
	add____$i8,%esp !0xc
	test___%eax,%eax
	mov____$i32,%eax %0x0
	leave
	ret


:puts
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
					# oputs(s); 
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %oputs
	add____$i8,%esp !0x4
	test___%eax,%eax
	push___$i32 &_string_lib/x86-mes/libc.S_0
	call32 %oputs
	add____$i8,%esp !0x4
	leave
	ret


:_exit
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____$i32,%eax SYS_exit
	mov____0x8(%ebp),%ebx !8
	int____$0x80
	leave
	ret


:_write
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____$i32,%eax SYS_write
	mov____0x8(%ebp),%ebx !8
	mov____0x8(%ebp),%ecx !12
	mov____0x8(%ebp),%edx !16
	int____$0x80
	leave
	ret


:write
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x10
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %_write
	add____$i8,%esp !0xc
	mov____%eax,0x8(%ebp) !0x-4
					# if (r < 0) ...else ...
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____$i32,%eax %0x0
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setl___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %_write_9_else
					# errno = -r
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	pop____%eax
	mov____$i32,%eax %0x0
	sub____%edx,%eax
	mov____%eax,0x32 &errno
	test___%eax,%eax
					# r = -1
	mov____$i32,%eax %0x-1
	mov____%eax,0x8(%ebp) !0x-4
	test___%eax,%eax
	jmp32  %_write_9_break
:_write_9_else
					# errno = 0
	mov____$i32,%eax %0x0
	mov____%eax,0x32 &errno
	test___%eax,%eax
:_write_9_break
	mov____0x8(%ebp),%eax !0x-4
	leave
	ret


:exit
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
					# if (__call_at_exit) ...
	mov____0x32,%eax &__call_at_exit
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	je32   %_exit_1_break
	mov____0x32,%eax &__call_at_exit
	movzbl_(%eax),%eax
	movzbl_%al,%eax
	push___%eax
	pop____%eax
	call___*%eax
	add____$i8,%esp !0x0
	test___%eax,%eax
	jmp32  %_exit_1_break
:_exit_1_break
					# _exit(code); 
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %_exit
	add____$i8,%esp !0x4
	test___%eax,%eax
	leave
	ret


:isdigit
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x30
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setge__%al
	movzbl_%al,%eax
	test___%eax,%eax
	test___%eax,%eax
	je32   %_isdigit_6_and_skip_b
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x39
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setle__%al
	movzbl_%al,%eax
	test___%eax,%eax
	test___%eax,%eax
:_isdigit_6_and_skip_b
	leave
	ret


:isxdigit
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %isdigit
	add____$i8,%esp !0x4
	test___%eax,%eax
	jne32  %_isxdigit_4_or_skip_b
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x61
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setge__%al
	movzbl_%al,%eax
	test___%eax,%eax
	test___%eax,%eax
	je32   %_isxdigit_12_and_skip_b
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x66
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setle__%al
	movzbl_%al,%eax
	test___%eax,%eax
	test___%eax,%eax
:_isxdigit_12_and_skip_b
	test___%eax,%eax
:_isxdigit_4_or_skip_b
	leave
	ret


:isspace
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x9
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
	test___%eax,%eax
	jne32  %_isspace_6_or_skip_b
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0xa
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
	test___%eax,%eax
:_isspace_6_or_skip_b
	test___%eax,%eax
	jne32  %_isspace_16_or_skip_b
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0xb
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
	test___%eax,%eax
:_isspace_16_or_skip_b
	test___%eax,%eax
	jne32  %_isspace_26_or_skip_b
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0xc
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
	test___%eax,%eax
:_isspace_26_or_skip_b
	test___%eax,%eax
	jne32  %_isspace_36_or_skip_b
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0xd
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
	test___%eax,%eax
:_isspace_36_or_skip_b
	test___%eax,%eax
	jne32  %_isspace_46_or_skip_b
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x20
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
	test___%eax,%eax
:_isspace_46_or_skip_b
	leave
	ret


:isnumber
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
					# if (base == 2) ...
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____$i32,%eax %0x2
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_isnumber_1_break
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x30
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setge__%al
	movzbl_%al,%eax
	test___%eax,%eax
	test___%eax,%eax
	je32   %_isnumber_14_and_skip_b
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x31
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setle__%al
	movzbl_%al,%eax
	test___%eax,%eax
	test___%eax,%eax
:_isnumber_14_and_skip_b
	leave
	ret
	jmp32  %_isnumber_1_break
:_isnumber_1_break
					# if (base == 8) ...
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____$i32,%eax %0x8
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_isnumber_27_break
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x30
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setge__%al
	movzbl_%al,%eax
	test___%eax,%eax
	test___%eax,%eax
	je32   %_isnumber_40_and_skip_b
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x37
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setle__%al
	movzbl_%al,%eax
	test___%eax,%eax
	test___%eax,%eax
:_isnumber_40_and_skip_b
	leave
	ret
	jmp32  %_isnumber_27_break
:_isnumber_27_break
					# if (base == 10) ...
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____$i32,%eax %0xa
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_isnumber_53_break
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %isdigit
	add____$i8,%esp !0x4
	leave
	ret
	jmp32  %_isnumber_53_break
:_isnumber_53_break
					# if (base == 16) ...
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____$i32,%eax %0x10
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_isnumber_67_break
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %isxdigit
	add____$i8,%esp !0x4
	leave
	ret
	jmp32  %_isnumber_67_break
:_isnumber_67_break
	leave
	ret


:abtoi
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x8
	mov____(%eax),%eax
	mov____%eax,0x8(%ebp) !0x-4
	mov____$i32,%eax %0x0
	mov____%eax,0x8(%ebp) !0x-8
	mov____$i32,%eax %0x1
	mov____%eax,0x8(%ebp) !0x-c
					# if (!base) ...
	mov____0x8(%ebp),%eax !0xc
	test___%eax,%eax
	sete___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %_abtoi_8_break
					# base = 10
	mov____$i32,%eax %0xa
	mov____%eax,0x8(%ebp) !0xc
	test___%eax,%eax
	jmp32  %_abtoi_8_break
:_abtoi_8_break
					# if (*s && *s == '-') ...
	mov____0x8(%ebp),%eax !0x-4
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	test___%al,%al
	je32   %_abtoi_21_break
	mov____0x8(%ebp),%eax !0x-4
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	push___%eax
	mov____$i32,%eax %0x2d
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_abtoi_21_break
					# sign = -1
	mov____$i32,%eax %0x-1
	mov____%eax,0x8(%ebp) !0x-c
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0x-4
	add____$i8,(%eax) !0x1
	pop____%eax
	test___%eax,%eax
	jmp32  %_abtoi_21_break
:_abtoi_21_break
					# while (isnumber(*s, base)) ...
	jmp32  %_abtoi_50_continue
:_abtoi_50_loop
					# i *= base
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x-8
	pop____%edx
	mul____%edx
	mov____%eax,0x8(%ebp) !0x-8
	test___%eax,%eax
					# *s > '9'? ...: ...
	mov____0x8(%ebp),%eax !0x-4
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	push___%eax
	mov____$i32,%eax %0x39
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setg___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %_abtoi_61_else
	mov____$i32,%eax %0x61
	add____$i8,%eax !0x-a
	test___%eax,%eax
	jmp32  %_abtoi_61_break
:_abtoi_61_else
	mov____$i32,%eax %0x30
	test___%eax,%eax
:_abtoi_61_break
	mov____%eax,0x8(%ebp) !0x-10
					# i += *s - m
	mov____0x8(%ebp),%eax !0x-4
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	push___%eax
	mov____0x8(%ebp),%eax !0x-10
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	push___%eax
	mov____0x8(%ebp),%eax !0x-8
	pop____%edx
	add____%edx,%eax
	mov____%eax,0x8(%ebp) !0x-8
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0x-4
	add____$i8,(%eax) !0x1
	pop____%eax
	test___%eax,%eax
:_abtoi_50_continue
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	push___%eax
	call32 %isnumber
	add____$i8,%esp !0x8
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	je32   %_abtoi_50_break
	jmp32  %_abtoi_50_loop
:_abtoi_50_break
					# *p = s
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	mov____%eax,%edx
	pop____%eax
	mov____%eax,(%edx)
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x-8
	push___%eax
	mov____0x8(%ebp),%eax !0x-c
	mov____%eax,%edx
	pop____%eax
	mul____%edx
	leave
	ret


:atoi
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x8
	mov____%eax,0x8(%ebp) !0x-4
	mov____$i32,%eax %0x0
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0x-4
	push___%eax
	call32 %abtoi
	add____$i8,%esp !0x8
	leave
	ret


:number_to_ascii
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____$i32,%eax &number_to_ascii-itoa_buf
	add____$i8,%eax !0xb
	mov____%eax,0x8(%ebp) !0x-4
					# *p = 0
	mov____$i32,%eax %0x0
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	pop____%eax
	mov____%al,(%edx)
	add____$i8,0x8(%ebp) !0x-4 !0x-1
	test___%eax,%eax
	mov____$i32,%eax %0x0
	mov____%eax,0x8(%ebp) !0x-8
	mov____0x8(%ebp),%eax !0x8
	mov____%eax,0x8(%ebp) !0x-c
					# if (signed_p && x < 0) ...
	mov____0x8(%ebp),%eax !0x10
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	je32   %_number_to_ascii_17_break
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x0
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setl___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %_number_to_ascii_17_break
					# sign = 1
	mov____$i32,%eax %0x1
	mov____%eax,0x8(%ebp) !0x-8
	test___%eax,%eax
					# u = -x
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	mov____%eax,%edx
	pop____%eax
	mov____$i32,%eax %0x0
	sub____%edx,%eax
	mov____%eax,0x8(%ebp) !0x-c
	test___%eax,%eax
	jmp32  %_number_to_ascii_17_break
:_number_to_ascii_17_break
					# do uwhile (...); 
:_number_to_ascii_44_loop
	mov____0x8(%ebp),%eax !0x-c
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	mov____%eax,%edx
	pop____%eax
	mov____%edx,%ebx
	xor____%edx,%edx
	idiv___%ebx
	mov____%edx,%eax
	mov____%eax,0x8(%ebp) !0x-10
					# *p = i > 9? 'a' + i - 10: '0' + i
					# i > 9? ...: ...
	mov____0x8(%ebp),%eax !0x-10
	push___%eax
	mov____$i32,%eax %0x9
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setg___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %_number_to_ascii_53_else
	mov____$i32,%eax %0x61
	push___%eax
	mov____0x8(%ebp),%eax !0x-10
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	add____$i8,%eax !0x-a
	test___%eax,%eax
	jmp32  %_number_to_ascii_53_break
:_number_to_ascii_53_else
	mov____$i32,%eax %0x30
	push___%eax
	mov____0x8(%ebp),%eax !0x-10
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	test___%eax,%eax
:_number_to_ascii_53_break
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	pop____%eax
	mov____%al,(%edx)
	add____$i8,0x8(%ebp) !0x-4 !0x-1
	test___%eax,%eax
					# u = u/base
	mov____0x8(%ebp),%eax !0x-c
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	mov____%eax,%edx
	pop____%eax
	mov____%edx,%ebx
	xor____%edx,%edx
	idiv___%ebx
	mov____%eax,0x8(%ebp) !0x-c
	test___%eax,%eax
:_number_to_ascii_44_continue
	mov____0x8(%ebp),%eax !0x-c
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	je32   %_number_to_ascii_44_break
	jmp32  %_number_to_ascii_44_loop
:_number_to_ascii_44_break
					# if (sign && *(p + 1) != '0') ...
	mov____0x8(%ebp),%eax !0x-8
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	je32   %_number_to_ascii_100_break
	mov____0x8(%ebp),%eax !0x-4
	add____$i8,%eax !0x1
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	push___%eax
	mov____$i32,%eax %0x30
	mov____%eax,%edx
	pop____%eax
	push___%eax
	sub____%edx,%eax
	setne__%al
	movzbl_%al,%eax
	xchg___%eax,(%esp)
	sub____%edx,%eax
	lahf
	xor____$i8,%ah !0x40
	sahf
	pop____%eax
					# jmp test LABEL
	jne32  %_number_to_ascii_100_break
					# *p = '-'
	mov____$i32,%eax %0x2d
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	pop____%eax
	mov____%al,(%edx)
	add____$i8,0x8(%ebp) !0x-4 !0x-1
	test___%eax,%eax
	jmp32  %_number_to_ascii_100_break
:_number_to_ascii_100_break
	mov____0x8(%ebp),%eax !0x-4
	add____$i8,%eax !0x1
	leave
	ret


:itoab
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____$i32,%eax %0x1
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %number_to_ascii
	add____$i8,%esp !0xc
	leave
	ret


:itoa
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____$i32,%eax %0x1
	push___%eax
	mov____$i32,%eax %0xa
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %number_to_ascii
	add____$i8,%esp !0xc
	leave
	ret


:utoa
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____$i32,%eax %0x0
	push___%eax
	mov____$i32,%eax %0xa
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %number_to_ascii
	add____$i8,%esp !0xc
	leave
	ret


:fdgetc
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
					# if (_ungetc_pos == -1) ...else ...
	mov____0x32,%eax &_ungetc_pos
	push___%eax
	mov____$i32,%eax %0x-1
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_fdgetc_1_else
	mov____$i32,%eax %0x1
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0x-4
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %read
	add____$i8,%esp !0xc
	mov____%eax,0x8(%ebp) !0x-c
					# if (r < 1) ...
	mov____0x8(%ebp),%eax !0x-c
	push___%eax
	mov____$i32,%eax %0x1
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setl___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %_fdgetc_17_break
	mov____$i32,%eax %0x-1
	leave
	ret
	jmp32  %_fdgetc_17_break
:_fdgetc_17_break
					# i = c
	mov____0x8(%ebp),%eax !0x-4
	movsbl_%al,%eax
	mov____%eax,0x8(%ebp) !0x-8
	test___%eax,%eax
	jmp32  %_fdgetc_1_break
:_fdgetc_1_else
					# i = _ungetc_buf[_ungetc_pos]
	mov____0x32,%eax &_ungetc_pos
	mov____%eax,%edx
	push___%eax
	mov____$i32,%eax &_ungetc_buf
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	mov____%eax,0x8(%ebp) !0x-8
	test___%eax,%eax
					# if (_ungetc_fd != fd && i == 10) ...else ...
	mov____0x32,%eax &_ungetc_fd
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	mov____%eax,%edx
	pop____%eax
	push___%eax
	sub____%edx,%eax
	setne__%al
	movzbl_%al,%eax
	xchg___%eax,(%esp)
	sub____%edx,%eax
	lahf
	xor____$i8,%ah !0x40
	sahf
	pop____%eax
					# jmp test LABEL
	jne32  %_fdgetc_47_else
	mov____0x8(%ebp),%eax !0x-8
	push___%eax
	mov____$i32,%eax %0xa
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_fdgetc_47_else
					# _ungetc_pos = -1
	mov____$i32,%eax %0x-1
	mov____%eax,0x32 &_ungetc_pos
	test___%eax,%eax
					# _ungetc_fd = -1
	mov____$i32,%eax %0x-1
	mov____%eax,0x32 &_ungetc_fd
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %fdgetc
	add____$i8,%esp !0x4
	leave
	ret
	jmp32  %_fdgetc_47_break
:_fdgetc_47_else
					# if (_ungetc_fd != fd) ...
	mov____0x32,%eax &_ungetc_fd
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	mov____%eax,%edx
	pop____%eax
	push___%eax
	sub____%edx,%eax
	setne__%al
	movzbl_%al,%eax
	xchg___%eax,(%esp)
	sub____%edx,%eax
	lahf
	xor____$i8,%ah !0x40
	sahf
	pop____%eax
					# jmp test LABEL
	jne32  %_fdgetc_76_break
					# eputs(" ***MES LIB C*** fdgetc ungetc conflict unget-fd="); 
	push___$i32 &_string_lib/x86-mes/libc.S_1
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs(itoa(_ungetc_fd)); 
	mov____0x32,%eax &_ungetc_fd
	push___%eax
	call32 %itoa
	add____$i8,%esp !0x4
	push___%eax
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs(", fdgetc-fd="); 
	push___$i32 &_string_lib/x86-mes/libc.S_2
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs(itoa(fd)); 
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %itoa
	add____$i8,%esp !0x4
	push___%eax
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs(", c="); 
	push___$i32 &_string_lib/x86-mes/libc.S_3
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs(itoa(_ungetc_buf[_ungetc_pos])); 
	mov____0x32,%eax &_ungetc_pos
	mov____%eax,%edx
	push___%eax
	mov____$i32,%eax &_ungetc_buf
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	push___%eax
	call32 %itoa
	add____$i8,%esp !0x4
	push___%eax
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs("\n"); 
	push___$i32 &_string_lib/x86-mes/libc.S_0
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# exit(1); 
	mov____$i32,%eax %0x1
	push___%eax
	call32 %exit
	add____$i8,%esp !0x4
	test___%eax,%eax
	jmp32  %_fdgetc_76_break
:_fdgetc_76_break
:_fdgetc_47_break
					# i = _ungetc_buf[_ungetc_pos]
	mov____0x32,%eax &_ungetc_pos
	mov____%eax,%edx
	push___%eax
	mov____$i32,%eax &_ungetc_buf
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	mov____%eax,0x8(%ebp) !0x-8
	test___%eax,%eax
					# _ungetc_pos -= 1
	mov____$i32,%eax %0x1
	push___%eax
	mov____0x32,%eax &_ungetc_pos
	pop____%edx
	sub____%edx,%eax
	mov____%eax,0x32 &_ungetc_pos
	test___%eax,%eax
					# if (_ungetc_pos == -1) ...
	mov____0x32,%eax &_ungetc_pos
	push___%eax
	mov____$i32,%eax %0x-1
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_fdgetc_155_break
					# _ungetc_fd = -1
	mov____$i32,%eax %0x-1
	mov____%eax,0x32 &_ungetc_fd
	test___%eax,%eax
	jmp32  %_fdgetc_155_break
:_fdgetc_155_break
:_fdgetc_1_break
					# if (i < 0) ...
	mov____0x8(%ebp),%eax !0x-8
	push___%eax
	mov____$i32,%eax %0x0
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setl___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %_fdgetc_170_break
					# i += 256
	mov____$i32,%eax %0x100
	push___%eax
	mov____0x8(%ebp),%eax !0x-8
	pop____%edx
	add____%edx,%eax
	mov____%eax,0x8(%ebp) !0x-8
	test___%eax,%eax
	jmp32  %_fdgetc_170_break
:_fdgetc_170_break
	mov____0x8(%ebp),%eax !0x-8
	leave
	ret


:fdputc
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
					# write(fd, (char*)&c, 1); 
	mov____$i32,%eax %0x1
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0x8
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	call32 %write
	add____$i8,%esp !0xc
	test___%eax,%eax
	mov____$i32,%eax %0x0
	leave
	ret


:fdputs
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %strlen
	add____$i8,%esp !0x4
	mov____%eax,0x8(%ebp) !0x-4
					# write(fd, s, i); 
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	call32 %write
	add____$i8,%esp !0xc
	test___%eax,%eax
	mov____$i32,%eax %0x0
	leave
	ret


:fdungetc
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
					# if (c == -1) ...
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x-1
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_fdungetc_1_break
	mov____0x8(%ebp),%eax !0x8
	leave
	ret
	jmp32  %_fdungetc_1_break
:_fdungetc_1_break
					# if (_ungetc_pos == -1) ...else ...
	mov____0x32,%eax &_ungetc_pos
	push___%eax
	mov____$i32,%eax %0x-1
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_fdungetc_13_else
					# _ungetc_fd = fd
	mov____0x8(%ebp),%eax !0xc
	mov____%eax,0x32 &_ungetc_fd
	test___%eax,%eax
	jmp32  %_fdungetc_13_break
:_fdungetc_13_else
					# if (_ungetc_fd != fd) ...
	mov____0x32,%eax &_ungetc_fd
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	mov____%eax,%edx
	pop____%eax
	push___%eax
	sub____%edx,%eax
	setne__%al
	movzbl_%al,%eax
	xchg___%eax,(%esp)
	sub____%edx,%eax
	lahf
	xor____$i8,%ah !0x40
	sahf
	pop____%eax
					# jmp test LABEL
	jne32  %_fdungetc_27_break
					# eputs(" ***MES LIB C*** fdungetc ungetc conflict unget-fd="); 
	push___$i32 &_string_lib/x86-mes/libc.S_4
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs(itoa(_ungetc_fd)); 
	mov____0x32,%eax &_ungetc_fd
	push___%eax
	call32 %itoa
	add____$i8,%esp !0x4
	push___%eax
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs(", fdungetc-fd="); 
	push___$i32 &_string_lib/x86-mes/libc.S_5
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs(itoa(fd)); 
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	call32 %itoa
	add____$i8,%esp !0x4
	push___%eax
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs("\n"); 
	push___$i32 &_string_lib/x86-mes/libc.S_0
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# exit(1); 
	mov____$i32,%eax %0x1
	push___%eax
	call32 %exit
	add____$i8,%esp !0x4
	test___%eax,%eax
	jmp32  %_fdungetc_27_break
:_fdungetc_27_break
:_fdungetc_13_break
	mov____0x32,%eax &_ungetc_pos
	push___%eax
	mov____$i32,%eax &_ungetc_pos
	add____$i8,(%eax) !0x1
	pop____%eax
	test___%eax,%eax
					# _ungetc_buf[_ungetc_pos] = c
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____0x32,%eax &_ungetc_pos
	mov____%eax,%edx
	push___%eax
	mov____$i32,%eax &_ungetc_buf
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	mov____%eax,%edx
	pop____%eax
	mov____%al,(%edx)
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x8
	leave
	ret


:_fdungetc_p
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x32,%eax &_ungetc_pos
	push___%eax
	mov____$i32,%eax %0x-1
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setg___%al
	movzbl_%al,%eax
	test___%eax,%eax
	leave
	ret


:eputc
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____$i32,%eax %0x2
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %fdputc
	add____$i8,%esp !0x8
	leave
	ret


:__sys_call
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !8
	int____$0x80
	leave
	ret


:__sys_call1
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !8
	mov____0x8(%ebp),%ebx !12
	int____$0x80
	leave
	ret


:__sys_call2
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !8
	mov____0x8(%ebp),%ebx !12
	mov____0x8(%ebp),%ecx !16
	int____$0x80
	leave
	ret


:__sys_call3
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !8
	mov____0x8(%ebp),%ebx !12
	mov____0x8(%ebp),%ecx !16
	mov____0x8(%ebp),%edx !20
	int____$0x80
	leave
	ret


:__sys_call4
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !8
	mov____0x8(%ebp),%ebx !12
	mov____0x8(%ebp),%ecx !16
	mov____0x8(%ebp),%edx !20
	mov____0x8(%ebp),%esi !24
	int____$0x80
	leave
	ret


:_sys_call
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %__sys_call
	add____$i8,%esp !0x4
	mov____%eax,0x8(%ebp) !0x-4
					# if (r < 0) ...else ...
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____$i32,%eax %0x0
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setl___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %__sys_call_5_else
					# errno = -r
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	pop____%eax
	mov____$i32,%eax %0x0
	sub____%edx,%eax
	mov____%eax,0x32 &errno
	test___%eax,%eax
					# r = -1
	mov____$i32,%eax %0x-1
	mov____%eax,0x8(%ebp) !0x-4
	test___%eax,%eax
	jmp32  %__sys_call_5_break
:__sys_call_5_else
					# errno = 0
	mov____$i32,%eax %0x0
	mov____%eax,0x32 &errno
	test___%eax,%eax
:__sys_call_5_break
	mov____0x8(%ebp),%eax !0x-4
	leave
	ret


:_sys_call1
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %__sys_call1
	add____$i8,%esp !0x8
	mov____%eax,0x8(%ebp) !0x-4
					# if (r < 0) ...else ...
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____$i32,%eax %0x0
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setl___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %__sys_call1_7_else
					# errno = -r
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	pop____%eax
	mov____$i32,%eax %0x0
	sub____%edx,%eax
	mov____%eax,0x32 &errno
	test___%eax,%eax
					# r = -1
	mov____$i32,%eax %0x-1
	mov____%eax,0x8(%ebp) !0x-4
	test___%eax,%eax
	jmp32  %__sys_call1_7_break
:__sys_call1_7_else
					# errno = 0
	mov____$i32,%eax %0x0
	mov____%eax,0x32 &errno
	test___%eax,%eax
:__sys_call1_7_break
	mov____0x8(%ebp),%eax !0x-4
	leave
	ret


:_sys_call2
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x10
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %__sys_call2
	add____$i8,%esp !0xc
	mov____%eax,0x8(%ebp) !0x-4
					# if (r < 0) ...else ...
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____$i32,%eax %0x0
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setl___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %__sys_call2_9_else
					# errno = -r
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	pop____%eax
	mov____$i32,%eax %0x0
	sub____%edx,%eax
	mov____%eax,0x32 &errno
	test___%eax,%eax
					# r = -1
	mov____$i32,%eax %0x-1
	mov____%eax,0x8(%ebp) !0x-4
	test___%eax,%eax
	jmp32  %__sys_call2_9_break
:__sys_call2_9_else
					# errno = 0
	mov____$i32,%eax %0x0
	mov____%eax,0x32 &errno
	test___%eax,%eax
:__sys_call2_9_break
	mov____0x8(%ebp),%eax !0x-4
	leave
	ret


:_sys_call3
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x14
	push___%eax
	mov____0x8(%ebp),%eax !0x10
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %__sys_call3
	add____$i8,%esp !0x10
	mov____%eax,0x8(%ebp) !0x-4
					# if (r < 0) ...else ...
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____$i32,%eax %0x0
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setl___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %__sys_call3_11_else
					# errno = -r
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	pop____%eax
	mov____$i32,%eax %0x0
	sub____%edx,%eax
	mov____%eax,0x32 &errno
	test___%eax,%eax
					# r = -1
	mov____$i32,%eax %0x-1
	mov____%eax,0x8(%ebp) !0x-4
	test___%eax,%eax
	jmp32  %__sys_call3_11_break
:__sys_call3_11_else
					# errno = 0
	mov____$i32,%eax %0x0
	mov____%eax,0x32 &errno
	test___%eax,%eax
:__sys_call3_11_break
	mov____0x8(%ebp),%eax !0x-4
	leave
	ret


:_sys_call4
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x18
	push___%eax
	mov____0x8(%ebp),%eax !0x14
	push___%eax
	mov____0x8(%ebp),%eax !0x10
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %__sys_call4
	add____$i8,%esp !0x14
	mov____%eax,0x8(%ebp) !0x-4
					# if (r < 0) ...else ...
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____$i32,%eax %0x0
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setl___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %__sys_call4_13_else
					# errno = -r
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	pop____%eax
	mov____$i32,%eax %0x0
	sub____%edx,%eax
	mov____%eax,0x32 &errno
	test___%eax,%eax
					# r = -1
	mov____$i32,%eax %0x-1
	mov____%eax,0x8(%ebp) !0x-4
	test___%eax,%eax
	jmp32  %__sys_call4_13_break
:__sys_call4_13_else
					# errno = 0
	mov____$i32,%eax %0x0
	mov____%eax,0x32 &errno
	test___%eax,%eax
:__sys_call4_13_break
	mov____0x8(%ebp),%eax !0x-4
	leave
	ret


:fork
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____$i32,%eax %0x2
	push___%eax
	call32 %_sys_call
	add____$i8,%esp !0x4
	leave
	ret


:read
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x10
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x3
	push___%eax
	call32 %_sys_call3
	add____$i8,%esp !0x10
	mov____%eax,0x8(%ebp) !0x-4
					# if (__mes_debug() > 3) ...
	call32 %__mes_debug
	add____$i8,%esp !0x0
	push___%eax
	mov____$i32,%eax %0x3
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setg___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %_read_11_break
					# if (bytes == 1) ...else ...
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____$i32,%eax %0x1
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_read_19_else
					# eputs("read fd="); 
	push___$i32 &_string_lib/x86-mes/libc.S_6
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs(itoa((int)filedes)); 
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %itoa
	add____$i8,%esp !0x4
	push___%eax
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs(" c="); 
	push___$i32 &_string_lib/x86-mes/libc.S_7
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputc(*((char*)buffer)); 
	mov____0x8(%ebp),%eax !0xc
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	push___%eax
	call32 %eputc
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs("\n"); 
	push___$i32 &_string_lib/x86-mes/libc.S_0
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
	jmp32  %_read_19_break
:_read_19_else
					# eputs("read fd="); 
	push___$i32 &_string_lib/x86-mes/libc.S_6
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs(itoa((int)filedes)); 
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %itoa
	add____$i8,%esp !0x4
	push___%eax
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs(" bytes="); 
	push___$i32 &_string_lib/x86-mes/libc.S_8
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs(itoa(bytes)); 
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	call32 %itoa
	add____$i8,%esp !0x4
	push___%eax
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs("\n"); 
	push___$i32 &_string_lib/x86-mes/libc.S_0
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
:_read_19_break
	jmp32  %_read_11_break
:_read_11_break
	mov____0x8(%ebp),%eax !0x-4
	leave
	ret


:open
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
					# ap = (char*)(&flags + 1)
	mov____%ebp,%eax
	add____$i8,%eax !0xc
	add____$i8,%eax !0x4
	mov____%eax,0x8(%ebp) !0x-4
	movzbl_%al,%eax
	test___%eax,%eax
	mov____$i32,%eax %0x-1
	shl____$i8,%eax !0x2
	push___%eax
					# ap = ap + 4
	mov____0x8(%ebp),%eax !0x-4
	add____$i8,%eax !0x4
	mov____%eax,0x8(%ebp) !0x-4
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	mov____(%eax),%eax
	mov____%eax,0x8(%ebp) !0x-8
					# if (!flags) ...
	mov____0x8(%ebp),%eax !0xc
	test___%eax,%eax
	sete___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %_open_18_break
					# _ungetc_pos = -1
	mov____$i32,%eax %0x-1
	mov____%eax,0x32 &_ungetc_pos
	test___%eax,%eax
					# _ungetc_fd = -1
	mov____$i32,%eax %0x-1
	mov____%eax,0x32 &_ungetc_fd
	test___%eax,%eax
	jmp32  %_open_18_break
:_open_18_break
	mov____0x8(%ebp),%eax !0x-8
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x5
	push___%eax
	call32 %_sys_call3
	add____$i8,%esp !0x10
	mov____%eax,0x8(%ebp) !0x-c
					# ap = 0
	mov____$i32,%eax %0x0
	mov____%eax,0x8(%ebp) !0x-4
	movzbl_%al,%eax
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x-c
	leave
	ret


:waitpid
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x10
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x7
	push___%eax
	call32 %_sys_call3
	add____$i8,%esp !0x10
	leave
	ret


:execve
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x10
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0xb
	push___%eax
	call32 %_sys_call3
	add____$i8,%esp !0x10
	leave
	ret


:chmod
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0xf
	push___%eax
	call32 %_sys_call2
	add____$i8,%esp !0xc
	leave
	ret


:access
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x21
	push___%eax
	call32 %_sys_call2
	add____$i8,%esp !0xc
	leave
	ret


:brk
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x2d
	push___%eax
	call32 %_sys_call1
	add____$i8,%esp !0x8
	leave
	ret


:ioctl
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
					# ap = (char*)(&command + 1)
	mov____%ebp,%eax
	add____$i8,%eax !0xc
	add____$i8,%eax !0x4
	mov____%eax,0x8(%ebp) !0x-4
	movzbl_%al,%eax
	test___%eax,%eax
	mov____$i32,%eax %0x-1
	shl____$i8,%eax !0x2
	push___%eax
					# ap = ap + 4
	mov____0x8(%ebp),%eax !0x-4
	add____$i8,%eax !0x4
	mov____%eax,0x8(%ebp) !0x-4
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	mov____(%eax),%eax
	mov____%eax,0x8(%ebp) !0x-8
	mov____0x8(%ebp),%eax !0x-8
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x36
	push___%eax
	call32 %_sys_call3
	add____$i8,%esp !0x10
	mov____%eax,0x8(%ebp) !0x-c
					# ap = 0
	mov____$i32,%eax %0x0
	mov____%eax,0x8(%ebp) !0x-4
	movzbl_%al,%eax
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x-c
	leave
	ret


:fsync
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x76
	push___%eax
	call32 %_sys_call1
	add____$i8,%esp !0x8
	leave
	ret


:__mes_debug
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
					# if (__mes_debug == -1) ...
	mov____0x32,%eax &__mes_debug-__mes_debug
	push___%eax
	mov____$i32,%eax %0x-1
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %___mes_debug_1_break
	push___$i32 &_string_lib/x86-mes/libc.S_9
	call32 %getenv
	add____$i8,%esp !0x4
	mov____%eax,0x8(%ebp) !0x-4
					# __mes_debug = p? atoi(p) > 1? atoi(p): 1: 0
					# p? ...: ...
	mov____0x8(%ebp),%eax !0x-4
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	je32   %___mes_debug_13_else
					# atoi(p) > 1? ...: ...
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	call32 %atoi
	add____$i8,%esp !0x4
	push___%eax
	mov____$i32,%eax %0x1
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	setg___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %___mes_debug_19_else
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	call32 %atoi
	add____$i8,%esp !0x4
	test___%eax,%eax
	jmp32  %___mes_debug_19_break
:___mes_debug_19_else
	mov____$i32,%eax %0x1
	test___%eax,%eax
:___mes_debug_19_break
	test___%eax,%eax
	jmp32  %___mes_debug_13_break
:___mes_debug_13_else
	mov____$i32,%eax %0x0
	test___%eax,%eax
:___mes_debug_13_break
	mov____%eax,0x32 &__mes_debug-__mes_debug
	test___%eax,%eax
	jmp32  %___mes_debug_1_break
:___mes_debug_1_break
	mov____0x32,%eax &__mes_debug-__mes_debug
	leave
	ret


:memcpy
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x8
	mov____%eax,0x8(%ebp) !0x-4
	mov____0x8(%ebp),%eax !0xc
	mov____%eax,0x8(%ebp) !0x-8
					# while (n--) ...
	jmp32  %_memcpy_5_continue
:_memcpy_5_loop
					# *p = *q++
	mov____0x8(%ebp),%eax !0x-8
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0x-8
	add____$i8,(%eax) !0x1
	pop____%eax
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	pop____%eax
	mov____%al,(%edx)
	add____$i8,0x8(%ebp) !0x-4 !0x1
	test___%eax,%eax
:_memcpy_5_continue
	mov____0x8(%ebp),%eax !0x10
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0x10
	add____$i8,(%eax) !0x-1
	pop____%eax
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	je32   %_memcpy_5_break
	jmp32  %_memcpy_5_loop
:_memcpy_5_break
	mov____0x8(%ebp),%eax !0x8
	leave
	ret


:malloc
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
					# if (!__brk) ...
	mov____0x32,%eax &__brk
	test___%eax,%eax
	sete___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %_malloc_1_break
					# __brk = brk(0)
	mov____$i32,%eax %0x0
	push___%eax
	call32 %brk
	add____$i8,%esp !0x4
	mov____%eax,0x32 &__brk
	test___%eax,%eax
	jmp32  %_malloc_1_break
:_malloc_1_break
					# if (brk(__brk + size) == (void*)-1) ...
	mov____0x32,%eax &__brk
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	push___%eax
	call32 %brk
	add____$i8,%esp !0x4
	push___%eax
	mov____$i32,%eax %0x-1
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_malloc_16_break
	mov____$i32,%eax %0x0
	leave
	ret
	jmp32  %_malloc_16_break
:_malloc_16_break
	mov____0x32,%eax &__brk
	mov____%eax,0x8(%ebp) !0x-4
					# __brk += size
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____0x32,%eax &__brk
	pop____%edx
	add____%edx,%eax
	mov____%eax,0x32 &__brk
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x-4
	leave
	ret


:assert_fail
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
					# eputs("assert fail: "); 
	push___$i32 &_string_lib/x86-mes/libc.S_10
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs(s); 
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
					# eputs("\n"); 
	push___$i32 &_string_lib/x86-mes/libc.S_0
	call32 %eputs
	add____$i8,%esp !0x4
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x8
	mov____%eax,0x8(%ebp) !0x-4
					# fail = 0
	mov____$i32,%eax %0x0
	mov____%eax,0x8(%ebp) !0x-4
	test___%eax,%eax
					# *fail = 0
	mov____$i32,%eax %0x0
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	pop____%eax
	mov____%al,(%edx)
	test___%eax,%eax
	leave
	ret


:getchar
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x32,%eax &g_stdin
	push___%eax
	call32 %fdgetc
	add____$i8,%esp !0x4
	leave
	ret


:putchar
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
					# write(1, (char*)&c, 1); 
	mov____$i32,%eax %0x1
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0x8
	push___%eax
	mov____$i32,%eax %0x1
	push___%eax
	call32 %write
	add____$i8,%esp !0xc
	test___%eax,%eax
	mov____$i32,%eax %0x0
	leave
	ret


:fputc
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %fdputc
	add____$i8,%esp !0x8
	leave
	ret


:fputs
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %fdputs
	add____$i8,%esp !0x8
	leave
	ret


:putc
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %fdputc
	add____$i8,%esp !0x8
	leave
	ret


:getc
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %fdgetc
	add____$i8,%esp !0x4
	leave
	ret


:fgetc
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %fdgetc
	add____$i8,%esp !0x4
	leave
	ret


:free
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	leave
	ret


:ungetc
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %fdungetc
	add____$i8,%esp !0x8
	leave
	ret


:strcmp
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
					# while (*a && *b && *a == *b) ...
	jmp32  %_strcmp_1_continue
:_strcmp_1_loop
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0x8
	add____$i8,(%eax) !0x1
	pop____%eax
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0xc
	add____$i8,(%eax) !0x1
	pop____%eax
	test___%eax,%eax
:_strcmp_1_continue
	mov____0x8(%ebp),%eax !0x8
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	test___%al,%al
	je32   %_strcmp_1_break
	mov____0x8(%ebp),%eax !0xc
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	test___%al,%al
	je32   %_strcmp_1_break
	mov____0x8(%ebp),%eax !0x8
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_strcmp_1_break
	jmp32  %_strcmp_1_loop
:_strcmp_1_break
	mov____0x8(%ebp),%eax !0x8
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	leave
	ret


:strcpy
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0x8
	mov____%eax,0x8(%ebp) !0x-4
					# while (*src) ...
	jmp32  %_strcpy_3_continue
:_strcpy_3_loop
					# *p = *src++
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0xc
	add____$i8,(%eax) !0x1
	pop____%eax
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	pop____%eax
	mov____%al,(%edx)
	add____$i8,0x8(%ebp) !0x-4 !0x1
	test___%eax,%eax
:_strcpy_3_continue
	mov____0x8(%ebp),%eax !0xc
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	test___%al,%al
	je32   %_strcpy_3_break
	jmp32  %_strcpy_3_loop
:_strcpy_3_break
					# *p = 0
	mov____$i32,%eax %0x0
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	pop____%eax
	mov____%al,(%edx)
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0x8
	leave
	ret


:realloc
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	call32 %malloc
	add____$i8,%esp !0x4
	mov____%eax,0x8(%ebp) !0x-4
					# if (ptr && new) ...
	mov____0x8(%ebp),%eax !0x8
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	je32   %_realloc_5_break
	mov____0x8(%ebp),%eax !0x-4
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	je32   %_realloc_5_break
					# memcpy(new, ptr, size); 
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	call32 %memcpy
	add____$i8,%esp !0xc
	test___%eax,%eax
					# free(ptr); 
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %free
	add____$i8,%esp !0x4
	test___%eax,%eax
	jmp32  %_realloc_5_break
:_realloc_5_break
	mov____0x8(%ebp),%eax !0x-4
	leave
	ret


:strncmp
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
					# if (!size) ...
	mov____0x8(%ebp),%eax !0x10
	test___%eax,%eax
	sete___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %_strncmp_1_break
	mov____$i32,%eax %0x0
	leave
	ret
	jmp32  %_strncmp_1_break
:_strncmp_1_break
					# while (*a && *b && *a == *b && --size) ...
	jmp32  %_strncmp_12_continue
:_strncmp_12_loop
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0x8
	add____$i8,(%eax) !0x1
	pop____%eax
	test___%eax,%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0xc
	add____$i8,(%eax) !0x1
	pop____%eax
	test___%eax,%eax
:_strncmp_12_continue
	mov____0x8(%ebp),%eax !0x8
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	test___%al,%al
	je32   %_strncmp_12_break
	mov____0x8(%ebp),%eax !0xc
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	test___%al,%al
	je32   %_strncmp_12_break
	mov____0x8(%ebp),%eax !0x8
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_strncmp_12_break
	mov____%ebp,%eax
	add____$i8,%eax !0x10
	add____$i8,(%eax) !0x-1
	mov____0x8(%ebp),%eax !0x10
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	je32   %_strncmp_12_break
	jmp32  %_strncmp_12_loop
:_strncmp_12_break
	mov____0x8(%ebp),%eax !0x8
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	leave
	ret


:getenv
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x32,%eax &environ
	mov____%eax,0x8(%ebp) !0x-4
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %strlen
	add____$i8,%esp !0x4
	mov____%eax,0x8(%ebp) !0x-8
					# while (*p) ...
	jmp32  %_getenv_7_continue
:_getenv_7_loop
					# if (!strncmp(s, *p, length) && *(*p + length) == '=') ...
	mov____0x8(%ebp),%eax !0x-8
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____(%eax),%eax
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %strncmp
	add____$i8,%esp !0xc
	test___%eax,%eax
	sete___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %_getenv_10_break
	mov____0x8(%ebp),%eax !0x-4
	mov____(%eax),%eax
	push___%eax
	mov____0x8(%ebp),%eax !0x-8
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	push___%eax
	mov____$i32,%eax %0x3d
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_getenv_10_break
	mov____0x8(%ebp),%eax !0x-4
	mov____(%eax),%eax
	push___%eax
	mov____0x8(%ebp),%eax !0x-8
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	add____$i8,%eax !0x1
	leave
	ret
	jmp32  %_getenv_10_break
:_getenv_10_break
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0x-4
	add____$i8,(%eax) !0x4
	pop____%eax
	test___%eax,%eax
:_getenv_7_continue
	mov____0x8(%ebp),%eax !0x-4
	mov____(%eax),%eax
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	je32   %_getenv_7_break
	jmp32  %_getenv_7_loop
:_getenv_7_break
	mov____$i32,%eax %0x0
	leave
	ret


:setenv
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x32,%eax &environ
	mov____%eax,0x8(%ebp) !0x-4
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %strlen
	add____$i8,%esp !0x4
	mov____%eax,0x8(%ebp) !0x-8
					# while (*p) ...
	jmp32  %_setenv_7_continue
:_setenv_7_loop
					# if (!strncmp(s, *p, length) && *(*p + length) == '=') ...
	mov____0x8(%ebp),%eax !0x-8
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____(%eax),%eax
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %strncmp
	add____$i8,%esp !0xc
	test___%eax,%eax
	sete___%al
	movzbl_%al,%eax
	test___%eax,%eax
					# jmp test LABEL
	je32   %_setenv_10_break
	mov____0x8(%ebp),%eax !0x-4
	mov____(%eax),%eax
	push___%eax
	mov____0x8(%ebp),%eax !0x-8
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	movzbl_(%eax),%eax
	movsbl_%al,%eax
	push___%eax
	mov____$i32,%eax %0x3d
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
					# jmp test LABEL
	jne32  %_setenv_10_break
	jmp32  %_setenv_7_break
	jmp32  %_setenv_10_break
:_setenv_10_break
	mov____0x8(%ebp),%eax !0x-4
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0x-4
	add____$i8,(%eax) !0x4
	pop____%eax
	test___%eax,%eax
:_setenv_7_continue
	mov____0x8(%ebp),%eax !0x-4
	mov____(%eax),%eax
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	je32   %_setenv_7_break
	jmp32  %_setenv_7_loop
:_setenv_7_break
	mov____0x8(%ebp),%eax !0x-8
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	call32 %strlen
	add____$i8,%esp !0x4
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	add____$i8,%eax !0x2
	push___%eax
	call32 %malloc
	add____$i8,%esp !0x4
	mov____%eax,0x8(%ebp) !0x-c
	mov____0x8(%ebp),%eax !0x-4
	mov____(%eax),%eax
	push___%eax
	mov____$i32,%eax %0x0
	mov____%eax,%edx
	pop____%eax
	sub____%edx,%eax
	sete___%al
	movzbl_%al,%eax
	mov____%eax,0x8(%ebp) !0x-10
					# *p = entry
	mov____0x8(%ebp),%eax !0x-c
	push___%eax
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	pop____%eax
	mov____%eax,(%edx)
	test___%eax,%eax
					# strcpy(entry, s); 
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____0x8(%ebp),%eax !0x-c
	push___%eax
	call32 %strcpy
	add____$i8,%esp !0x8
	test___%eax,%eax
					# strcpy(entry + length, "="); 
	push___$i32 &_string_lib/x86-mes/libc.S_11
	mov____0x8(%ebp),%eax !0x-c
	push___%eax
	mov____0x8(%ebp),%eax !0x-8
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	push___%eax
	call32 %strcpy
	add____$i8,%esp !0x8
	test___%eax,%eax
					# strcpy(entry + length + 1, v); 
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x-c
	push___%eax
	mov____0x8(%ebp),%eax !0x-8
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	add____$i8,%eax !0x1
	push___%eax
	call32 %strcpy
	add____$i8,%esp !0x8
	test___%eax,%eax
					# *(entry + length + strlen(v) + 2) = 0
	mov____$i32,%eax %0x0
	push___%eax
	mov____0x8(%ebp),%eax !0x-c
	push___%eax
	mov____0x8(%ebp),%eax !0x-8
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	call32 %strlen
	add____$i8,%esp !0x4
	mov____%eax,%edx
	pop____%eax
	add____%edx,%eax
	add____$i8,%eax !0x2
	mov____%eax,%edx
	pop____%eax
	mov____%al,(%edx)
	test___%eax,%eax
					# if (end_p) ...
	mov____0x8(%ebp),%eax !0x-10
	test___%eax,%eax
					# jmp test LABEL
	test___%eax,%eax
	je32   %_setenv_130_break
					# *++p = 0
	mov____$i32,%eax %0x0
	push___%eax
	mov____%ebp,%eax
	add____$i8,%eax !0x-4
	add____$i8,(%eax) !0x4
	mov____0x8(%ebp),%eax !0x-4
	mov____%eax,%edx
	pop____%eax
	mov____%eax,(%edx)
	test___%eax,%eax
	jmp32  %_setenv_130_break
:_setenv_130_break
	mov____$i32,%eax %0x0
	leave
	ret


:isatty
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____$i32,%eax %0x0
	push___%eax
	mov____$i32,%eax %0x5401
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %ioctl
	add____$i8,%esp !0xc
	push___%eax
	mov____$i32,%eax %0xf0
	mov____%eax,%edx
	pop____%eax
	and____%edx,%eax
	leave
	ret


:wait
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____$i32,%eax %0x0
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	mov____$i32,%eax %0x-1
	push___%eax
	call32 %waitpid
	add____$i8,%esp !0xc
	leave
	ret


:execv
	push___%ebp
	mov____%esp,%ebp
	sub____%esp,$i32 %0x1054
	mov____0x32,%eax &environ
	push___%eax
	mov____0x8(%ebp),%eax !0xc
	push___%eax
	mov____0x8(%ebp),%eax !0x8
	push___%eax
	call32 %execve
	add____$i8,%esp !0xc
	leave
	ret

:__mes_debug-__mes_debug
!0x-1 !0x-1 !0x-1 !0x-1

:number_to_ascii-itoa_buf
!0x00 !0x00 !0x00 !0x00 !0x00 !0x00 !0x00 !0x00 !0x00 !0x00 !0x00 !0x00

:comparison_fn_t
!0x00 !0x00 !0x00 !0x00

:environ
!0x00 !0x00 !0x00 !0x00

:g_stdin
!0x00 !0x00 !0x00 !0x00

:g_stdout
!0x00 !0x00 !0x00 !0x00

:errno
!0x00 !0x00 !0x00 !0x00

:_string_lib/x86-mes/libc.S_0
"
" !0x00 !0x00

:__call_at_exit
!0x00 !0x00 !0x00 !0x00

:_ungetc_pos
!0x-1 !0x-1 !0x-1 !0x-1

:_ungetc_fd
!0x-1 !0x-1 !0x-1 !0x-1

:_ungetc_buf
!0x00 !0x00 !0x00 !0x00 !0x00 !0x00 !0x00 !0x00 !0x00 !0x00 !0x00 !0x00

:_string_lib/x86-mes/libc.S_1
" ***MES LIB C*** fdgetc ungetc conflict unget-fd=" !0x00 !0x00

:_string_lib/x86-mes/libc.S_2
", fdgetc-fd=" !0x00 !0x00 !0x00

:_string_lib/x86-mes/libc.S_3
", c=" !0x00 !0x00 !0x00

:_string_lib/x86-mes/libc.S_4
" ***MES LIB C*** fdungetc ungetc conflict unget-fd="

:_string_lib/x86-mes/libc.S_5
", fdungetc-fd=" !0x00

:_string_lib/x86-mes/libc.S_6
"read fd=" !0x00 !0x00 !0x00

:_string_lib/x86-mes/libc.S_7
" c="

:_string_lib/x86-mes/libc.S_8
" bytes="

:g_stdin
!0x00 !0x00 !0x00 !0x00

:_string_lib/x86-mes/libc.S_9
"MES_DEBUG" !0x00 !0x00

:__brk
!0x00 !0x00 !0x00 !0x00

:_string_lib/x86-mes/libc.S_10
"assert fail: " !0x00 !0x00

:_string_lib/x86-mes/libc.S_11
"=" !0x00 !0x00
