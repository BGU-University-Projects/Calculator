section	.rodata			; we define (global) read-only variables in .rodata section
	format_stringNEWLINE: db "%s", 10, 0	; format string
	format_string: db "%s", 0	; format string

section	.data
	size: db 5	
	curpos:db 0
	deg: db 0	
	NumOfOp: db 0	
	counterNode: db 0
	inputcheck: db 0
	newval: db 0
	goodbye: db "TOTAL OPS: ",0
	lengoodbye:  equ $ - goodbye
	plsinputmsg: db "calc: ",0
	lenplsinputmsg:  equ $ - plsinputmsg
	errOverFlow: db "Error: Operand Stack Overflow",10,0
	lenOverFlow: equ $ - errOverFlow 
	errPlus: db "Error: '+' Needs At Least 2 Numbers",10,0
	lenerrPlus: equ $ - errPlus 
	nonumbersmsg: db "Error:No Numbers In The Stack",10,0
	lennonumbers: equ $ - nonumbersmsg 
	errInsufficient : db "Error: Insufficient Number of Arguments on Stack",10,0
	lenInsufficient : equ $ - errInsufficient 
	result: db 0
	side: db 0
	lenan: db 0
	node1: dd 0
	node2: dd 0	
	deluntil: dd 0	
	tmp:  dd 0
	num1: dd 0
    num2: dd 0
    pow:  dd 0
	rlyinput: dd 0


section .bss			; we define (global) uninitialized variables in .bss section
	an: resb 80		; enough to store integer in [-2,147,483,648 (-2^31) : 2,147,483,647 (2^31-1)]
	mystack: RESD 1
	newNode: RESD 1
	input: RESD 1
	
	

section .text
  align 16
  global main
  extern printf
  extern fprintf 
  extern fflush
  extern malloc 
  extern calloc 
  extern free 
  extern gets 
  extern getchar 
  extern fgets 
system_call:
    push    ebp             ; Save caller state
    mov     ebp, esp
    sub     esp, 4          ; Leave space for local var on stack
    pushad                  ; Save some more caller state

    mov     eax, [ebp+8]    ; Copy function args to registers: leftmost...        
    mov     ebx, [ebp+12]   ; Next argument...
    mov     ecx, [ebp+16]   ; Next argument...
    mov     edx, [ebp+20]   ; Next argument...
    int     0x80            ; Transfer control to operating system
    mov     [ebp-4], eax    ; Save returned value...
    popad                   ; Restore caller state (registers)
    mov     eax, [ebp-4]    ; place returned value where caller can see it
    add     esp, 4          ; Restore caller state
    pop     ebp             ; Restore caller state
    ret                     ; Back to caller

%macro addNumber 0
	mov byte[counterNode],0
	%%.nextchar:
	movzx edx,byte[counterNode]
	add edx,[rlyinput]
	mov ecx,edx
	movzx ecx,byte[ecx]
	cmp ecx,10	;if its \n
	je %%.end_nextchar
	mov [newval],cl
	addNode
	inc byte[counterNode]
	jmp %%.nextchar
	%%.end_nextchar:
	mov byte[counterNode],0
%endmacro

%macro addNode 0		;this func do not update curpos
	;pushad
	movzx edx,byte[curpos]  
	mov eax,4
	mul edx
	mov ebx,dword[mystack]
	add ebx,eax
	%%.empty:
	push 5
	push 1
	call calloc
	mov edx,[newval]
	mov byte[eax],dl	;input value
	mov ecx,[ebx]
	mov dword[eax+1] ,ecx			;next
	movzx edx , byte[curpos]
	mov [ebx],eax
	pop edx
	pop edx
	;popad
%endmacro

%macro addNodeAtEnd 0	;return address in eax to new node with the value given by result vaalue
	;pushad
	push 5
	push 1
	call calloc
	movzx edx,byte[result]
	mov byte[eax],dl	;input value
	mov dword[eax+1] ,0			;next
	pop edx
	pop edx
	;popad
%endmacro

%macro add_func 0
	%%.B:		;make b normal
	mov ebx,[node1]
	movzx ebx,byte[ebx]
	cmp ebx,58
	jl %%.Bunder58
		sub ebx,55
		jmp %%.C
	%%.Bunder58:
		sub ebx,48
	%%.C:					;make c normal
	mov ecx,[node2]
	movzx ecx,byte[ecx]
	cmp ecx,58
	jl %%.Cunder58
		sub ecx,55
		jmp %%.calculate
	%%.Cunder58:
		sub ecx,48
	%%.calculate:
	add ebx,ecx
	movzx edx,byte[side]
	add ebx,edx 		;if theres Addition from before= side..
	mov byte[side],0
	cmp ebx,16
	jl %%.noAdd
	sub ebx,16
	inc byte[side]
	%%.noAdd:
	cmp ebx,10
		jB %%.dec
		jmp %%.hex
			%%.dec:
				add ebx,48
				jmp %%.after
			%%.hex:
				add ebx,55
				jmp %%.after
	%%.after:
%endmacro

%macro plus_func 0
    mov dword[deluntil],0 ;init deluntil
	mov byte[side],0	;make side zero before calc
	;mov byte[counterNode,0] ; make numnode 0 

	movzx edx,byte[curpos]  
	mov eax,4
	mul edx
	sub eax,4
	mov ebx,dword[mystack]
	add ebx,eax
	mov edx,[ebx]
	mov [node1],edx		;first node-node1
	sub ebx,4
	mov edx,[ebx]
	mov [node2],edx		;second node-node2
	%%.loop:
	add_func
	mov eax,[node2]
	mov byte[eax],bl		;update the result of add_func
	mov eax,[node1]
	mov ebx,dword[eax +1]		;ebx=next node of first
	mov eax,[node2]
	mov ecx,dword[eax +1]		;ecx=next node of second
	mov edx,0
	add edx,ebx
	add edx,ecx			;edx = sum of new values, if its 0 they both ended
	cmp edx,0
	je %%.endboth  
			;if we past here someone not finished...			
	cmp ebx,0		;if first node ended
		je %%.endFirst
	cmp ecx,0		;if second node ended
		je %%.endSecond
	mov [node1],ebx
	mov [node2],ecx
	jmp %%.loop			;if both not ended
						
	%%.endboth:
	movzx edx,byte[side]
	cmp edx,0
	je %%.end
	add edx,48
	mov [result],edx
	addNodeAtEnd 
	mov ecx,[node2]
	mov [ecx +1],eax
	jmp %%.end
	%%.endFirst:
		movzx edx,byte[side]	;add [side] to this node and finish
		add byte[ecx],dl
		cmp byte[ecx],71	;case just turned to 16=g
		jne %%.end
		mov byte[ecx],48
		mov byte[result],49
		cmp dword[ecx+1],0	;check if next
		je %%.nonext
		mov ecx,dword[ecx+1]	;make ecx his next and loop again
		jmp %%.endFirst
		;jmp %%.end
		%%.nonext:		;case no next
		mov [node2],ecx
		addNodeAtEnd
		mov ecx,[node2]
		mov [ecx +1],eax
		jmp %%.end
	%%.endSecond:
	mov [deluntil],ebx 	;deluntil is the adrres to delete until not include
	mov edx,[node2]
	mov dword[edx +1],ebx	;make node2 next to ebx=next node of First
	movzx edx,byte[side]	;add [side] to this node and finish
	mov ecx,ebx
	jmp %%.endFirst
	%%.end:
	deleteFirst
%endmacro

%macro deleteFirst 0
	movzx edx,byte[curpos]  
	mov eax,4
	mul edx
	sub eax,4
	mov ebx,dword[mystack]
	add ebx,eax
	mov eax,[ebx]		;eax=first node address
	freeNodeList
	%%.afterfree:
	movzx edx,byte[curpos]  
	mov eax,4
	mul edx
	sub eax,4
	%%.stophere:
	mov ebx,dword[mystack]
	add ebx,eax
	mov dword[ebx],0				;init the pointer in Array
	movzx edx,byte[curpos]
	sub edx,1
	mov byte[curpos],dl		;curpos-1
%endmacro

%macro freeNodeList 0		;recive address eax and del all the list until [deluntil]
	%%.startdel:
	cmp eax,[deluntil]		;if = [deluntil]
	je %%.enddel				
	cmp eax,0				;if = 0
	je %%.enddel				
	mov ebx,dword[eax+1]	;ebx =next node address
	push eax
	call free
	pop eax
	mov eax,ebx				;eax=next
	jmp %%.startdel
	%%.enddel:
%endmacro

%macro toPrint 0	;recive address eax and print all the list
	pushad
	mov ecx,0
	mov edx,0
	mov eax,an
	%%.init_an:
	cmp	ecx,80
	je %%.end_init_an
	add eax,ecx
	mov byte[eax],dl		;init [an]
	sub eax,ecx
	inc ecx
	jmp %%.init_an
	%%.end_init_an:
	movzx edx,byte[curpos]  
	mov eax,4
	mul edx
	sub eax,4
	mov ebx,dword[mystack]
	add ebx,eax
	mov eax,[ebx]		;eax=first node address
	mov ecx,0
	%%.startpush:
	cmp eax,0				;if = 0
	je %%.endpush				
	mov ebx,dword[eax+1]	;ebx =next node address
	movzx edx,byte[eax]		;edx=the value
	cmp edx,57				
	jle %%.decc		;if edx is number
		;sub edx,55
	jmp %%.rdy
	%%.decc:
		;sub edx,48
	%%.rdy:
	push edx
	inc ecx			;inc counter
	mov eax,ebx				;eax=next
	jmp %%.startpush
	%%.endpush:
	mov ebx,0
	mov eax,an
	cmp ecx,0
	je %%.endprint
	mov byte[counterNode],cl
	%%.makeAN:
	cmp ecx,0	;conterNode
	jE %%.printAN
		pop edx
		;add eax,ebx
		mov eax,an
		mov byte[eax],dl		;ebx counter chars, eax the number

	push eax	
	push format_string
	call printf			;print
	pop edx
	pop edx

		movzx ecx,byte[counterNode]
		sub ecx,1
		mov byte[counterNode],cl

	jmp %%.makeAN
	%%.printAN:
	;mov eax,an
	;push eax
	;push format_string
	;call printf			;print
	;pop edx
	;pop edx

		;mov	edx,1	  			;message length
		;mov	ecx,'\n'	    	;message to write
		;mov	ebx,1       		;file descriptor (stdout)
		;mov	eax,4       		;system call number (sys_write)
		;int	0x80        		;call kernel

	mov eax,an
	mov byte[eax],10		;ebx counter chars, eax the number
	push eax	
	push format_string
	call printf			;print
	pop edx
	pop edx

	mov dword[deluntil],0	;init [deluntil]
	deleteFirst			;should be on///////////////////////////////////////////////////////

	%%.endprint:
	popad
%endmacro

%macro dup 0	;dup the first number
				;firstNode
	movzx edx,byte[curpos]  
	mov eax,4
	mul edx
	mov ebx,dword[mystack]
	add ebx,eax
	sub ebx,4
	mov ecx,[ebx]
	movzx ecx,byte[ecx]
	mov [newval],cl
	addNode					;add first value and node
	mov [node1],eax			;node1 is the new node
	movzx edx,byte[curpos]  
	mov eax,4
	mul edx
	mov ebx,dword[mystack]
	add ebx,eax
	sub ebx,4
	mov ebx,[ebx]
	mov ebx,dword[ebx+1]	;ebx = next node to dup
	%%.startdup:
	cmp ebx,0				;if = 0
	je %%.endup	
	movzx edx,byte[ebx]		;edx=the value
	add ebx,1		
	mov ebx,dword[ebx]
	mov [node2],ebx	;[node2] =next node address
	mov byte[result],dl  ;add edx
	addNodeAtEnd		;eax is the next to add
	mov edx,[node1]
	add edx,1
	mov dword[edx],eax	;eax is now the next
	mov [node1],eax			;node1 is the new node
	mov ebx,[node2]				;eax=next
	jmp %%.startdup
	%%.endup:
%endmacro

%macro toLength	0		;pop first element and push his length
	movzx edx,byte[curpos]  
	mov eax,4
	mul edx
	mov ebx,dword[mystack]
	add ebx,eax
	sub ebx,4
	mov ebx,dword[ebx]		;ebx is the first node
	mov byte[counterNode],0		;init counternode
	mov ecx,0				;counter
	%%.startCheckLength:
	cmp ebx,0				;if current node= 0
	je %%.endCheckLength	
	add ebx,1		
	mov ebx,dword[ebx]
	add ecx,1				;counter ++
	jmp %%.startCheckLength
	%%.endCheckLength:
	mov byte[counterNode],cl
	mov dword[deluntil],0		;init deluntil
	deleteFirst
	%%.afterthedel:
	movzx eax,byte[counterNode]	;eax=counter
	mov ebx,16
	mov edx,0
	div ebx
	mov [counterNode],al
	cmp edx,10				;first value at edx
	jB %%.decHIMfirst
	jmp %%.hexHIMfirst
	%%.decHIMfirst:
		add edx,48
		jmp %%.afterfirst
	%%.hexHIMfirst:
		add edx,55
		jmp %%.afterfirst
	%%.afterfirst:
	mov [newval],dl
	addNode					;add first value and node
	mov [node1],eax			;node1 is the first
	movzx eax,byte[counterNode]
	%%.startToAdd:
	cmp eax,0			;check finish
	je %%.finito
	mov ebx,16
	mov edx,0
	div ebx
	cmp edx,10
	jB %%.decHIM
	jmp %%.hexHIM
	%%.decHIM:
		add edx,48
		jmp %%.afterHIM
	%%.hexHIM:
		add edx,55
		jmp %%.afterHIM
	%%.afterHIM:
	mov [counterNode],al		;store at counter
	mov [result],dl				;store value ar result
	addNodeAtEnd
	mov ebx,[node1]		;ebx is the last before add
	add ebx,1
	mov dword[ebx],eax	;make connection to next
	mov [node1],eax
	movzx eax,byte[counterNode]
	jmp %%.startToAdd
	%%.finito:				;finish adding nodes

%endmacro

%macro bitwiseAND 0		;doing the and op
	mov dword[deluntil],0 ;init deluntil
	mov byte[side],0	;make side zero before calc
	mov byte[counterNode],0	;make counter zero before 

	movzx edx,byte[curpos]  
	mov eax,4
	mul edx
	sub eax,4
	mov ebx,dword[mystack]
	add ebx,eax
	mov edx,[ebx]
	mov [node1],edx		;first node-node1
	sub ebx,4
	mov edx,[ebx]
	mov [node2],edx		;second node-node2
	%%.loop:
	and_func
	mov eax,[node2]
	mov byte[eax],bl		;update the result of and_func
	mov eax,[node1]
	mov ebx,dword[eax +1]		;ebx=next node of first
	mov eax,[node2]
	mov ecx,dword[eax +1]		;ecx=next node of second
	mov edx,0
	add edx,ebx
	add edx,ecx			;edx = sum of new values, if its 0 they both ended
	cmp edx,0
	je %%.endboth  
			;if we past here someone not finished...			
	cmp ebx,0		;if first node ended
		je %%.endSecond							;in _AND ONLY_ we will earase the continue of node2!!!!!
	cmp ecx,0		;if second node ended
		je %%.endSecond
	mov [node1],ebx
	mov [node2],ecx

	jmp %%.loop			;if both not ended
						
	%%.endboth:
		deleteFirst							;delete the first

	jmp %%.end
	%%.endFirst:
		deleteFirst							;delete the first

	jmp %%.end
	
	%%.endSecond:
	deleteFirst							;delete the first
	mov edx,[node2]
	mov eax,dword[edx +1]	;make node2 next to eax
	cmp eax,0
	je %%.end
	mov dword[deluntil],0 ;init deluntil
	freeNodeList				;delete the continue we dont need
	mov edx,[node2]
	mov dword[edx +1],0					;delete the connection to the deleted
	%%.end:				;for case second is the shorter number
	earaseZeroesFrom_First				;delete the zeroes we dont need

%endmacro

%macro earaseZeroesFrom_First 0
	movzx edx,byte[curpos]  
	mov eax,4
	mul edx
	sub eax,4
	mov ebx,dword[mystack]
	add ebx,eax
	mov edx,[ebx]
	mov [node1],edx		;first node = node1
	mov byte[counterNode],0	;init counter

	%%.start_check:
	cmp edx,0			;case no more nodes
	je %%.finish_check
	cmp byte[edx],0
	je %%.nextpls
	cmp byte[edx],48
	je %%.nextpls
			;case its not zero
	jmp %%.nextpls_no_zero
	%%.nextpls:
	add edx,1
	mov edx,dword[edx]
	jmp %%.start_check
	%%.nextpls_no_zero:
	add edx,1
	mov edx,dword[edx]
	mov [node1],edx		;first node = node1
	jmp %%.start_check

	%%.finish_check:

	;to check if its the first node we will keep it
	movzx edx,byte[curpos]  
	mov eax,4
	mul edx
	sub eax,4
	mov ebx,dword[mystack]
	add ebx,eax
	mov edx,dword[ebx]		;edx is first node
	mov eax,[node1]			;eax = node to del
	cmp eax,edx
	jne %%.noFirst
	mov eax,dword[edx+1]
	%%.noFirst:
	freeNodeList

	;to unconnect that node
	movzx edx,byte[curpos]  
	mov eax,4
	mul edx
	sub eax,4
	mov ebx,dword[mystack]
	add ebx,eax
	mov edx,dword[ebx]		;edx is first node
	mov eax,[node1]				;case first is 0
	cmp edx,eax
	je %%.ToDel
	%%.loop_h:
	mov ebx,dword[edx+1]		;ebx is next
	cmp ebx,eax
	je %%.ToDel
	mov edx,ebx
	jmp %%.loop_h
	%%.ToDel:
	mov dword[edx+1],0		;del the connection
	%%.rlyend:

%endmacro

%macro and_func 0
	%%.B_and:		;make b normal
	mov ebx,[node1]
	movzx ebx,byte[ebx]
	cmp ebx,58
	jl %%.Bunder58_and
		sub ebx,55
		jmp %%.C_and
	%%.Bunder58_and:
		sub ebx,48
	%%.C_and:					;make c normal
	mov ecx,[node2]
	movzx ecx,byte[ecx]
	cmp ecx,58
	jl %%.Cunder58_and
		sub ecx,55
		jmp %%.calculate_and
	%%.Cunder58_and:
		sub ecx,48
	%%.calculate_and:
	and ebx,ecx		;the and func
	
	cmp ebx,16
	jl %%.noAdd_and
	sub ebx,16
	inc byte[side]
	%%.noAdd_and:
	cmp ebx,10
		jB %%.dec_and
		jmp %%.hex_and
			%%.dec_and:
				add ebx,48
				jmp %%.after_and
			%%.hex_and:
				add ebx,55
				jmp %%.after_and
	%%.after_and:
%endmacro

%macro or_func 0
	%%.B_and:		;make b normal
	mov ebx,[node1]
	movzx ebx,byte[ebx]
	cmp ebx,58
	jl %%.Bunder58_and
		sub ebx,55
		jmp %%.C_and
	%%.Bunder58_and:
		sub ebx,48
	%%.C_and:					;make c normal
	mov ecx,[node2]
	movzx ecx,byte[ecx]
	cmp ecx,58
	jl %%.Cunder58_and
		sub ecx,55
		jmp %%.calculate_and
	%%.Cunder58_and:
		sub ecx,48
	%%.calculate_and:
	or ebx,ecx		;the or func
	
	cmp ebx,16
	jl %%.noAdd_and
	sub ebx,16
	inc byte[side]
	%%.noAdd_and:
	cmp ebx,10
		jB %%.dec_and
		jmp %%.hex_and
			%%.dec_and:
				add ebx,48
				jmp %%.after_and
			%%.hex_and:
				add ebx,55
				jmp %%.after_and
	%%.after_and:
%endmacro

%macro doPow 0              	;// Power: Add num1 x num2 times and 
        mov eax, [num1]			;// add result x num2 times till ebx=0

        mov ebx, [num2]

		mov ecx,eax
		.startpow:
		cmp ebx,1
		je .afterpow
		mul ecx
		sub ebx,1
		jmp .startpow

		.afterpow:

%endmacro

%macro bitwiseOR 0		;doing the or op
	mov dword[deluntil],0 ;init deluntil
	mov byte[side],0	;make side zero before calc
	mov byte[counterNode],0	;make counter zero before 

	movzx edx,byte[curpos]  
	mov eax,4
	mul edx
	sub eax,4
	mov ebx,dword[mystack]
	add ebx,eax
	mov edx,[ebx]
	mov [node1],edx		;first node-node1
	sub ebx,4
	mov edx,[ebx]
	mov [node2],edx		;second node-node2
	%%.loop:
	or_func
	mov eax,[node2]
	mov byte[eax],bl		;update the result of and_func
	mov eax,[node1]
	mov ebx,dword[eax +1]		;ebx=next node of first
	mov eax,[node2]
	mov ecx,dword[eax +1]		;ecx=next node of second
	mov edx,0
	add edx,ebx
	add edx,ecx			;edx = sum of new values, if its 0 they both ended
	cmp edx,0
	je %%.endboth  
			;if we past here someone not finished...			
	cmp ebx,0		;if first node ended
		je %%.endFirst							;in _AND ONLY_ we will earase the continue of node2!!!!!
	cmp ecx,0		;if second node ended
		je %%.endSecond
	mov [node1],ebx
	mov [node2],ecx

	jmp %%.loop			;if both not ended
						
	%%.endboth:
		deleteFirst								;delete the first
		earaseZeroesFrom_First				;delete the zeroes we dont need
	jmp %%.endOR
	%%.endFirst:
		deleteFirst								;delete the first
		earaseZeroesFrom_First				;delete the zeroes we dont need
	jmp %%.endOR
	%%.endSecond:

	mov [deluntil],ebx 	;deluntil is the adrres to delete until not include
	deleteFirst							;delete the first

	mov eax,[node2]
	add eax,1
	mov ebx,[deluntil]
	mov dword[eax],ebx		;ecx=next node of second

	earaseZeroesFrom_First				;delete the zeroes we dont need
	
	%%.endOR:				
	
%endmacro

%macro printDeg 0
	cmp byte[deg],0
	je .noDeg

	movzx edx,byte[curpos]  
	mov eax,4
	mul edx
	sub eax,4
	mov ebx,dword[mystack]
	add ebx,eax
	mov eax,[ebx]		;eax=first node address
	mov ecx,0
	%%.startpush:
	cmp eax,0				;if = 0
	je %%.endpush				
	mov ebx,dword[eax+1]	;ebx =next node address
	movzx edx,byte[eax]		;edx=the value
	cmp edx,57				
	jle %%.decc		;if edx is number
		;sub edx,55
	jmp %%.rdy
	%%.decc:
		;sub edx,48
	%%.rdy:
	push edx
	inc ecx			;inc counter
	mov eax,ebx				;eax=next
	jmp %%.startpush
	%%.endpush:
	mov ebx,0
	mov eax,an
	cmp ecx,0
	je %%.endprint
	mov byte[counterNode],cl
	%%.makeAN:
	cmp ecx,0	;conterNode
	jE %%.printAN
	.p:
		pop edx
		;add eax,ebx
		mov eax,an
		mov byte[eax],dl		;ebx counter chars, eax the number

		mov edx,1	  			;message length
		mov ecx,an	    		;message to write
		mov	ebx,2       		;file descriptor (stdout)
		mov	eax,4       		;system call number (sys_write)
		int	0x80        		;call kernel
	.pp:
		movzx ecx,byte[counterNode]
		sub ecx,1
		mov byte[counterNode],cl

	jmp %%.makeAN
	%%.printAN:

		mov eax,an
		mov byte[eax],10		;ebx counter chars, eax the number

	.ppp:
		mov edx,1	  			;message length
		mov ecx,an	    		;message to write
		mov	ebx,2       		;file descriptor (stdout)
		mov	eax,4       		;system call number (sys_write)
		int	0x80        		;call kernel
	.pppp:
	mov dword[deluntil],0	;init [deluntil]
	;deleteFirst			;should be on///////////////////////////////////////////////////////

	%%.endprint:
	.noDeg:

%endmacro

%macro toPrintOp 0	;print num of ops
	mov ecx,0
	mov edx,0
	mov eax,an
	%%.init_an:
	cmp	ecx,80
	je %%.end_init_an
	add eax,ecx
	mov byte[eax],dl		;init [an]
	sub eax,ecx
	inc ecx
	jmp %%.init_an
	%%.end_init_an:

	mov ecx,0
	movzx eax,byte[NumOfOp]
	%%.startmakehim:
	mov edx,0
	mov ebx,16
	div ebx					;eax=rest of num, edx=the div result
	cmp edx,10				
	jl %%.decc		;if edx is number
		add edx,55
	jmp %%.rdy
	%%.decc:
		add edx,48
	%%.rdy:
	push edx
	inc ecx
	cmp eax,0
	jne %%.startmakehim

vb:

	mov eax,an
	mov ebx,0
	%%startmakehim2:
	cmp ecx,0
	je %%.finitoo
	pop edx
	add eax,ebx
	mov byte[eax],dl
	sub eax,ebx
	inc ebx
	sub ecx,1
	jmp %%startmakehim2

	%%.finitoo:

	mov eax,an
	push eax	
	push format_stringNEWLINE
	call printf			;print
	pop edx
	pop edx

%endmacro

main:
	push ebp
	mov ebp, esp	
	pushad
	mov edx , [ebp+8]
	mov ebx , [ebp+12]				;pointer to argv array
	cmp  edx  , 1
	je end_of_args
	mov ecx , 2
	start_loop:	;loop
	mov edx , [ebp+8]
	mov ebx , [ebp+12]				;pointer to argv array
	cmp ecx,edx
	jg end_of_args
	mov eax,4
	mul ecx
	sub eax,4
	add ebx,eax
	mov eax,[ebx]
		cmp byte[eax] ,45 ;if = '-'
		je debuger
		jmp inputSet
	debuger:
		mov byte [deg],1
		inc ecx				;turn on debug mode
		jmp start_loop
	inputSet:					;read new size
		mov eax, [ebx]				;pointer to string input of first argument
		mov eax , [eax]
		mov  [size] , al
		cmp byte[size],58
		jl under58
		mov eax,55				;pointer to string input of first argument
		sub [size],eax
		inc ecx				
		jmp start_loop
	under58:
		mov eax,48				;pointer to string input of first argument
		sub [size],eax
		inc ecx				
		jmp start_loop
end_of_args:
	mov eax,0xFF
	mov ebx,32
	mul ebx
	push eax
	push 1
	call calloc
	mov [mystack],eax
	pop eax
	pop eax
	push 8
	push 80
	call calloc
	mov [input],eax
	pop eax
	pop eax
	;push 8
	;push 80
	;call calloc
	;mov [rlyinput],eax
	;pop eax
	;pop eax
	call myCalc 
	mov eax, 4      		; system call number (sys_read)
    mov ebx, 1      		; file descriptor (stdin)
    mov ecx, goodbye   		; buffer to keep the read data
    mov edx, lengoodbye    	; bytes to read
    int 0x80		 		; call kernel
	toPrintOp

	jmp end_of_func

myCalc:
	push ebp              		; save Base Pointer (bp) original value
	mov ebp, esp         		; use Base Pointer to access stack contents (do_Str(...) activation frame)
    pushad 
myCalcSecond:      
reading_user_input:
	mov	edx,lenplsinputmsg	  			;message length
	mov	ecx,plsinputmsg	    ;message to write
	mov	ebx,1       		;file descriptor (stdout)
	mov	eax,4       		;system call number (sys_write)
	int	0x80        		;call kernel
	;reading use input
	mov eax, 3      	; system call number (sys_read)
    mov ebx, 0      	; file descriptor (stdin)
    mov ecx, [input]   	; buffer to keep the read data
    mov edx, 640       	; bytes to read
	int 0x80		 	;call kernel
	
	mov byte[counterNode],0
	get_input:
	mov ecx,[input]
	movzx edx,byte[counterNode]
	add ecx,edx
	movzx ecx,byte[ecx]
	cmp ecx,48
	je itsZero
	jne itsNoZero
	itsZero:
	inc byte[counterNode]
	jmp get_input

	itsNoZero:
	mov ecx,[input]
	movzx edx,byte[counterNode]
	add ecx,edx
	mov [rlyinput],ecx
		
	movzx ecx,byte[ecx]
	mov byte[counterNode],0



cmp ecx,113		;case q
		je endmyCalc
		cmp ecx,43		;case +
		je plus
		cmp ecx,112		;case p
		je p
		cmp ecx,100		;case d
		je d	
		cmp ecx,38		;case &
		je and	
		cmp ecx,124		;case |
		je or	
		cmp ecx,110		;case n
		je n	
		cmp ecx,42		;case *
		;je addNumber	

		jmp regNumber	;case Number	

regNumber:	;case Number

	
	cmp byte[deg],0
	je .noDeg

	mov byte[counterNode],0
	.nextchar:
	movzx edx,byte[counterNode]
	add edx,[rlyinput]
	mov ecx,edx
	movzx ecx,byte[ecx]
	cmp ecx,10	;if its \n
	je .fcount
	inc byte[counterNode]
	jmp .nextchar
	.fcount:
		inc byte[counterNode]		;to include \n

		movzx edx,byte[counterNode]	  			;message length
		mov ecx,[rlyinput]	    	;message to write
		mov	ebx,2       		;file descriptor (stdout)
		mov	eax,4       		;system call number (sys_write)
		int	0x80        		;call kernel
	.noDeg:

	movzx edx,byte[curpos]
	cmp dl,[size]		;check if theres a place for new number
	jge errorOverflow
	addNumber		  ;add number
	inc byte[curpos] ;inc free position in array


	;inc byte[NumOfOp]
	jmp myCalcSecond


plus:	;case +
	movzx edx,byte[curpos]
	cmp dl,1		;check if there 2 or more numbers
	jle errInsufficientLabel
	plus_func
	printDeg
		inc byte[NumOfOp]

	jmp myCalcSecond
	
p:
	movzx edx,byte[curpos]
	cmp dl,0		;check if there a number
	je errInsufficientLabel
	toPrint
		inc byte[NumOfOp]

	jmp myCalcSecond

n:
	movzx edx,byte[curpos]
	cmp dl,0		;check if there a number
	je errInsufficientLabel
	toLength
	inc byte[curpos] ;inc free position in array
	printDeg
		inc byte[NumOfOp]

	jmp myCalcSecond
	
d:
	movzx edx,byte[curpos]
	cmp dl,0		;check if there a number
	je errInsufficientLabel
	cmp dl,[size]		;check if theres a place for new number
	jge errorOverflow
	dup
	inc byte[curpos] ;inc free position in array
	printDeg
	inc byte[NumOfOp]

	jmp myCalcSecond

and:	;case &

	movzx edx,byte[curpos]
	cmp dl,1		;check if there 2 or more numbers
	jle errInsufficientLabel
	bitwiseAND
	printDeg
		inc byte[NumOfOp]

	jmp myCalcSecond	

or:	;case |
	movzx edx,byte[curpos]
	cmp dl,1		;check if there 2 or more numbers
	jle errInsufficientLabel
	bitwiseOR
	printDeg
		inc byte[NumOfOp]

	jmp myCalcSecond

errorOverflow:
	mov	edx,lenOverFlow	  			;message length
	mov	ecx,errOverFlow	    ;message to write
	mov	ebx,1       		;file descriptor (stdout)
	mov	eax,4       		;system call number (sys_write)
	int	0x80        		;call kernel

			inc byte[NumOfOp]

	jmp myCalcSecond


errInsufficientLabel:
	mov	edx,lenInsufficient	  			;message length
	mov	ecx,errInsufficient	    ;message to write
	mov	ebx,1       		;file descriptor (stdout)
	mov	eax,4       		;system call number (sys_write)
	int	0x80        		;call kernel
	inc byte[NumOfOp]
	jmp myCalcSecond

endmyCalc:
		movzx eax,byte[NumOfOp]
		popad                   ; restore all previously used registers
        mov esp, ebp			; free function activation frame
        pop ebp					; restore Base Pointer previous value (to returnt to the activation frame of main(...))
        ret						; returns from do_Str(...) function

end_of_func:

	startdeleting:
	cmp byte[curpos],0		;function to delete all existing nodes
	je finishdeleting
	deleteFirst
	jmp startdeleting
	finishdeleting:

	mov eax,[mystack]
	push eax
	call free				;delete the Array
	pop eax

	mov eax,[input]
	push eax
	call free				;delete the input
	pop eax

	;mov eax,[rlyinput]
	;push eax
	;call free				;delete the rlyinput
	;pop eax


	popad                   ; Restore caller state (registers)
    mov     esp,ebp    		; place returned value where caller can see it
    pop     ebp             ; Restore caller state
    ret                     ; Back to caller






















