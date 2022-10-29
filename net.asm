;======================================================================================================
;=										    Created by 						 					  	  =
;=									Kiss Krisztian, january 2022									  =
;======================================================================================================

; Compile:
; nasm -fwin32 net.asm

; dependencies
%include            'io.inc'
%include            'util.inc'
%include 	        'dbgfile.inc'

; size in which the input image should be resized (width/height), and the total size in bytes that the new image will occupy
%define             RESIZE_SIZE                 28                                      
%define             RESIZE_BYTES                RESIZE_SIZE * RESIZE_SIZE * 4

; constant sizes of Convolution2d
%define 		    CONV_KERNEL_SIZE	        3
%define 		    CONV_PADDING		        1
%define			    CONV_STRIDE			        1

; constant sizes of MaxPool2d
%define			    POOL_KERNEL_SIZE	        2
%define			    POOL_STRIDE			        2

; code and character constants for storing the structure of the neural network
%define			    CONV2D_CODE			        0
%define			    CONV2D_CHAR			        'C'
%define 		    RELU_CODE			        1
%define 		    RELU_CHAR			        'R'
%define 		    MAXPOOL2D_CODE		        2
%define 		    MAXPOOL2D_CHAR		        'M'
%define 		    LINEAR_CODE			        3
%define			    LINEAR_CHAR			        'L'
%define			    SOFTMAX_CODE		        4
%define			    SOFTMAX_CHAR		        'S'

; character constants
%define 		    NEW_LINE_CHR		        10				; '\n'
%define 			SPACE_CHR					' '
%define 			TAB_CHR						9				; '\t'

; floating point constants
    ; minus infinity represented as a 32 bit floating point number
%define			    FLT_MIN_INF			        0xFF800000              
    ; if used on the same XMM register with the SHUFPS operation, this immediate value will
    ; broadcast the lower 32 bit of the register to all other parts of the register  
%define 		    SHUFPS_ALL			        0x00    
    ; as above, but this whill shuffle the elements one position to the left, the most significant becoming the least one     
%define			    SHUFPS_SHL			        0x93

; global functions, must be "extern"-ed in 'net.inc' to be used in the main function
global net_init, net_start, net_destroy

section .text

;======================================================================================================
; Macro, that takes 4 parameters. It calculates the memory location of position (row, col) = (%2, %3) with
; base memory %1 in a 2d map of size (width/height) %4 and containing 4 byte unit data. It places the calculated
; memory location in EAX.
%macro __get_buffer_nocheck 4
	push	ebx         ; save the used registers
	push	ecx
	mov		eax, %1     ; EAX = memory base
	mov		ebx, %3     ; EBX = col
	shl		ebx, 2      ; EBX = col * 4
	add		eax, ebx    ; EAX = mem_base + col * 4
	mov		ebx, %2     ; EBX = row
	mov		ecx, %4     ; ECX = size
	imul	ebx, ecx    ; EBX = row * size
	shl		ebx, 2      ; EBX = row * size * 4
	add		eax, ebx    ; EAX = mem_base + (col + row * size) * 4
	pop		ecx         ; load the saved registers
	pop		ebx
%endmacro ; (__get_buffer_nocheck)

;======================================================================================================
; Macro, that takes 4 parameters. It calculates the memory location of position (row, col) = (%2, %3) with
; base memory %1 in a 2d map of size (width/height) %4 and containing 4 byte unit data. It places the calculated
; memory location in EAX. It also checks if (row, col) is inside of the map, if it is not, EAX will be 0.
; Warning: this macro can only be used at one place in a function, because it has a jump tag inside.
%macro __get_buffer_check 4
	push	ebx         ; save the used registers
	push	ecx
	mov		ebx, %2     ; EBX = row
	cmp		ebx, 0      ; check if the row is inside of the map (it is >= 0)
	jl		.not_valid
	cmp		ebx, %4     ; check if the row if inside of the map (it is < size)
	jge		.not_valid
	mov		ebx, %3     ; EBX = col
	cmp		ebx, 0      ; check if the col is inside of the map (it is >= 0)
	jl		.not_valid
	cmp		ebx, %4     ; check if the col is inside of the map (it is < size)
	jge		.not_valid
	mov		eax, %1     ; EAX = memory base
	mov		ebx, %3     ; EBX = col
	shl		ebx, 2      ; EBX = col * 4
	add		eax, ebx    ; EAX = mem_base + col * 4
	mov		ebx, %2     ; EBX = row
	mov		ecx, %4     ; ECX = size
	imul	ebx, ecx    ; EBX = row * size
	shl		ebx, 2      ; EBX = row * size * 4
	add		eax, ebx    ; EAX = mem_base + (col + row * size) * 4
	pop		ecx         ; load the saved registers
	pop		ebx
	jmp		.over
  .not_valid:
	xor		eax, eax    ; EAX = 0
	pop		ecx         ; load the saved registers
	pop		ebx
  .over:
%endmacro ; (__get_buffer_check)

;======================================================================================================
; Initializes the structure of the net from the txt file, the parameters of the functions from the binary
; file, and prints the structure of the net to the screen. No in/out parameters.
net_init:
	call	_init_model_struct
	call	_write_struct
	call	_init_net_weight		; this function returns error in CF, so it has to be the last that gets called
    ret
; END (net_init)

;======================================================================================================
; Starts evaluating the net functions on the given input image. In: EBX - memory pointer to the beginning
; of the original image's buffer, ECX - size in pixels of the original image's size (width/height),
; EDX - debug state variable. Out: none.
net_start:
	push	eax                                     ; save the used registers to the stack
	push	ebx
	push	ecx
	push	edx
	push	ebp

	mov		[img_orig_buff], ebx                    ; save the pointer to the original image's buffer
	mov		[img_orig_size], ecx                    ; save the size (width/height) of the original image

	mov		[fs_debug], edx                  		; save the debug state variable

    call	_img_resize                             ; resizes the input image to a RESIZE_SIZE width/height image, with [-1.00...+1.00] values

	test	dword [fs_debug], 0x2            		; check if the maps are needed to be printed to the debug file during the net functions
	jz		.no_f_debug1
	mov		eax, [fs_debug]                  
	call	dbgfile_init                            ; if yes, initialize the debug file module with EAX = debug state variable
	mov		eax, [map_buff]                         ; then write the resized image to the file in the first step
	mov		ebx, [map_size]
	mov		ecx, [map_channels]
	call	dbgfile_write                          	; this function takes EAX, EBX, ECX as input parameters
  .no_f_debug1:

	mov		eax, [net_weight_buff]                  ; EAX points to the beginning of memory location which stores the weights
	mov		[net_weight_ptr], eax                   ; move this pointer into a variable

	mov		ebp, net_model                          ; EBP placed at the beginning of the net structure memory array
	.loop_func:
		cmp		[ebp], byte -1                      ; check if we reached the end of the array (we terminated all the functions)
		je		.end_loop_func
		inc		ebp                                 ; increase EBP, because this should be done either way in each jump
		cmp		[ebp - 1], byte CONV2D_CODE         ; check which function is coded at the current position
		je		.conv2d
		cmp		[ebp - 1], byte RELU_CODE			
		je		.relu
		cmp		[ebp - 1], byte MAXPOOL2D_CODE
		je		.maxpool2d
		cmp		[ebp - 1], byte LINEAR_CODE
		je		.linear
		cmp		[ebp - 1], byte SOFTMAX_CODE
		je		.softmax
		jmp		.error                              ; if we didn't meet any known function codes, there should be an error
	  .conv2d:
	  	call	_conv2d
		add		ebp, 8								; this function holds two parameters, skip these after evaluating
		jmp		.write_dbg 
	  .relu:
	  	call	_relu 
		jmp		.write_dbg
	  .maxpool2d:
	  	call	_maxpool2d
		jmp		.write_dbg
	  .linear:
	  	call	_linear
		add		ebp, 8								; this function holds two parameters, skip these after evaluating
		jmp		.write_dbg
	  .softmax:
	  	call	_softmax
		jmp		.write_dbg
	  .write_dbg:
	  	test	dword [fs_debug], 0x2				; check if we need to write to the debug file
		jz		.loop_func
		mov		eax, [map_buff]						; writing to the debug file takes EAX = map buffer memory pointer,
		mov		ebx, [map_size]						; EBX = size of the map (width/height),
		mov		ecx, [map_channels]					; ECX = number of channels, as parameters
		call	dbgfile_write
		jmp		.loop_func
  .end_loop_func:

	test	dword [fs_debug], 0x1					; check if we need to print debug information to the screen
	jz		.no_s_debug1
	mov		eax, msg_success_evaluate
	call	io_writestr
  .no_s_debug1:

	test	dword [fs_debug], 0x2					; check if we used the file debug
	jz		.no_f_debug2						
	call	dbgfile_destroy							; if so, we need to call the module's destructor
  .no_f_debug2:

	call	_write_result							; write the result (prediction) of the net to the screen

	mov		eax, [map_buff]							; free the memory used for storing the map
	call	mem_free 

	pop		ebp										; load the saved registers
	pop		edx
	pop		ecx
	pop		ebx
	pop		eax
	ret

  .error:											; in case of an error
	mov		eax, [map_buff]
	call	mem_free								; we still have to free the memory
	test	dword [fs_debug], 0x1					; check if we have to display the debug message
	jz		.no_s_debug2
	mov		eax, msg_error_evaluate
	call	io_writestr
  .no_s_debug2:
	pop		ebp										; load the saved registers
	pop		edx
	pop		ecx
	pop		ebx
	pop		eax
    ret
; END (net_start)

;======================================================================================================
; Basically frees the memory allocated for the weights of the network. No in/out parameters.
net_destroy:
	push	eax

	mov		eax, [net_weight_buff]			; the networks weights are stored in this memory location
	call	mem_free

	test	dword [fs_debug], 0x1			; check if we need to display debug information on the screen
	jz		.no_s_debug
	mov		eax, msg_success_dest
	call	io_writestr
  .no_s_debug:

	pop		eax
	ret
; END (net_destroy)

;======================================================================================================
; Initializes the structure array of the neural network. Reads the functions from the txt files, and 
; stores the crucial information, which will be used later on while evaluating the whole big net function.
_init_model_struct:
	push	eax								; save the used registers to the stack 
	push	ebx
	push	ecx
	push	edx
	push	ebp

	; open the txt file (EAX) in read mode (EBX = 0)
	mov		eax, conv_txt_name
	xor		ebx, ebx
	call	fio_open

	mov		ebx, net_model					; EBX points to the beginning of the structure array

	.read_lines:							; iterate through the lines of the txt file	
		call	_next_line					; read the current line into temp_str string
		cmp		[temp_str], byte 0			; if the first character is the null character, there are no more lines
		je		.end_read_lines				; so we jump out
		call	_parse_line					; else we parse the line
		jmp		.read_lines					; then jump back to the beginning of the loop
  .end_read_lines:

	mov		[ebx], byte -1					; we indicate the end of the structure array with a -1
	shl		dword [net_weight_bytes], 2		; we counted how many float numbers should be in the bin file, now we multiply this by 4 to get the number of bytes
	
	call	fio_close						; close the file, EAX = file handle

	pop		ebp								; load the saved registers
	pop		edx
	pop		ecx
	pop		ebx
	pop		eax
	ret
; END (_init_model_struct)

;======================================================================================================
; Reads the next line from EAX file handle into temp_str global string.
_next_line:
	push	ebx								; save the used registers
	push	ecx
	push	edx
	push	ebp

	xor		ecx, ecx
	inc		ecx								; ECX = 1, the number of bytes that we will read in a step
	mov		ebx, temp_str					; EBX = the beginning of the string

	.loop_char:								; read char by char from the file
		call	fio_read					; EAX file handle, EBX memory location, ECX=1 number of bytes -> reads a char to the end of the string
		test 	edx, edx					; check how many bytes did it read
		jz		.end_loop_char				; if it read none, jump out
		cmp		[ebx], byte NEW_LINE_CHR	; check if we reached the newline character
		je		.end_loop_char
		inc		ebx							; step 1 byte in the string
		jmp		.loop_char

  .end_loop_char:
	mov		[ebx], byte 0					; place the null character at the end of the string

	pop		ebp								; load the saved registers
	pop		edx
	pop		ecx
	pop		ebx
	ret
; END (_next_line)

;======================================================================================================
; Finds the next numeric value in a string starting from EAX location, and returns this in ECX.
_next_num:
	push	ebx							; save this man to the stack

	dec		eax							; step one back, so that we can increment it as the first step of the loop
	.find_first_num:					; basically we find the first position where a digit occours
		inc		eax
		cmp		[eax], byte 0			; check if we reached the end of the string
		je		.end_find_first_num
		cmp		[eax], byte '0'			; if it is less than the character zero, it is not a digit
		jl		.find_first_num
		cmp		[eax], byte '9'			; if it is greater than the character nince, it is not a digit
		jge		.find_first_num
  .end_find_first_num:					; if we reached this, either we terminated the string or found the first digit

	xor		ecx, ecx					; ECX = the resulting numeric value				
	xor		ebx, ebx					; we need to set EBX to 0, because we will only use its lower byte for data move, but we will use the whole for operations
	.build_num:							; loop through the string
		cmp		[eax], byte 0			; if we reached the end of the string, we jump out
		je		.end
		cmp		[eax], byte '0'			; if we reached a character that is not a digit, we jump out
		jl		.end
		cmp		[eax], byte '9'
		jg 		.end
		mov		bl, [eax]				; on position EAX there is an one byte ascii digit, we move that to BL
		sub		bl, '0'					; we convert the character to numeric value
		imul	ecx, ecx, 10			; ECX = ECX * 10
		add		ecx, ebx				; ECX += new digit
		inc		eax						; step 1 byte in the string
		jmp		.build_num				; go to the next character
  .end:

	pop		ebx							; load this saved bad boi
	ret
; END (_next_num)

;======================================================================================================
; Goes through temp_str, checking if it's structure matches one of the known functions. Then it places
; the matched function's code (and parameters, if any) to the structure array, ending at EBX.
_parse_line:
	push	eax								; save the used registers to the stack
	push	ecx

	mov 	eax, temp_str					; with EAX begin at the beginning of the string
	dec		eax								; we start before the beginning of it, because the first step will be incrementing it
	.find_first_char:						; we want to find the first character in the line, from this we will deduce the function
		inc 	eax 						
		cmp		[eax], byte 0				; if we reached the end of the string, it's over
		je		.end_find_first_char
		cmp		[eax], byte SPACE_CHR		; if we encountered a space, we skip it
		je	 	.find_first_char
		cmp		[eax], byte TAB_CHR			; if we encountered a tab, we skip it
		je		.find_first_char 			
  .end_find_first_char:						; if it is not space or tab, we found a character
	
	; let's check which function could indicate the character by comparing the first char in the string
	; to the first character in the names of the funcitons
	cmp		[eax], byte CONV2D_CHAR
	je		.conv2d
	cmp		[eax], byte RELU_CHAR
	je		.relu
	cmp		[eax], byte MAXPOOL2D_CHAR
	je		.maxpool2d 
	cmp		[eax], byte LINEAR_CHAR 
	je 		.linear
	cmp		[eax], byte SOFTMAX_CHAR
	je		.softmax
	jmp		.end							; if no match, we don't do anything, it's not a function

  .conv2d:
	mov		[ebx], byte CONV2D_CODE			; place the code of the function into the structure array on 1 byte
	call	_next_num						; skip the '2' from the name of the function
	call	_next_num						; get the first parameter
	mov		[ebx + 1], ecx					; place the first parameter after the code on 4 bytes
	call	_next_num
	mov		[ebx + 5], ecx					; get the second parameter
	add		[net_weight_bytes], ecx			; there will be out_channels number of biases, add this to the total number of floats
	imul	ecx, [ebx + 1]					; calculate in_channels * out_channels * KERNEL_SZ * KERNEL_SZ,
	imul	ecx, ecx, CONV_KERNEL_SIZE		; this will be the number of weights in this convok
	imul	ecx, ecx, CONV_KERNEL_SIZE
	add		[net_weight_bytes], ecx
	add		ebx, 9							; step 1 byte + 2 * 4 bytes in the structure array
	jmp		.end
  .relu:
	mov		[ebx], byte RELU_CODE			; only the code needs to be placed, there are no parameters
	inc		ebx
	jmp		.end
  .maxpool2d:
	mov		[ebx], byte MAXPOOL2D_CODE		; like above
	inc		ebx
	jmp		.end
  .linear:
	mov		[ebx], byte LINEAR_CODE			; just like in the case of Conv2d, we have two parameters
	call	_next_num						; the first parameter is the first number
	mov		[ebx + 1], ecx
	call	_next_num						; the second parameter is the second number
	mov		[ebx + 5], ecx
	add		[net_weight_bytes], ecx			; the second parameter indicates how many biases will there be
	imul	ecx, [ebx + 1]					; there will be in_features * out_features number of weights			
	add		[net_weight_bytes], ecx
	add		ebx, 9							; step 1 + 2 * 4 bytes in the structure array
	jmp		.end
  .softmax:
	mov		[ebx], byte SOFTMAX_CODE		; no parameters function
	inc		ebx
	jmp		.end

  .end:
	pop		ecx
	pop		eax
	ret
; END (_parse_line)

;======================================================================================================
; Initializes the buffer for the net weights and biases, by reading the precalculated number of floating
; point 32 bit numbers from the binary file. This is easy, because we have the number of bytes that need
; to be read, and because we don't place the floats in any specific order into the buffer, we just
; read everything from the file into the buffer.
; This function returns the error state in the carry flag, CF=0 if no error, CF=1 otherwise.
_init_net_weight:
	push	eax								; save the used registers to the stack
	push	ebx
	push	ecx
	push	edx

	mov		eax, [net_weight_bytes]			; load the number of bytes that the net weights and biases occupy
	call	mem_alloc						; allocate memory for the buffer
	mov		[net_weight_buff], eax			; save the pointer to the allocated memory

	mov		eax, net_bin_name				; open the file with EAX name 
	xor		ebx, ebx						; and EBX = 0 -> read mode
	call	fio_open						; after the call, EAX will hold the file handler

	mov		ebx, [net_weight_buff]			; where do we want to read in
	mov		ecx, [net_weight_bytes]			; how many bytes do we need to read
	call	fio_read						; read the data from the file, EAX, EBX, ECX parameters
	call	fio_close						; close the file with EAX handle

	cmp		edx, [net_weight_bytes]			; if we couldn't read the desired number of data, there isn't correspondance
	jne		.error							; between the txt and bin files

	test	dword [fs_debug], 0x1			; check if we need to display debug info to the screen
	jz		.no_s_debug1
	mov		eax, msg_success_param
	call	io_writestr
  .no_s_debug1:

	pop		edx								; load the saved registers
	pop		ecx
	pop		ebx
	pop		eax
	clc										; CF=0, no error
	ret

  .error:
	test	dword [fs_debug], 0x1			; check if we need to display debug info to the screen
	jz		.no_s_debug2
	mov		eax, msg_error_param
	call	io_writestr
  .no_s_debug2:

	pop		edx								; load the saved registers
	pop		ecx
	pop		ebx
	pop		eax
	stc 									; CF=1, error
	ret
; END (_init_net_weight)

;======================================================================================================
; Resizes the input image whose 1 byte pixels are stored on [img_orig_buff] memory location, to a new,
; RESIZE_SIZE * RESIZE_SIZE image by adding together the original pixels, then averages the new values
; to get a 0..255 range grayscale pixel in each cell, and scales these new cells to [-1.00...+1.00] interval.
; After adding together the original pixels, it applies the following formula:
; new_img[i][j] = (2 * sum[i][j] - y^2) / y^2, where y = ORIGINAL_SIZE / RESIZE_SIZE and i, j = 0..RESIZE_SIZE-1
_img_resize:
	push	eax										; save the used registers to the stack
	push	ebx
	push	ecx
	push	edx
	push	esi
	push	edi
	push	ebp

    mov     eax, RESIZE_BYTES						; allocate memory for the resized image
    call    mem_alloc
    mov     [map_buff], eax							; save the allocated memory pointer

    xor     ecx, ecx								; loop through the resized image buffer, set everything to 0
    xorps   xmm0, xmm0								; XMM0 = 0 on all 4 parts (128 bits)
    .load_zero:
        cmp     ecx, RESIZE_BYTES					; check if we reached the end of the buffer
        jge     .end_load_zero		
        movups  [eax + ecx], xmm0					; use SSE vectorization, move the 0 to 4 floating point numbers in a step
        add     ecx, 16								; step by 4 * 4 bytes
        jmp     .load_zero
  .end_load_zero:

    mov     esi, [img_orig_buff]					; source = original image
    mov     edi, [map_buff]							; destination = new, resized image

    mov     eax, [img_orig_size]					; EAX = ORIG_SIZE
    mov     ebx, RESIZE_SIZE						; EBX = RESIZE_SIZE
    cdq												; adjust EDX according to a division
    idiv    ebx                 					; divide EDX:EAX by EBX, quotient gets placed in EAX
    push    eax                 					; we calculated y = ORIG_SZ / RESIZE_SZ, push this to the stack

    xor     eax, eax								; EAX = 0, with this we loop through each cell of the new image in a continuous way, by 4 bytes
    .loop1:
        cmp     eax, RESIZE_BYTES					; check if we reached the end of the new image
        jge     .end_loop1
        push    esi									; save the base pointing to the top left corner of our current old image grid
        xor     ebx, ebx							; EBX = 0, with this we loop through each row of the old image corresponding to the current new cell
        xor     edx, edx							; EDX = 0, in this we will extract one single pixel from the old image
        xor     ebp, ebp							; EBP = 0, in this we will accumulate the sum of the cells from the original image
        .loop2:
            cmp     ebx, [esp + 4]					; check if we reached y rows from the original image
            jge     .end_loop2
            xor     ecx, ecx						; ECX = 0, with this we loop through each column of the old image corresponding to the current new cell
            .loop3:
                cmp     ecx, [esp + 4]				; check if we reached y columns from the original image
                jge     .end_loop3
                mov     dl, [esi]					; extract 1 byte from the original image
                add     ebp, edx					; add it to the new image cell sum
                inc     esi							; step to the next cell from the old image
                inc     ecx							; step to the next column
                jmp     .loop3						; loop through the columns
          .end_loop3:								; at this point we are done with a row
            add     esi, [img_orig_size]			; step one whole row down with the buffer pointer,
            sub     esi, [esp + 4]					; and also y columns back, this way we step one row down from the base of the old image grid
            inc     ebx								; step to the next row
            jmp     .loop2							; loop through the rows
      .end_loop2:									; at this point we are done with a whole old grid/nem cell
        pop     esi									; retrieve the original top left corner base of the old grid
		push    eax									; save EAX and EBX, we have to use them for calculations
        push    ebx
        shr     eax, 2								; EAX stores the bytes of the new image, we have to divide this by 4 to get the number of cell we are into
        inc     eax									; increase EAX to get indexing from 1
        mov     ebx, RESIZE_SIZE					; EBX = RESIZE_SIZE
        cdq											; adjust EDX according to a division
        idiv    ebx									; divide EDX:EAX by EBX, EAX = quotient of the division (which row of the new image we are into)
        cmp     edx, 0								; EDX = remainder of the division (which column of the new image we are into - if this is 0, we are in the last column)
        je     .next_row							; if we are in the last column, we have to jump one row down
        add     esi, [esp + 8]						; else we have to jump to the next cell in the new image, so add y to the base of the old image's grid
        pop     ebx									; load the saved registers
        pop     eax
        jmp     .end_new							; go back to the end of the grid loop
      .next_row:									; if we have to go to the next row, we have to add y * ORIG_SIZE to the base of the old image's grid
        mov     eax, [esp + 8]						; EAX = y
        imul    eax, [img_orig_size]				; EAX = y * ORIG_SIZE
        add     esi, eax							; add y * ORIG_SIZE to the base
        sub     esi, [img_orig_size]				; return the base to the beginning of the row
        add     esi, [esp + 8]						; but place it to the 0th column
        pop     ebx									; load the saved registers
        pop     eax
      .end_new:
        cvtsi2ss    xmm0, ebp						; convert the accumulated sum to floating point 
        movd        [edi + eax], xmm0				; place it in the new image's buffer
        add     eax, 4								; step 4 bytes in the new image's buffer
        jmp     .loop1								; loop through all the 
  .end_loop1:										; we are done with the whole map

    cvtsi2ss    xmm1, [esp]							; convert y to floating point number
    shufps      xmm1, xmm1, SHUFPS_ALL				; broadcast it to all elements of XMM1
    mulps       xmm1, xmm1							; calculate the square of y

	mov			eax, 255							
    cvtsi2ss	xmm2, eax							; XMM2 = 255.00
    shufps      xmm2, xmm2, SHUFPS_ALL				; broadcast it to all elements of XMM2

    xor     eax, eax								; EAX = 0, with this we will loop through all floats in the resized map
    .loop_resize:
        cmp     eax, RESIZE_BYTES					; check if we reached the end
        jge     .end_loop_resize
        movups  xmm0, [edi + eax]					; load 4 floats from the resized map, and apply the averaging and scaling formula on them
        divps   xmm0, xmm2							; map_value = sum (map_value) / 255.00
        addps   xmm0, xmm0							; map_value = 2 * map_value
        subps   xmm0, xmm1							; map_value = 2 * map_value - y^2
        divps   xmm0, xmm1							; map_value = (2 * map_value - y^2) / y^2
        movups  [edi + eax], xmm0					; load the new values back to their place
        add     eax, 16								; step 4 * 4 bytes in the resized map
        jmp     .loop_resize						; loop
  .end_loop_resize:

    add		esp, 4									; ignore the saved y value
	
	; set the map properties
	mov		[map_size], dword RESIZE_SIZE			; map_size = RESIZE_SIZE (width/height)	
	mov		[map_channels], dword 1					; only one channel

	test	dword [fs_debug], 0x1					; check if we need to print debug information
	jz		.no_s_debug
    mov     eax, msg_success_prep
    call    io_writestr
  .no_s_debug:
    
	pop		ebp										; load the saved registers
	pop		edi
	pop		esi
	pop		edx
	pop		ecx
	pop		ebx
	pop		eax
    ret
; END (_img_resize)

;======================================================================================================
; Convolutional function. Its two 4 byte parameters (in_channels and out_channels) are strored in the
; structure array at positions [ebp] and [ebp + 4]. It operates on the current map, and creates a new
; map in which it will store the new calculated values.
_conv2d:
	push	eax													; save the used registers to the stack
	push	ebx
	push	ecx
	push	edx
	push	esi
	push	edi

	mov		eax, [ebp + 4]										; EAX = out_channels
	imul	eax, [map_size]										
	imul	eax, [map_size]										; EAX = out_channels * map_size^2
	shl		eax, 2												; EAX = out_channels * map_size^2 * 4, the size of the output map in bytes
	call	mem_alloc											; allocate EAX number of bytes
	mov		[map_buff_new], eax									; save the new buffer pointer
	mov		edi, eax											; EDI = destination = new buffer

	mov		eax, [ebp + 4]										; EAX = out_channels
	imul	eax, [map_size]
	imul	eax, [map_size]										; EAX = out_channels * map_size^2
	shl		eax, 2												; EAX = out_channels * map_size^2 * 4 = number of bytes in output map
	xorps	xmm0, xmm0											; XMM0 = 0 in all its elements
	xor		ecx, ecx											; ECX = 0, with this we loop through all the floats of the new map
	.load_zero:													; we want to zero all elements of the new map
		cmp		ecx, eax										; check if we reached the number of bytes in output map
		jge		.end_load_zero
		movups	[edi + ecx], xmm0								; zero 4 floats in new map
		add		ecx, 16											; step by 4 * 4 bytes
		jmp		.load_zero
  .end_load_zero:

	mov		eax, [ebp]											; EAX = in_channels
	imul	eax, [ebp + 4]										; EAX = in_channels * out_channels
	imul	eax, eax, CONV_KERNEL_SIZE							
	imul	eax, eax, CONV_KERNEL_SIZE							; EAX = in_channels * out_channels * CONV_SZ^2
	shl		eax, 2												; EAX = in_channels * out_channels * CONV_SZ^2 * 4, the number of bytes of weights of the convolution
	add		eax, [net_weight_ptr]								; EAX = previous base + number of bytes of weights
	mov		[bias_ptr], eax										; the bias floats start exactly at this calculated position, after all the weights of the current convolution

	mov		[count1], dword 0									; in count1 we keep track at which convolution filter we are at (there are out_channel in total)
	.convolution:
		mov		eax, [count1]									
		cmp		eax, [ebp + 4]									; check if we reached the number of convolutions
		jge		.end_convolution
		mov		esi, [map_buff]									; we start from the beginning of the old map
		mov		[count2], dword 0								; in count2 we keep track at which depth of the old map we are at (there are in_channel in total)
		.depth:
			mov		eax, [count2]
			cmp		eax, [ebp]									; check if we reached the number of old channels/depths
			jge		.end_depth
			mov		ebx, [net_weight_ptr]						; EBX = the beginning of the weights for the current convolution
			mov		[i1], dword 0								; in i1 we keep track at which row of the new map we are at (there are map_size in total)
			.loop_row:
				mov		eax, [i1]
				cmp		eax, [map_size]							; check if we reached the number of rows
				jge		.end_loop_row 
				mov		[j1], dword 0							; in j1 we keep track at which column of the new map we are at (there are map_size in total)
				.loop_col:
					mov		eax, [j1]
					cmp		eax, [map_size]						; check if we reached the number of columns
					jge		.end_loop_col
					xorps	xmm0, xmm0							; XMM0 = 0, in this we will accumulate the value of the filtering in the current cell of the map
					mov 	edx, CONV_KERNEL_SIZE				
					shr		edx, 1								; EDX = [CONV_SZ / 2] (whole part, so in case if the kernel is 3, EDX will be 1)
					mov		[i2], edx							
					neg 	dword [i2]							; with i2 we start the filtering from -EDX (-1), this indicates the relative row distance from the current cell
					xor		ecx, ecx							; ECX = 0, keeps track of the number of the weight that we need to use
					.filter_row:
						cmp		[i2], edx						; check if we reached the end of the kernel (1) with the rows
						jg		.end_filter_row
						mov		[j2], edx				
						neg 	dword [j2]						; with j2 we start the filtering from -EDX (-1), this indicates the relative column distance from the current cell
						.filter_col:
							cmp		[j2], edx					; check if we reached the end of the kernel (1) with the columns
							jg		.end_filter_col
							mov		eax, [i1]					
							push	eax							; save i1 to stack
							add		eax, [i2]					; calculate absolute row coordinate of kerneling (i1 + i2)
							mov		[i1], eax					; place it in i1
							mov		eax, [j1]
							push	eax							; save j1 to stack
							add		eax, [j2]					; calculate absolute column coordinate of kerneling (j1 + j2)
							mov		[j1], eax					; place it in j1
							xorps	xmm1, xmm1					; XMM1 = 0, the result of the multiplication
							; get the address at row i1 and column j1 from the old map starting at buffer position ESI
							__get_buffer_check		esi, dword [i1], dword [j1], dword [map_size]
							test	eax, eax					; if we got back 0, it means that the position was out of bound, so we are off the edge
							jz		.skip						; no problem, we just keep the value of XMM1 to be zero (this way we don't need to explicitely use padding)
							movss	xmm1, [eax]					; if the address is valid, load the float from the map
						  .skip:
						  	movss	xmm2, [ebx + ecx]			; load the float from the filter
							mulss	xmm1, xmm2					; multiply the map value with the filter weight
							addss	xmm0, xmm1					; add the calculated value to the accumulator
							pop		eax							
							mov		[j1], eax					; restore j1
							pop		eax
							mov		[i1], eax					; restore i1
							inc		dword [j2]					; step to the next column of the kernel
							add		ecx, 4						; step to the next weight in the filter
							jmp		.filter_col					; loop on the kernel columns
					  .end_filter_col:							; we are done with a row in the kernel
						inc		dword [i2]						; step to the next row of the kernel
						jmp		.filter_row						; loop on the kernel rows
				  .end_filter_row:
				  	; get the address at row i1 and column j1 from the new map starting at buffer position EDI
					__get_buffer_nocheck		edi, dword [i1], dword [j1], dword [map_size]
					addss	xmm0, [eax]							; add the previous value on that address to the accumulated sum
					movss	[eax], xmm0							; load the new sum back to its place
					inc		dword [j1]							; step to the next column of the new map
					jmp		.loop_col							; loop on the new map columns
			  .end_loop_col:
			  	inc		dword [i1]								; step to the next row of the new map
				jmp		.loop_row								; loop on the new map rows
		  .end_loop_row:										; we are done with a depth/channel of the old map
		  	mov		eax, CONV_KERNEL_SIZE						; calculate CONV_SZ * CONV_SZ * 4
			imul	eax, eax
			shl		eax, 2										
			add		[net_weight_ptr], eax						; we can skip this many bytes, and move on to the next filter
			mov		eax, [map_size]								; calculate map_size * map_size * 4
			imul	eax, eax
			shl		eax, 2
			add		esi, eax									; we have to move to the next channel, so we need to add the calculated value to the base
			inc		dword [count2]								; step to the next depth/channel of the old map
			jmp		.depth
	  .end_depth:												; we are done with a convolution, now we have to add the bias to the generated channel
	  	mov		eax, [bias_ptr]									; EAX points to the next bias
		movss	xmm0, [eax]										; move the byte bias to XMM0 
		shufps	xmm0, xmm0, SHUFPS_ALL							; broadcast the value to all elements of XMM0
	  	mov		eax, [map_size]									; calculate map_size * map_size * 4, the total size in bytes of the generated new depth/channel
		imul	eax, eax
		shl		eax, 2
		xor		ecx, ecx										; ECX = 0, with this we step on the newly generated depth/channel of the new map
	  	.loop_bias:
		  	cmp		ecx, eax									; if we reached the number of bytes in the channel, we jump out
			jge		.end_loop_bias
			movups	xmm1, [edi + ecx]							; load 4 floats from the channel
			addps	xmm1, xmm0									; add the bias to the value
			movups	[edi + ecx], xmm1							; load the value back to its place
			add		ecx, 16										; step by 4 * 4 bytes
			jmp		.loop_bias
	  .end_loop_bias:
	  	add		[bias_ptr], dword 4								; with the bias pointer, move to the next float
	  	add		edi, eax										; the destination (new map buffer pointer) gets increased by the size of a single depth of the output map (size^2 * 4)
	  	inc		dword [count1]									; step to the next convolution
		jmp		.convolution
  .end_convolution:												; at this point, we are done with all the convolutions

	mov		eax, [map_buff]										; the old map has no use anymore, let's free its buffer
	call	mem_free
	mov		eax, [map_buff_new]									; move the new buffer pointer in the old's place
	mov		[map_buff], eax
	mov		eax, [ebp + 4]										; the number of channels will be the out_channels parameter of the convolution function
	mov		[map_channels], eax
	; NOTE: the map size remains the same

	; with the net_weight_ptr we reached the beginning of the biases, we have to skip these
	mov		eax, [ebp + 4]										; calculate the bytes occupied by the biases, whose number is equal to out_channels							
	shl		eax, 2												; EAX = out_channels * 4
	add		[net_weight_ptr], eax								; skip the calculated number of bytes with the pointer

	pop		edi													; load the saved registers
	pop		esi
	pop		edx
	pop		ecx
	pop		ebx
	pop		eax
	ret
; END (_conv2d)

;======================================================================================================
; Rectified Linear Unit function. It has no parameters. It operates only on the current map, applying
; the following function to all of its elements: map[i] = max(0.00, map[i]).
_relu:
	push	eax									; save the used registers
	push	ebx
	push	ecx

	mov		ebx, [map_size]						; calculate the number of bytes in the map,
	imul	ebx, ebx							; which is equal to map_size^2 * map_channels * 4
	imul	ebx, [map_channels]					; then we store this value in EBX
	shl		ebx, 2

	mov		eax, [map_buff]						; EAX points to the beginning of the map's buffer
	xor		ecx, ecx							; ECX = 0, with this we will iterate through the bytes of the map
	xorps	xmm1, xmm1							; XMM1 = 0 in all of its elements, we will use this for operand of the MAXPS operation

	.apply:
		cmp		ecx, ebx						; check if we reached the total number of bytes
		jge		.end_apply
		movups	xmm0, [eax + ecx]				; load 4 float values in XMM0
		maxps	xmm0, xmm1						; apply the function to these 4 floats
		movups	[eax + ecx], xmm0				; load them back to their place
		add		ecx, 16							; step by 4 * 4 bytes
		jmp		.apply
  .end_apply:

	pop		ecx									; load the saved registers
	pop		ebx
	pop		eax
	ret
; END (_relu)

;======================================================================================================
; Maxpooling function. It has no parameters. It operates on the current map, and creates a new map in
; which it scales down the current map by POOL_KERNEL_SIZE * POOL_KERNEL_SIZE cells. So the new size
; will be MAP_SIZE / POOL_KERNEL_SIZE. It operates with the simplest stride possible, which is equal to
; the kernel size.
_maxpool2d:
	push	eax														; save the used registers to the stack
	push	ebx
	push	ecx
	push	edx
	push	esi
	push	edi

	mov		eax, [map_size]											; calculate map_size^2 * 4, the total bytes of a channel in the old map
	imul	eax, eax
	shl		eax, 2
	push	eax														; push this value to the stack (old_s^2 * 4)

	mov		eax, [map_size]
	mov		ebx, POOL_KERNEL_SIZE
	cdq
	idiv	ebx														; EAX = the new size of the map = MAP_SIZE / KERNEL_SIZE 
	mov		[map_size_new], eax										; load the new size to the memory
	imul	eax, eax												; calculate new_size^2 * 4, the total bytes of a channel in the new map
	shl		eax, 2
	push	eax														; push this value to the stack (new_s^2 * 4)

	imul	eax, [map_channels]										; calculate new_s^2 * channels * 4, the total bytes needed for the new map
	call	mem_alloc												; allocate a memory buffer with EAX bytes
	mov		[map_buff_new], eax										; load the pointer to the buffer to the memory

	mov 	esi, [map_buff]											; source = the beginning of the buffer of the old map
	mov		edi, [map_buff_new]										; destination = the beginning of the buffer of the new map

	mov		[count1], dword 0										; count1 = 0, we will use this to iterate through all the channels of the old/new map
	.loop_depth:
		mov		eax, [count1]
		cmp		eax, [map_channels]									; check if we reached the number of channels, if so, we jump out
		jge		.end_loop_depth
		mov		[i1], dword 0										; i1 = 0, with this we will iterate through the rows of the new map on the current depth
		.loop_new_row:
			mov		eax, [i1]
			cmp		eax, [map_size_new]								; check if we reached the number of rows in the new map
			jge		.end_loop_new_row
			mov		[j1], dword 0									; j1 = 0, with this we will iterate through the columns of the new map on the current depth
			.loop_new_col:
				mov		eax, [j1]
				cmp		eax, [map_size_new]							; check if we reached the number of columns in the new map
				jge		.end_loop_new_col
				mov		eax, FLT_MIN_INF							; calculate the maximal value of this (i1, j1) cell of the new map,
				movd	xmm1, eax									; and by default this maximum is minus infinity
				mov		[i2], dword 0								; i2 = 0, with this we will iterate through the rows of the old map's grid corresponding to the cell of the new map
				.loop_old_row:
					cmp		[i2], dword POOL_KERNEL_SIZE			; check if we reached the number of rows in the pooling kernel
					jge		.end_loop_old_row
					mov		[j2], dword 0							; j2 = 0, with this we will iterate through the columns of the old map's grid corresponding to the cell of the new map
					.loop_old_col:
						cmp		[j2], dword POOL_KERNEL_SIZE		; check if we reached the number of columns in the pooling kernel
						jge		.end_loop_old_col
						mov		eax, [i2]
						push	eax									; save i2 to the stack
						mov		eax, [i1]							; in EAX, calculate the absolute row coordinate of the current cell in the old map
						imul	eax, eax, POOL_STRIDE				; this is equal to i1 * POOL_STRIDE + i2
						add		eax, [i2]
						mov		[i2], eax							; load this calculated value to i2
						mov		eax, [j2]							
						push	eax									; save j2 to the stack
						mov		eax, [j1]							; in EAX, calculate the absolute column coordinate of the current cell in the old map
						imul	eax, eax, POOL_STRIDE				; this is equal to j1 * POOL_STRIDE + j2
						add		eax, [j2]
						mov		[j2], eax							; load this calculated value to j2
						; get the address at row i2 and column j2 from the old map starting at buffer position ESI
						__get_buffer_nocheck	esi, dword [i2], dword [j2], dword [map_size]
						movss	xmm0, [eax]							; load the float from this address to XMM0
						maxps	xmm1, xmm0							; actualize the maximum value in XMM1
						pop		eax									; retrieve the saved value of i2
						mov		[j2], eax
						pop		eax									; retrieve the saved value of j2
						mov		[i2], eax							
						inc		dword [j2]							; step to the next column in the pool kernel
						jmp		.loop_old_col
				  .end_loop_old_col:
				  	inc		dword [i2]								; step to the next row in the pool kernel
					jmp		.loop_old_row
			  .end_loop_old_row:
			  	; get the address at row i1 and column j1 from the new map starting at buffer position EDI
				__get_buffer_nocheck edi, dword [i1], dword [j1], dword [map_size_new]
			  	movss	[eax], xmm1									; load the maximum value which was calculated in the kernel, to its place in the new map
			  	inc		dword [j1]									; step to the next column of the new map
				jmp		.loop_new_col
		  .end_loop_new_col:
		  	inc		dword [i1]										; step to the next row of the new map
			jmp		.loop_new_row
	  .end_loop_new_row:											; we are done with a depth
	  	mov		eax, [esp + 4]										; EAX = old_s^2 * 4
		add		esi, eax											; add a single channels old size to the pointer of the old map, stepping to the next depth with the source ptr
		mov		eax, [esp]											; EAX = new_s^2 * 4
		add		edi, eax											; add a single channels new size to the pointer of the new map, stepping to the next depth with the destination ptr
		inc		dword [count1]										; step to the next depth/channel
		jmp		.loop_depth
  .end_loop_depth:													; we are done with all the depths
	add		esp, 8													; ignore the two 4 byte values stored in the stack

	mov		eax, [map_size_new]										; actualize the size of the current map to be the size of the new map
	mov		[map_size], eax						

	mov		eax, [map_buff]											; the old map goes out of scope, we free the buffer's memory
	call	mem_free
	mov		eax, [map_buff_new]										; actualize the buffer pointer of the current map to be the buffer pointer of the new map
	mov		[map_buff], eax

	pop		edi														; load the saved registers
	pop		esi
	pop		edx
	pop		ecx
	pop		ebx
	pop		eax
	ret
; END (_maxpool2d)

;======================================================================================================
; Fully connected linear layer function. Its two 4 byte parameters (in_features and out_features) are
; stored in the structure array at position [ebp] and [ebp + 4]. It operates on the current map, anc
; creater a new map in which it will store the new calculated values.
_linear:
	push	eax							; save the used registers to the stacl
	push	ebx
	push	ecx
	push	esi
	push	edi

	mov		eax, [ebp + 4]				; EAX = out_features
	shl		eax, 2						; EAX = out_features * 4, the number of bytes needed for the output map
	call	mem_alloc					; allocate EAX number of bytes
	mov		[map_buff_new], eax			; load the pointer to the allocated memory to a variable

	mov		edi, eax					; destination = new, output map
	mov		ebx, [net_weight_ptr]		; EBX points to the beginning of the float weights

	mov		eax, [ebp]					; calculate in_features * out_features * 4, the number of bytes that the layer's weights occupy
	imul	eax, [ebp + 4]				; we have to skip the calculated number of bytes to get to the biases
	shl		eax, 2
	mov		[bias_ptr], ebx				; the bias pointer is now set to the beginning of the weights
	add		[bias_ptr], eax				; by adding to it the bytes occupied by the weights, we get it to point to the beginning of the biases

	mov		[count1], dword 0			; count1 = 0, with this we loop through the output nodes
	.loop_out:
		mov		eax, [count1]
		cmp		eax, [ebp + 4]			; check if we reached the number of output nodes
		jge		.end_loop_out
		mov		esi, [map_buff]			; ESI points to the beginning of the input nodes
		xorps	xmm0, xmm0				; XMM0 = 0, in this we accumulate the product of the input nodes and weights
		mov		[count2], dword 0		; count2 = 0, with this we loop through the input nodes
		.loop_in:
			mov		eax, [count2]		
			cmp		eax, [ebp]			; check if we reached the number of input nodes
			jge		.end_loop_in
			movss	xmm1, [esi]			; load a float from the input nodes into XMM1
			movss	xmm2, [ebx]			; load the corresponding next weight into XMM2
			mulss	xmm1, xmm2			; multiply the input node by the weight
			addss	xmm0, xmm1			; add the product to the accumulator
			add		esi, 4				; move on to the next input node
			add		ebx, 4				; move on to the next weight
			inc		dword [count2]		; step with the in_nodes counter
			jge		.loop_in
	  .end_loop_in:
		mov		eax, [bias_ptr]			; EAX points to the next bias float
		addss	xmm0, [eax]				; add the bias to the accumulated sum
		movss	[edi], xmm0				; load the sum in the next output node
		add		edi, 4 					; move on to the next output node
		add		[bias_ptr], dword 4		; move on to the next bias
		inc		dword [count1]			; step with the out_nodes counter
		jmp		.loop_out
  .end_loop_out:						; at this point all the out_nodes are calculated

	mov		eax, [map_buff]				; the old map with the input nodes goes out of scope, we call free its memory location
	call	mem_free

	mov		eax, [map_buff_new]			; actualize the memory buffer to be the newly calculated map buffer
	mov		[map_buff], eax

	mov		[map_size], dword 1			; actualize the new size - the size needs to be 1, because now we are dealing with the map as being in 1d
	mov		eax, [ebp + 4]				; EAX = out_features
	mov		[map_channels], eax			; make the channel size be equal to the number of output nodes (out_features)

	mov		eax, [ebp + 4]				; calculate out_features * 4, the number of bytes that biases occupy
	shl 	eax, 2						; we have to skip the biases to get to the next weights later on
	add		ebx, eax					; EBX now points to the beginning of the biases, adding to it the size of the biases
	mov		[net_weight_ptr], ebx		; gets us to the beginning of the next set of weights

	pop		edi							; load the saved registers
	pop		esi
	pop		ecx
	pop		ebx
	pop		eax
	ret
; END (_linear)

;======================================================================================================
; Softmax function. It has no parameters. It operates only on the current map, applying the following
; function to all of its elements: map[i] = (e^map[i]) / sum(e^map[j]). This way, the sum of the nodes
; in the map will be 1.00. So if we then multiply them with 100.00, we can interpret them as percentages
; for prediction.
_softmax:
	push	eax							; save the used registers to the stack
	push	ecx

	mov		eax, [map_buff]				; EAX points to the beginning of the map buffer
	xor		ecx, ecx					; ECX = 0, with this we will loop through the floats of the map
	xorps	xmm1, xmm1					; XMM1 = 0, in this we will calculate the sum part of the formula
	.sum_map:
		cmp		ecx, [map_channels]		; check if we reached the total number of bytes in the map
		jge		.end_sum_map
		movss	xmm0, [eax + 4 * ecx]	; load a value from the map into XMM0
		call	exp_ss					; XMM0 = e^XMM0
		movss	[eax + 4 * ecx], xmm0	; load the exponentiated value back to its place
		addss	xmm1, xmm0				; add the calculated value to the sum
		inc		ecx						; step to the next float
		jmp		.sum_map
  .end_sum_map:
  
  	xor		ecx, ecx					; ECX = 0, with this we will loop through the floats of the map again
	.replace:
		cmp		ecx, [map_channels]		; check if we reached the total number of bytes in the map
		jge		.end_replace
		movss	xmm0, [eax + 4 * ecx]	; load a value from the map into XMM0
		divss	xmm0, xmm1				; divide the exponentiated value by the calculated sum
		movss	[eax + 4 * ecx], xmm0	; load the softmaxed value back to its place
		inc		ecx 					; step to the next float
		jmp		.replace
  .end_replace:

	mov		eax, 100
	cvtsi2ss	xmm0, eax				; XMM0 = 100.00

	mov		eax, [map_buff]				; EAX points to the beginning of the map buffer
	xor		ecx, ecx					; ECX = 0, with this we will loop through the float of the map, once again
	.mul100:
		cmp		ecx, [map_channels]		; check if we reached the total number of bytes in the map
		jge		.end_mul100				
		movss	xmm1, [eax + 4 * ecx]	; load a value from the map into XMM1
		mulss	xmm1, xmm0				; multiply the value with XMM0 = 100.00
		movss	[eax + 4 * ecx], xmm1	; load the multiplied value back to its place
		inc		ecx 					; step to the next float
		jmp		.mul100
  .end_mul100:

	pop		ecx							; load the saved registers
	pop		eax
	ret
; END (_softmax)

;======================================================================================================
; Writes the result of the neural process to the screen. It will write the percentages for each digit,
; if the file debugging is set to on. It will always print the digit with the highest prediction.
_write_result:
	push	eax									; save the used registers
	push	ebx
	push	ecx
	push	edx

	call	io_writeln
	mov		eax, msg_header						; print a cute header
	call	io_writestr

	mov		ebx, [map_buff]						; EAX points to the beginning of the map buffer
	mov 	ecx, FLT_MIN_INF					
	movd	xmm1, ecx							; XMM1 gets the minus infinity float value, in this we will calculate the highest map value
	xor		edx, edx						
	dec		edx									; EDX = -1 by default, in this we will calculate the digit associated with the highest map value

	xor		ecx, ecx							; ECX = 0, loop through all the floats in the map
	.loop_channel:
		cmp		ecx, [map_channels]				; check if we reached the end of the map
		jge		.end_loop_channel
		movss	xmm0, [ebx + 4 * ecx]			; load the next float into XMM0
		comiss	xmm0, xmm1						; check if this new float is greater than the maximum value so far
		jb		.not_new_max					; if it isn't greater, skip this part
		movss	xmm1, xmm0						; actualize the maximum to be the current float
		mov		edx, ecx						; actualize the associated digit
	  .not_new_max:
		test	dword [fs_debug], 0x1			; check if we need to display debug information to the screen
		jz		.no_s_debug
		mov		eax, ecx						; in our case, the debug inforamation is the digit and it's float percentage in the map
		call	io_writeint
		mov		eax, msg_dotspace
		call	io_writestr
		call	io_writeflt
		mov		eax, msg_pct
		call	io_writestr
	  .no_s_debug:								; if we don't need to display debug, we just step on to the next float
	  	inc		ecx
		jmp		.loop_channel
  .end_loop_channel:

	; write the final result to the screen
	mov		eax, msg_result
	call	io_writestr
	mov		eax, edx
	call	io_writeint
	call	io_writeln

	mov		eax, msg_header
	call	io_writestr
	call	io_writeln

	pop		edx									; load the saved registers
	pop		ecx
	pop		ebx
	pop		eax
	ret
; END (_write)

;======================================================================================================
; Prints the structure of the neural network to the screen. It doesn't consider what the debug state
; is in, because it is only called at the beginning of the program, at the initialization of the network.
_write_struct:
	push	eax									; save the used registers to the stack
	push	ebp

	call	io_writeln							
	mov		eax, msg_header						
	call	io_writestr							; print a fancy header to the screen
	mov		eax, msg_struct
	call	io_writestr							; print a line of message to the screen

	mov		ebp, net_model						; EBP points to the beginning of the structure array
	.loop_func:
		cmp		[ebp], byte -1					; if we reached the end of the array indicated by -1, we jump out
		je		.end_loop_func
		inc		ebp								; step to the next byte, because we would have to do this in every function
		cmp		[ebp - 1], byte CONV2D_CODE		; search for a match between the known function codes
		je		.conv2d
		cmp		[ebp - 1], byte RELU_CODE
		je		.relu
		cmp		[ebp - 1], byte MAXPOOL2D_CODE
		je		.maxpool2d
		cmp		[ebp - 1], byte LINEAR_CODE
		je		.linear
		cmp		[ebp - 1], byte SOFTMAX_CODE
		je		.softmax
		jmp		.end_loop_func					; if no match found, there has to be some kind of error
	  .conv2d:									
	  	mov		eax, msg_conv2d
		call	io_writestr						; print the name of the conv2d function
		mov		eax, [ebp]
		call	io_writeint						; print its first parameter
		mov		eax, msg_space
		call	io_writestr						; print a space
		mov		eax, [ebp + 4]
		call	io_writeint						; print its second parameter
		call	io_writeln
		add		ebp, 8							; skip the memory occupied by the two parameters
		jmp		.loop_func 
	  .relu:
	  	mov		eax, msg_relu
		call	io_writestr						; print the name of the relu function
		jmp		.loop_func
	  .maxpool2d:
	  	mov		eax, msg_maxpool2d				
		call	io_writestr						; print the name of the maxpool
		jmp		.loop_func
	  .linear:
	  	mov		eax, msg_linear
		call	io_writestr						; print the name of the linear function
		mov		eax, [ebp]
		call	io_writeint						; print its first parameter
		mov		eax, msg_space
		call	io_writestr						; print a space
		mov		eax, [ebp + 4]
		call	io_writeint						; print its second parameter
		call	io_writeln
		add		ebp, 8							; skip the memory occupied ny the two parameters
		jmp		.loop_func
	  .softmax:
	  	mov		eax, msg_softmax			
		call	io_writestr						; print the name of the softmax function
		jmp		.loop_func
  .end_loop_func:

	mov		eax, msg_header						; print the fancy header again
	call	io_writestr
	call	io_writeln

	pop		ebp									; load the saved registers
	pop		eax
	ret
; END (_write_struct)

;======================================================================================================
section .data
	; the buffer pointer for the original image before resizing, and the size of the image (width/height)
    img_orig_buff                   dd                      0
    img_orig_size                   dd                      0

	; variables for dealing with the weights and biases of the neural network, and their size
	net_weight_bytes				dd						0
	net_weight_buff					dd						0
	net_weight_ptr					dd						0
	bias_ptr						dd						0
	
	; this always points to the buffer memory location of the current map
	map_size						dd						0
	map_channels					dd						0
	map_buff						dd						0

	; same as above, but points to the new buffer
	map_size_new					dd						0
	map_buff_new					dd						0

	; some screen debug texts
	msg_success_param				db						'Net parameters read succesfully.', 10, 0
	msg_struct						db						'Net structure:', 10, 0
	msg_success_evaluate			db						'Network functions succesfully evaluated.', 10, 0
    msg_success_prep				db                      'Image preprocessed.', 10, 0
	msg_success_dest 				db						'Net succesfully destroyed.', 10, 0
	msg_error_param					db						'Error: the number of network parameters does not match to the structure.', 10, 0
	msg_error_evaluate				db						'Error while evaluating the net functions.', 10, 0
	msg_result						db						'Recognized digit: ', 0
	msg_header						db						'========================', 10, 0
	msg_dotspace					db						': ', 0
	msg_pct							db						' %', 10, 0
	msg_space 						db						' ', 0
	; names of the neural net functions, also debug information
	msg_conv2d						db						'Conv2d: ', 0
	msg_relu						db						'ReLU', 10, 0
	msg_maxpool2d					db						'MaxPool2d', 10, 0
	msg_linear						db						'Linear: ', 0
	msg_softmax						db						'Softmax', 10, 0

	; the names of the files that contain the 'neural network'
	conv_txt_name					db						'conv_model.txt', 0			; contains the structure of the neural network
	net_bin_name					db						'conv_model.bin', 0			; contains the weights associated with each function of the structure

	; debug state variable - first bit set to 1 if screen debug is enabled, second bit set to 1 if file debug is enabled
	fs_debug						dd						0x1

	; loop counters
	i1								dd						0
	i2								dd						0
	j1								dd						0
	j2								dd						0
	count1							dd						0
	count2							dd						0
; END (section .data)

;======================================================================================================
section .bss
	net_model						resb					100				; neural network structure array
	temp_char						resb					1				; temporal character variable
	temp_str						resb					256				; temporal, globally used string variable
; END (section .bss)
