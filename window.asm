;======================================================================================================
;=										    Created by 						 					  	  =
;=									Kiss Krisztian, january 2022									  =
;======================================================================================================

; Compile:
; nasm -fwin32 window.asm

; dependencies
%include            'gfx.inc'
%include            'util.inc'
%include            'io.inc'

; ascii codes for user events
%define             EXIT_CODE                   23
%define             MOUSE_LEFT                  1
%define 	        BTN_DEC_PEN			        'n'
%define		        BTN_INC_PEN			        'm'
%define             BTN_ACCEPT                  'a'
%define             BTN_CLEAR                   'c'
%define             BTN_TOGGLE_F_DEBUG          'f'
%define             BTN_TOGGLE_S_DEBUG          's'
%define             BTN_RESZ_MIN                '1'    
%define             BTN_RESZ_MAX                '5'
%define             BTN_HELP                    'h'

; constants on which the window's and its elements size and place are calculated
%define             CONST_BASE                  14
%define             CONST_W_MULT                12
%define             CONST_H_MULT                10

; pen size constants
%define 	        PEN_PCT_DEF		            60
%define		        PEN_PCT_STEP	            5
%define 	        PEN_PCT_MIN		            0
%define 	        PEN_PCT_MAX		            100

; color constants (a color occupies 4 bytes of place)
; from most significant to least significant byte: 0, R, G, B
%define             BLACK                       0x00000000
%define             WHITE                       0x00FFFFFF
%define             YELLOW                      0x00FFC800
%define             RED                         0x00FF0000
%define             GREEN                       0x0000FF00

; global functions, must be "extern"-ed in 'windows.inc' to be used in the main function
global win_init, win_draw, win_update, win_destroy, win_disphelp

section .text

;======================================================================================================
; Initializes the window. No in/out parameters.
; Returns fail in CF. CF=0 if everything went well, CF=1 if error occoured.
win_init:
	push    eax                     		; saving the used registers to the stack
	push    ebx
	push	ecx
	push	edx
	push    ebp

	; calculates the absolute base unit of the screen based on the [screen_base] variable (range 1..5)
	mov     ebp, [screen_base]
	imul    ebp, ebp, CONST_BASE    		; B = absolute base = CONST_BASE * [screen_base] (this gives 14, 28, 42, 56 or 70)

	; calculates the width and height of the window based on the calculated absolute base (B) and the given constant base multipliers
	mov     eax, CONST_W_MULT
	imul    eax, ebp                
	mov     [screen_width], eax     		; EAX = [screen_width] = B * CONST_W_MULT 
	mov     ebx, CONST_H_MULT
	imul    ebx, ebp
	mov     [screen_height], ebx    		; EBX = [screen_height] = B * CONST_H_MULT
	xor     ecx, ecx                		; ECX = 0 (window mode window, not fullscreen)
	mov     edx, win_title          		; EDX = pointer to window title string
	call    gfx_init                		; calling the initialization of the window with set EAX, EBX, ECX, EDX registers

	; check for failure
	test    eax, eax
	jz      .error

	; allocates memory for the pixel buffer
	mov     eax, [screen_width]
	imul    eax, [screen_height]
	shl     eax, 2                  		; EAX = width * height * 4, required bytes for the pixels (each pixel is 4 bytes)
	mov     [pixelbuff_bytes], eax  		; saving the calculated number of bytes
	call    mem_alloc
	mov     [pixelbuff], eax        		; saving the allocated memory pointer

	; initializes the drawable 'boxes'
	; each box represents an object (a frame, a button, etc.), with the following properties:
	; starting x coordinate, starting y coordinate, box width, box height, color, fill (0 or 1)
	; each property is a 4 byte value
  .init_boxes:            
	; main frame box, with (B - 1, B - 1) starting coordinates and (B * 8 + 2) width and height and YELLOW color and without fill
	mov     [frame_box], ebp
	dec 	dword [frame_box]
	mov     [frame_box + 4], ebp
	dec 	dword [frame_box + 4]
	mov     eax, ebp
	shl     eax, 3
	add		eax, 2
	mov     [frame_box + 8], eax
	mov     [frame_box + 12], eax
	mov     [frame_box + 16], dword YELLOW
	mov     [frame_box + 20], dword 0

	; a clear black box for the main frame, with (B, B) starting coordinates and (B * 8) width and height and BLACK color and with fill
	mov     [canvas_box], ebp
	mov     [canvas_box + 4], ebp
	mov     eax, ebp
	shl     eax, 3
	mov     [canvas_box + 8], eax
	mov     [canvas_box + 12], eax
	mov     [canvas_box + 16], dword BLACK
	mov     [canvas_box + 20], dword 1

	; clear button box, with (B * 10, B) starting coordinates and B width and height and RED color and with fill
	mov     eax, ebp
	shl     eax, 3
	add     eax, ebp
	add     eax, ebp
	mov     [clear_box], eax
	mov     [clear_box + 4], ebp
	mov     [clear_box + 8], ebp
	mov     [clear_box + 12], ebp
	mov     [clear_box + 16], dword RED
	mov     [clear_box + 20], dword 1

	; accept button box, with (B * 10, B * 3) starting coordinates and B width and height and GREEN color and with fill
	mov     eax, ebp
	shl     eax, 3
	add     eax, ebp
	add     eax, ebp
	mov     [accept_box], eax
	mov     eax, ebp
	add     eax, ebp
	add     eax, ebp
	mov     [accept_box + 4], dword eax
	mov     [accept_box + 8], ebp
	mov     [accept_box + 12], ebp
	mov     [accept_box + 16], dword GREEN
	mov     [accept_box + 20], dword 1

	; initializes the frame buffer which will be fetched to the main function in the event of pressing the accept button
	mov		eax, [canvas_box + 8]
	imul    eax, [canvas_box + 12]
	shl     eax, 2                          ; EAX = frame width * height * 4, the required memory in bytes
	call	mem_alloc
	mov		[framebuff], eax

	; fills the pixel buffer with black color
	mov     eax, [pixelbuff]
	xor     ecx, ecx
	.init_buff:
		cmp     ecx, [pixelbuff_bytes]
		je      .end_init_buff
		mov     [eax + ecx], dword BLACK
		add     ecx, 4
		jmp     .init_buff
  .end_init_buff:

	; draws the main frame box to the pixel buffer
	mov     eax, frame_box
	call    _draw_box

	; draws the clear button to the pixel buffer
	mov     eax, clear_box
	call    _draw_box

	; draws the accept button to the pixel buffer
	mov     eax, accept_box
	call    _draw_box

	; draws all the pixels to the window
	call    win_draw

	; prints the debug information that the screen was initialized, and prints the size of the canvas in pixels
	test     dword [fs_debug], 0x1
	jz      .no_debug
	mov     eax, msg_success_win_init
	call    io_writestr
	mov     eax, [canvas_box + 8]
	call    io_writeint
	mov     eax, str_space
	call    io_writestr
	mov     eax, [canvas_box + 12]
	call    io_writeint
	call    io_writeln
  .no_debug:

	pop     ebp                     		; loading the saved registers
	pop		edx
	pop		ecx
	pop     ebx
	pop     eax
	clc                             		; CF=0, no error
	ret

  .error:
	; prints an error message in the event of an error while initializing the window
	mov     eax, msg_error_win_init
	call    io_writestr
	pop     ebp                     		; loading the saved registers
	pop		edx
	pop		ecx
	pop     ebx
	pop     eax
	stc                             		; CF=1, error occoured
	ret
; END (win_init)

;======================================================================================================
; Draws the pixel buffer to the window. No in/out parameters.
win_draw:
	push    eax                             ; saving the used registers to the stack
	push    ebx
	push    ecx
	push    edx

	; this functions return a memory location in EAX which needs to be filled with
	; the pixels that we want to draw on the screen
	call    gfx_map                         
	xor     ecx, ecx
	mov     ebx, [pixelbuff]

	; we loop through all our pixels, and copy them to the mapped memory location
	.fill_map:
		cmp     ecx, [pixelbuff_bytes]
		jge     .end_fill_map
		mov     edx, [ebx + ecx]
		mov     [eax + ecx], edx
		add     ecx, 4
		jmp     .fill_map
  .end_fill_map:

	; when we are done with copying, we have to unmap the memory location
	; so that the changes take place, then we can call the draw function
	call    gfx_unmap
	call    gfx_draw

	pop     edx                             ; loading back the saved registers 
	pop     ecx
	pop     ebx
	pop     eax
	ret
; END (win_draw)

;======================================================================================================
; Handles the events invoked by the user. The events can be pressing the buttons on the screen,
; painting to the screen, pressing some keyboard buttons or closing the window.
; If the user paints on the screen, only the canvas part of it can be affected, and the pixel buffer
; will be updated accordingly.
; The function doesn't have input, but is has output in EAX, EBX, ECX, EDX registers.
; EAX - the first bit will indicate wether the window was closed or not, the second bit will indicate
;       if the start_net function should be invoked in the main function.
; EBX - frame buffer memory location in case of pressing the accept button, otherwise unchanged
; ECX - the size (width/height) of the frame in case of pressing the accept button, otherwise unchanged
; EDX - the state of the debug to file/screen variable ([fs_debug]) in case of accept button,
;       otherwise unchanged
win_update:
	push	ebp									; save this used register to the stack

	; DL = 0, it will hold the events that are important in the main function (and some other local things too)
	xor     dl, dl   

	cmp     [pen_down], byte 0              	; we check if the pen is down
	je      .event_loop

	; it it is down, we have to check if the mouse is pressed on the canvas
	call    gfx_getmouse                    	; mouse coordinates (x, y) in (EAX, EBX)
	mov     ecx, canvas_box            			; the clear box is basically our canvas
	call    _check_boundary                 	; we check if (EAX, EBX) is inside the box
	jnc     .event_loop
	
	; if it is pressed on the canvas, we have to draw a dot
	call    _draw_dot

	.event_loop:
		call    gfx_getevent                    ; returns an event in EAX
		mov		ebp, eax						; move the event value to EBP
		test    ebp, ebp                        ; if EBP = 0, there are no more events
		jz      .end_event_loop

		cmp     ebp, EXIT_CODE      			; if the windows was closed, the first bit of dl will be set
		jne     .mouse_pressed
		or		dl, 0x1
		jmp     .event_loop                     ; we still query all other events

	  .mouse_pressed:
		cmp     ebp, MOUSE_LEFT     			; if the left mouse button was pressed
		jne     .mouse_released
		mov     [pen_down], byte 1              ; we have to indicate that the drawing pen is down
		call    gfx_getmouse                    ; we check where the mouse was pressed (EAX, EBX) = (x, y)
		mov     ecx, clear_box                  ; we check if the mouse was pressed on the clear button (red)
		call    _check_boundary
		jnc     .box_accept                     ; if it was not, we continue to check something else
		test	dword [fs_debug], 0x1    		; first let's see if we need to display some debug information to the screen
		jz      .no_debug1
		mov     eax, msg_clr_win                ; if we do, we print that the windows was cleared
		call    io_writestr
	  .no_debug1:
		mov     eax, canvas_box            		; either way, we have to draw the canvas black
		call    _draw_box
		jmp		.event_loop                     ; check the next event

	  .box_accept:
		mov     ecx, accept_box                 ; we are still in the mouse pressed condition, we check if the accept button was pressed (green)
		call    _check_boundary 
		jnc     .mouse_released                 ; if not, let's check the next thing
		call	_copy_canvas                	; if it was pressed, we have to copy the pixels of the canvas to the frame buffer
		or		dl, 0x2                         ; we indicate that the accept was pressed
		jmp		.event_loop                     ; check the next event

	  .mouse_released:  
		cmp     ebp, -MOUSE_LEFT    			; if the mouse was released, we should also take the pen up
		jne     .clear  
		mov     [pen_down], byte 0  
		jmp		.event_loop                     ; check the next event

	  .clear:   
		cmp     ebp, BTN_CLEAR      			; check if the keyboard button for clear was pressed
		jne     .accept 
		test	dword [fs_debug], 0x1    		; from now on, it's the same as above for clear
		jz      .no_debug2
		mov     eax, msg_clr_win
		call    io_writestr
	  .no_debug2:
		mov     eax, canvas_box
		call    _draw_box
		jmp		.event_loop

	  .accept:
		cmp     ebp, BTN_ACCEPT     			; check if the keyboard button for accept was pressed
		jne     .help   
		call	_copy_canvas                	; same as above for accept
		or		dl, 0x2 
		jmp		.event_loop 

	  .help:    
		cmp     ebp, BTN_HELP       			; check if the help button was pressed
		jne     .inc_pen    
		call    win_disphelp                    ; if yes, display help
		jmp     .event_loop                     ; check the next event

	  .inc_pen: 
	  	cmp		ebp, BTN_INC_PEN    			; check if the pen is needed to be increased
		jne		.dec_pen
		add		[pen_pct], dword PEN_PCT_STEP   ; increase the pen size with constant ammount
		cmp		[pen_pct], dword PEN_PCT_MAX    ; if it got above maximum value, take it back to the maximum value
		jle		.end_inc_pen
		mov		[pen_pct], dword PEN_PCT_MAX
	  .end_inc_pen:
		test	dword [fs_debug], 0x1    		; if we need to display debug info, we display it
		jz      .no_debug3
	  	mov		eax, msg_pen_inc
		call	io_writestr
		mov		eax, [pen_pct]
		call	io_writeint
		mov		eax, msg_pen_pct
		call	io_writestr
	  .no_debug3:
		jmp		.event_loop                     ; check the next event
	
	  .dec_pen:
	  	cmp		ebp, BTN_DEC_PEN     			; check if the pen size is needed to be decreased
		jne		.resize
		sub		[pen_pct], dword PEN_PCT_STEP   ; decrease the size with constant ammount
		cmp		[pen_pct], dword PEN_PCT_MIN    ; if it got below minimum, take it back to minimum
		jge		.end_dec_pen
		mov		[pen_pct], dword PEN_PCT_MIN
	  .end_dec_pen:
		test	dword [fs_debug], 0x1    		; check for need for debug
		jz      .no_debug4
	  	mov		eax, msg_pen_dec
		call	io_writestr
		mov		eax, [pen_pct]
		call	io_writeint
		mov		eax, msg_pen_pct
		call	io_writestr
	  .no_debug4:
		jmp		.event_loop                     ; check the next event
	
	  .resize:
		cmp     ebp, BTN_RESZ_MIN   			; check if the button is betweem the given (RESZ_MIN..RESZ_MAX) range for resize
		jl      .toggle_f_debug
		cmp     eax, BTN_RESZ_MAX
		jg      .toggle_f_debug
		sub     eax, '0'                        ; if it is, we convert from digital character to real numeric value
		cmp     eax, [screen_base]              ; we check if the new value is the same as the old
		je      .event_loop                     ; if it is, we don't resize, we check the next event
		mov     [screen_base], eax              ; we update the base multiplier
		call    win_destroy                  	; we destroy the window
		call    win_init                     	; then create a new one, with the new size
		jmp     .event_loop                     ; check the next event

	  .toggle_f_debug:							; check if the toggle button for file debugging was pressed
		cmp     ebp, BTN_TOGGLE_F_DEBUG  
		jne     .toggle_s_debug
		xor     [fs_debug], dword 0x2    		; if yes, invert the second bit of the toggle variable
		test	dword [fs_debug], 0x1    		; check if we need to debug to the screen
		jz      .no_debug5
		mov     eax, msg_toggle_f_debug1        ; if yes, print that the file debugging was toggled
		call    io_writestr
		mov     ebx, [fs_debug]      
		shr     ebx, 1                          ; EBX=0 if file debugging is OFF, EBX=1 if ON
		lea     eax, [msg_toggle_f_debug2 + 4 * ebx] ; with this little hack we save a conditional jump
		call    io_writestr                 
		call    io_writeln
	  .no_debug5:
		jmp     .event_loop                     ; check the next event
	
	  .toggle_s_debug:							; check if the toggle button for screen debugging was pressed
		cmp     ebp, BTN_TOGGLE_S_DEBUG         
		jne     .event_loop
		xor     [fs_debug], dword 0x1    		; if yes, invert the first bit of the toggle variable
		mov     eax, msg_toggle_s_debug1        ; print the corresponding message
		call    io_writestr
		mov     ebx, [fs_debug]
		and     ebx, 0x1                        ; same hack as above
		lea     eax, [msg_toggle_s_debug2 + 4 * ebx]
		call    io_writestr
		call    io_writeln
		jmp     .event_loop
  .end_event_loop:

	mov		al, dl                              ; save the collected flags to AL (EAX)
	test    dl, 0x2                             ; if the accept button was pressed, we also need to load some stuff to the registers
	jz      .no_accept
	mov		ebx, [framebuff]
	mov		ecx, [canvas_box + 8]
	mov     edx, [fs_debug]
  .no_accept:
	pop		ebp									; load the saved register
	ret
; END (win_update)

;======================================================================================================
; Destroys the window, frees the memory of the allocated memory buffers, prints debug message if needed
win_destroy:
	push    eax
	call    gfx_destroy                     ; it takes no arguments
	mov     eax, [pixelbuff]                ; free memory of pixel buffer
	call    mem_free    
	mov     eax, [framebuff]                ; free memory of frame buffer
	call    mem_free    
	test    dword [fs_debug], 0x1    		; check is screen debug is enabled
	je      .no_debug
	mov     eax, msg_success_win_dest
	call    io_writestr
  .no_debug:                                ; if no, simply return
	pop     eax
	ret
; END (close_window)

;======================================================================================================
; Displays the welcome/help message to the screen. It is starting from memory address msg_help1,
; and there is a declaration for each line, but they don't contain the terminating character (0) and
; they are placed continuously in the memory, so they will be all printed at once.
win_disphelp:
	push    eax
	mov     eax, msg_help1
	call    io_writestr
	call    io_writeln
	pop     eax
	ret
; END (win_disphelp)

;======================================================================================================
; Returns in ECX the absolute buffer position of (x, y) = (EAX, EBX) point in the pixel buffer
_get_buffer_position:
	push    eax
	mov     ecx, [screen_width]     ; ECX = W
	shl     ecx, 2                  ; ECX = W * 4
	imul    ecx, ebx                ; ECX = x * W * 4
	add     ecx, [pixelbuff]        ; ECX = base_of_memory + x * W * 4
	shl     eax, 2                  ; EAX = y * 4
	add     ecx, eax                ; ECX = base_of_memory + (x * W + y) * 4
	pop     eax
	ret
; END (_get_buffer_position)

;======================================================================================================
; Draw line on y=EAX axis between x=EBX..ECX axis points with EDX color (EBX < ECX!)
_draw_horizontal_line:
	push    ebx
	push    ecx

	push    ecx
	xchg    eax, ebx                ; swap y with start_x
	call    _get_buffer_position    ; get the buffer index of (start_x, y) in ECX
	xchg    eax, ebx                ; swap them back
	push    ecx                     ; save the index of start point to the stack

	mov     ebx, [esp + 4]          ; EBX = end_x
	xchg    eax, ebx                ; swap y with end_x
	call    _get_buffer_position    ; get the buffer index of (end_x, y) in ECX
	xchg    eax, ebx                ; swap them back
	pop     ebx                     ; EBX = index of start point
	add     esp, 4                  ; move the stack pointer by 4 bytes

	.loop_pixel:                    ; loop through all the pixels on the horizontal line
		cmp     ebx, ecx            ; if we reached the index of end point, we jump out
		jg      .end_loop_pixel
		mov     [ebx], edx          ; copy the color to the location of EBX
		add     ebx, 4              ; step 4 bytes with EBX
		jmp     .loop_pixel
   .end_loop_pixel:

	pop     ecx
	pop     ebx
	ret
; END (_draw_horizontal_line)

;======================================================================================================
; Draw line on x=EAX axis between y=EBX..ECX y axis points with EDX color (EBX < ECX!)
_draw_vertical_line:
	push    eax
	push    ebx
	push    ecx

	push    ecx
	call    _get_buffer_position    ; get the buffer index of (x, start_y) in ECX
	push    ecx                     ; save the buffer index of start point to the stack

	mov     ebx, [esp + 4]          ; EBX = end_y
	call    _get_buffer_position    ; get the buffer index of (x, end_y) in ECX
	pop     ebx                     ; EBX = buffer index of start point
	add     esp, 4                  ; move the stack pointer by 4 bytes

	mov     eax, [screen_width]     
	shl     eax, 2                  ; increment by WIDTH * 4 bytes, thus stepping a whole row

	.loop_pixel:
		cmp     ebx, ecx            ; if reached the end index, we jump out
		jg      .end_loop_pixel
		mov     [ebx], edx          ; copy the color to the location of EBX
		add     ebx, eax            ; add a whole row to EBX, thus remaining in the same column but moving down
		jmp     .loop_pixel
  .end_loop_pixel:

	pop     ecx
	pop     ebx
	pop     eax
	ret
; END (_draw_vertical_line)

;======================================================================================================
; Draws a box with EAX memory location to the pixel buffer (see the win_init function to know
; what a box is)
_draw_box:
	push    eax
	push    ebx
	push    ecx
	push    edx
	push    edi
	push    esi

	mov     edi, eax                    ; EDI = address of the box
	mov     eax, [edi + 4]              ; EAX = y coordinate
	mov     ebx, [edi]                  ; EBX = x starting coordinate
	mov     ecx, [edi]              
	add     ecx, [edi + 8]              ; ECX = x starting + width = x ending coordinate
	mov     edx, [edi + 16]             ; EDX = color
	call    _draw_horizontal_line       ; draw a horizontal line on EAX y axis between EBX, ECX x axis points

	; check if the box needs to be filled with the given color
	mov     esi, [edi + 20]
	test    esi, esi
	jz      .no_fill
	mov     esi, [edi + 4]              ; ESI = y coordinate
	add     esi, [edi + 12]             ; ESI = y coordinate + height = ending y coordinate

	.fill:
		inc     eax                     ; step one on the y axis
		cmp     eax, esi                ; check if we reached the ending y coordinate
		jg      .end
		call    _draw_horizontal_line   ; draw a horizontal line on EAX y axis between EBX, ECX x axis points
		jmp     .fill                   ; basically, we draw horizontal stripes which will fill the area

  .no_fill:
	add     eax, [edi + 12]             ; EAX = y + height = y ending coordinate
	call    _draw_horizontal_line       ; draw horizontal line on EAX y axis between EBX, ECX x axis points

	mov     eax, [edi]                  ; EAX = x coordinate
	mov     ebx, [edi + 4]              ; EBX = y starting coordinate
	mov     ecx, [edi + 4]
	add     ecx, [edi + 12]             ; ECX = y starting + height = y ending coordinate
	call    _draw_vertical_line         ; draw vertical line on EAX x axis between EBX, ECX y axis points

	add     eax, [edi + 8]              ; EAX = x + width = x coordinate
	call    _draw_vertical_line         ; draw vertical line on EAX x axis between EBX, ECX y axis points

  .end:                                 ; if the box was filled with horizontal lines, the vertical part is skipped and it jumps here
	pop     esi
	pop     edi
	pop     edx
	pop     ecx
	pop     ebx
	pop     eax
	ret
; END (_draw_box)

;======================================================================================================
; Check if the (x, y) = (EAX, EBX) point is inside of the box whose memory location is in ECX
; If the point is inside CF=1, if it isn't inside CF=0. For the structure of a box, check win_init
_check_boundary:
	push    eax
	push    ebx

	cmp     eax, [ecx]          ; if x < starting_x, it is not inside
	jl      .no_contain
	sub     eax, [ecx + 8]  
	cmp     eax, [ecx]          ; if x > starting_x + width, it is not inside
	jg      .no_contain

	cmp     ebx, [ecx + 4]      ; if y < starting_y, it is not inside
	jl      .no_contain
	sub     ebx, [ecx + 12]
	cmp     ebx, [ecx + 4]      ; if y > starting_y + height, it is not inside
	jg      .no_contain

	pop     ebx
	pop     eax
	stc                         ; if reached this point, it is inside, CF=1
	ret

  .no_contain:
	pop     ebx
	pop     eax
	clc                         ; it is not inside, CF=0
	ret
; END (_check_boundary)

;======================================================================================================
; Returns the euclidean distance between (EAX, EBX) and (ECX, EDX) points in XMM0 register
_get_dist:
	push    eax
	push    ebx

	movd    eax, xmm1
	push    eax
	mov     eax, [esp + 8]

	sub     eax, ecx            	; EAX = x difference
	sub     ebx, edx            	; EBX = y difference

	cvtsi2ss    xmm0, eax       	; convert x_diff to XMM0
	cvtsi2ss    xmm1, ebx       	; convert y_diff to XMM1
	mulss       xmm0, xmm0      	; square XMM0
	mulss       xmm1, xmm1      	; square XMM1
	addss       xmm0, xmm1      	; XMM0 = sum of the squares
	sqrtss      xmm0, xmm0      	; XMM0 = square root of the sum of the squares = distance between the points

	pop     eax
	movd    xmm1, eax

	pop     ebx
	pop     eax
	ret
; END (_get_dist)

;======================================================================================================
; Draws a dot (circle) with (EAX, EBX) center and ABSOLUTE_BASE * [pen_pct] radius size
_draw_dot:	
	push    eax                             ; save some registers
	push    ebx
	push    ecx
	push    edx
	push	ebp

	mov			eax, [pen_pct]
	cvtsi2ss	xmm1, eax                   ; XMM1 = percentage

	mov			eax, 100
	cvtsi2ss	xmm2, eax                   ; XMM2 = 100.00
	divss		xmm1, xmm2                  ; XMM1 = percentage / 100.00 = pct (in range 0.00-1.00)
	mov			eax, [screen_base]
	imul        eax, eax, CONST_BASE        ; EAX = CONST_BASE * [screen_base] = ABSOLUTE_BASE
	cvtsi2ss	xmm2, eax                   ; XMM2 = ABSOLUTE_BASE
	mulss		xmm1, xmm2                  ; XMM1 = pct * ABSOLUTE_BASE = radius
	cvttss2si	ebp, xmm1                   ; EBP = integer value of radius

	mov     ecx, [esp + 16]                 ; ECX = original value of EAX
	mov     edx, [esp + 12]                 ; EDX = original value of EBX

	push    ecx
	sub     [esp], ebp                      ; push (center_x - radius) to stack
	push    edx
	sub     [esp], ebp                      ; push (center_y - radius) to stack
	shl     ebp, 1                          ; EBP = 2 * radius
	push    ebp
	add     [esp], ecx                      ; push (center_x + radius) to stack
	push    ebp
	add     [esp], edx                      ; push (center_y + radius) to stack

	mov     eax, [esp + 12]                 ; EAX = x of upper left corner of the scanned grid
	.loop_x:
		cmp     eax, [esp + 4]              ; check if EAX got to the x of bottom right corner of the grid
		jg      .end_loop_x                 ; if yes, jump out
		mov     ebx, [esp + 8]              ; EBX = y of upper left corner of the scanned grid
		.loop_y:
			cmp     ebx, [esp]              ; check if EBX got to the y of bottom right corner of the grid
			jg      .end_loop_y             ; if yes, jump out
			push    ecx                     ; save ECX to stack
			mov     ecx, canvas_box    		; we have to check if (EAX, EBX) point is inside of the canvas = canvas_box box
			call    _check_boundary
			jnc     .no_draw1               ; if not, we have to skip the drawing and jump to a place where we still pop out the saved ECX
			pop     ecx                     ; retrieve ECX
			call    _get_dist               ; get distance of (EAX, EBX) and (ECX, EDX) in XMM0
			comiss  xmm0, xmm1              ; compare the distance to the radius
			ja      .no_draw2               ; if the distance is above the radius, we don't draw
			push    ecx                     ; save ECX to stack
			call    _get_buffer_position    ; get buffer index position of (EAX, EBX) in ECX
			mov     [ecx], dword WHITE      ; paint the corresponding pixel white
		  .no_draw1:
			pop     ecx                     ; retrieve ECX, whether it was saved before _get_dist or before _check_boundary
		  .no_draw2:
			inc     ebx                     ; increase the y coordinate
			jmp     .loop_y                 ; go to next "row"
	  .end_loop_y:
		inc     eax                         ; increase the x coordinate
		jmp     .loop_x                     ; go to next column
  .end_loop_x:
  
	add     esp, 16                         ; ignore the stuff saved to the stack
	
	pop		ebp                             ; retrieve the saved registers
	pop		edx
	pop		ecx
	pop		ebx
	pop		eax
	ret
; END (_draw_dot)

;======================================================================================================
; Copies the content of the main drawing frame to the buffer located on [framebuff] memory address.
; No in/out parameters. It is called by the win_update function, when the canvas is needed to
; be fetched to the main function, then to the neural network.
_copy_canvas:
	push	eax                                 ; saving the used registers to the stack
	push	ebx
	push	ecx
	push	edx

	mov		eax, [canvas_box]                   ; EAX = x coordinate of upper left corner of canvas = x_start
	push	eax
	mov		ebx, [canvas_box + 4]               ; EBX = y coordinate of upper left corner of canvas = y_start
	push	ebx
	add		eax, [canvas_box + 8]               ; EAX = x_start + width = x_end
	push	eax
	add		ebx, [canvas_box + 12]              ; EBX = y_start + height = y_end
	push	ebx

	mov		edx, [framebuff]                    ; EDX = start of destination memory location

	mov		ebx, [esp + 8]                      ; EBX = y_start
	.loop_y:
		cmp		ebx, [esp]                      ; check if we reached y_end
		jge		.end_loop_y
		mov		eax, [esp + 12]                 ; EAX = x_start
		.loop_x:
			cmp		eax, [esp + 4]              ; check if we reached x_end
			jge		.end_loop_x 
			call	_get_buffer_position        ; get the buffer index of the original screen from point (EAX, EBX) in ECX
			mov		ecx, [ecx]                  ; copy the pixel from original into ECX
			mov		[edx], cl                   ; the pixel is either BLACK (0, 0, 0) or WHITE (255, 255, 255), so the first byte is enough
			inc		edx                         ; step a byte in the destination buffer
			inc		eax                         ; step one on the x axis
			jmp		.loop_x
	  .end_loop_x:
		inc		ebx                             ; step one on the y axis
		jmp		.loop_y
  .end_loop_y:

	add		esp, 16                             ; ignore the used stuff in the stack
	pop		edx                                 ; retrieve the saved registers
	pop		ecx
	pop		ebx
	pop		eax
	ret
; END (_copy_canvas)

;======================================================================================================
section .data
	; title of the window
	win_title           	        db                  'Convolutional Neural Network', 0

	; messages for debugging
	msg_error_win_init              db                  'Error while initializing the window.', 10, 0
	msg_success_win_init            db                  'Window succesfully initialized with following frame sizes: ', 0
	msg_success_win_dest            db                  'Window succesfully destroyed.', 10, 0
	msg_clr_win                     db                  'Window cleared.', 10, 0
	msg_pen_inc				        db					'Pen size increased to ', 0
	msg_pen_dec				        db					'Pen size decreased to ', 0
	msg_pen_pct				        db					'%', 10, 0
	msg_toggle_f_debug1             db                  'Debugging net process to file set to ', 0
	msg_toggle_f_debug2             db                  'OFF', 0
	msg_toggle_f_debug3             db                  'ON', 0
	msg_toggle_s_debug1             db                  'Debugging activity to screen set to ', 0
	msg_toggle_s_debug2             db                  'OFF', 0
	msg_toggle_s_debug3             db                  'ON', 0
	str_space                       db                  ' ', 0

	; help message (NOTE: it is a continuous part of memory, not with 0 terminated strings, but with many ENDLN terminated strings
	msg_help1                       db                  '  /=====================================================\', 10
	msg_help2                       db                  '  |              Digit recognition using                |', 10
	msg_help3                       db                  '  |            convolutional neural network             |', 10
	msg_help4                       db                  '  =======================================================', 10
	msg_help5                       db                  '  |                       Usage:                        |', 10
	msg_help6                       db                  '  |                                                     |', 10
	msg_help7                       db                  '  |     Draw a one digit number with your mouse on      |', 10
	msg_help8                       db                  '  |  the canvas of the grafical window, then press the  |', 10
	msg_help9                       db                  '  |    green button to see the result of the neural     |', 10
	msg_help10                      db                  '  |  network. Press the red button to reset the canvas. |', 10
	msg_help11                      db                  '  |=====================================================|', 10
	msg_help12                      db                  '  |             Additional key features:                |', 10
	msg_help13                      db                  '  |       A            -         test the input         |', 10
	msg_help14                      db                  '  |       C            -          clear canvas          |', 10
	msg_help15                      db                  '  |     1 - 5          -     resize grafical screen     |', 10
	msg_help16                      db                  '  |      N/M           -   decrease/increase pen size   |', 10
	msg_help17                      db                  '  |       S            - toggle debug process on screen |', 10
	msg_help18                      db                  '  |       F            -  toggle debug net map to file  |', 10
	msg_help19                      db                  '  |       H            -       display this message     |', 10
	msg_help20                      db                  '  =======================================================', 10
	msg_help21                      db                  '  |          Kiss Krisztian, january 2022               |', 10
	msg_help22                      db                  '  \=====================================================/', 10, 0
	; this is the end of the help message

	pixelbuff          		        dd                  0                   ; memory address of buffer used by the screen itself
	pixelbuff_bytes                 dd                  0                   ; number of bytes that the pixel buffer occupies
	framebuff				        dd					0                   ; memory address of buffer which will onlyc only the canvas pixels

	; box datastructure, where each field has a 4 byte integer property:
	; start_x, start_y, width, height, color, fill (0 or 1)
	frame_box           	        dd                  0, 0, 0, 0, 0, 0    ; the frame of the canvas
	canvas_box     	                dd                  0, 0, 0, 0, 0, 0    ; the canvas itself
	clear_box           	        dd                  0, 0, 0, 0, 0, 0    ; red button
	accept_box          	        dd                  0, 0, 0, 0, 0, 0    ; green button

	pen_down            	        db                  0                   ; boolean variable, indicates wether the pen is up or down
	pen_pct					        dd					PEN_PCT_DEF         ; pen size indicator, its value will always lay between PEN_PCT_MIN (0) and PEN_PCT_MAX (100)

	; debug state variable - first bit set to 1 if screen debug is enabled, second bit set to 1 if file debug is enabled
	fs_debug                 		dd                  0x1

	; window size indicators
	screen_base                     dd                  3                   ; multiplier for the absolute base calculation, can be changed by the user on runtime
	screen_width                    dd                  0                   ; the values of width and height are also calculated on runtime,
	screen_height                   dd                  0                   ; and are based on the multiplier mentioned above
; END (section .data)