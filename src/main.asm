;======================================================================================================
;=										    Created by 						 					  	  =
;=									Kiss Krisztian, january 2022									  =
;======================================================================================================

; Compile:
; nasm -fwin32 main.asm

%include 			'window.inc'
%include 			'io.inc'
%include 			'net.inc'

global main

section .text

;======================================================================================================
main:
		call    win_disphelp				; print the welcome message to the screen

		call    win_init					; initialize the window
		jc      .return						; if win_init returned error, we terminate the program

		call    net_init					; initialize the neural network
		jc      .end_main_loop				; if we couldn't initialize the net, we still have to close the window and free the net buffer

		.main_loop:							; loop until the windows doesn't close
				call    win_draw			; draw the contents of the screen to the windows
				call    win_update			; update, and also handle user events
				test    al, 0x1				; if the first bit of AL is set, then the windows was closed, and we need to terminate the program
				jnz     .end_main_loop
				test    al, 0x2				; if the second bit of AL is set, then the drawn image needs to be evaluated by the neural network
				jz      .main_loop			; otherwise we have to jump back to the loop
				call    net_start			; calling the evaluating function
				jmp     .main_loop			; jumping back to the beginning of the loop

	.end_main_loop:
		call    win_destroy					; we have to close the window, and free the allocated memory for the network
		call    net_destroy					; we have to free the allocated memory for the network

	.return:								; in case of error, only return
		mov		eax, msg_pause				; display pause message
		call	io_writestr
		call	io_readint					; wait until the user presses the enter key
		ret
; END (main)

;======================================================================================================
section .data
	msg_pause					db					'Press enter to exit . . .', 0
; END (.data)
