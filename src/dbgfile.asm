;======================================================================================================
;=										    Created by 						 					  	  =
;=									Kiss Krisztian, january 2022									  =
;======================================================================================================

; Compile:
; nasm -fwin32 dbgfile.asm

; dependencies
%include            'io.inc'
%include            'util.inc'

%define             FLOAT_PREC                  6           ; how many decimals do we want after the decimal point
%define             NEW_LINE_CHR                10          ; constant for newline character ('\n')

; global functions, must be "extern"-ed in 'dbgfile.inc' to be used in a caller function
global dbgfile_init, dbgfile_write, dbgfile_destroy

section .text

;======================================================================================================
; Initializes the writing to the output file
; In: EAX - debug parameters (the first bit of EAX is set if messages need to be printed to the screen)
dbgfile_init:
    push    eax                                 ; sace the used registers to the stack
    push    ebx

    mov     [fs_debug], eax                     ; save the debug state variable

    ; open the debug file
    mov     eax, dbg_file_name
    xor     ebx, ebx
    inc     ebx                                 ; EBX = 1, we want to open the file for writing
    call    fio_open                            ; we call the function from the util library, it takes EAX and EBX as parameters
    mov     [file_handle], eax                  ; the functions returns the file handler in EAX, we save this

    test    dword [fs_debug], 0x1               ; check if we need to print some debug information to the screen
    jz      .no_s_debug
    mov     eax, msg_success_open
    call    io_writestr
  .no_s_debug:

    pop     ebx                                 ; load the saved registers
    pop     eax
    ret
; END (dbgfile_init)

;======================================================================================================
; In parameters:
; EAX - memory pointer to 32 bit float buffer, which needs to be written
; EBX - size (width and height) of map
; ECX - total channels of map
; In total, the buffer should contain size * size * channel 32 bit floating point numbers
dbgfile_write:
    push    eax                                 ; save the used registers to the stack
    push    ebx
    push    ecx
    push    edx
    push    edi

    xor     ecx, ecx                            ; ECX = 0, this is the counter which iterates through the channels
    .loop_channels:
        cmp     ecx, [esp + 8]                  ; if we reached the number of channels, we jump out
        jge     .end_loop_channels
        xor     ebx, ebx                        ; EBX = 0, this is the counter which iterates through the size (height/rows) of the map
        .loop_row:
            cmp     ebx, [esp + 12]             ; if we reached the number of rows, we jump out
            jge     .end_loop_row
            mov     edi, string                 ; EDI = beginning of the memory location allocated for the string
            xor     edx, edx                    ; EDX = 0, this is the counter which iterates through the size (width/columns) of the map
            .loop_col:
                cmp     edx, [esp + 12]         ; if we reacher the number of columns, we jump out
                jge     .end_loop_col
                movss   xmm0, [eax]             ; move a single 4 byte floating point number from EAX memory location to XMM0
                call    _add_float              ; convert XMM0 to string by concatenating it to the global string ending on EDI
                mov     [edi], byte ' '         ; add a space to the string
                inc     edi
                add     eax, 4                  ; step 4 bytes in the buffer
                inc     edx                     ; step to the next column
                jmp     .loop_col               ; loop through the columns
          .end_loop_col:                        ; we reached the end of a row
            mov     [edi], byte NEW_LINE_CHR    ; put a newline character to the end of the string
            inc     edi
            call    _put_string                 ; write the built string to the file
            inc     ebx                         ; step to the next row
            jmp     .loop_row                   ; loop through the rows
      .end_loop_row:                            ; we reached the end of a channel
        call    _put_newline                    ; write an extra newline character to the file at the end of a channel
        inc     ecx                             ; step to the next channel
        jmp     .loop_channels                  ; loop through the channels
  .end_loop_channels:                           ; we reached the end of all channels

    call    _put_newline                        ; we put another extra newline character, so the maps will be separated by 2 newline characters

    pop     edi                                 ; load the saved registers
    pop     edx
    pop     ecx
    pop     ebx
    pop     eax
    ret
; END (dbgfile_write)

;======================================================================================================
; Closes the opened debug file. No in/out parameters.
dbgfile_destroy:
    push    eax

    mov     eax, [file_handle]
    call    fio_close                           ; closing the file, by placing the file handler into EAX

    test    dword [fs_debug], 0x1        		; check if we need to print some debug information to the screen
    jz      .no_s_debug
    mov     eax, msg_success_close
    call    io_writestr
  .no_s_debug:

    pop     eax
    ret
; END (dbgfile_destroy)

;======================================================================================================
; Writes the globally used string starting at memory address 'string', and ending one byte before memory
; location stored in EDI, to the debug file.
_put_string:
    push    eax                                 ; save the used registers to the stack
    push    ebx
    push    ecx

    ; calculate the length of the string in bytes, by subtracting
    ; the start of the memory location of the string from the end of it
    sub     edi, string
    mov     eax, [file_handle]                  ; place the file handler to EAX
    mov     ebx, string                         ; place the beginning of the memory location to EBX
    mov     ecx, edi                            ; place the number of bytes (the length of the string) to ECX
    call    fio_write                           ; write to file, given the parameters EAX, EBX, ECX

    pop     ecx                                 ; load the saved registers
    pop     ebx
    pop     eax
    ret
; END (_put_file)

;======================================================================================================
; Prints a single newline character to the file.
_put_newline:
    push    eax                                 ; save the used registers to the stack
    push    ebx
    push    ecx

    mov     eax, [file_handle]                  ; place the file handler to EAX
    mov     ebx, string                         ; place the beginning of the memory location to EBX
    mov     [ebx], byte NEW_LINE_CHR            ; place the newline character as the first byte of the string
    xor     ecx, ecx
    inc     ecx                                 ; ECX = 1, the number of bytes that need to be written
    call    fio_write                           ; write to file, given the parameters EAX, EBX, ECX

    pop     ecx                                 ; load the saved registers
    pop     ebx
    pop     eax
    ret
; END (_put_newline)

;======================================================================================================
; Concatenates a 32 bit float from XMM0 to the string starting at memory location in EDI
_add_float:
    push    eax                                 ; save the used registers to the stack
    push    ebx
    push    ecx

    mov     ecx, FLOAT_PREC                     ; move the number of decimals needed to ECX

    mov     al, '+'
    movd    ebx, xmm0                           ; check the sign of the number (the most significant bit - the 32th)
    test    ebx, 0x80000000
    jz      .sign_write                         ; if the sign bit is zero, the number is positive, so we add a '+' sign to the string
    inc     al                                  ; else, we add 2 to AL, so it becomes '-'
    inc     al
    xor     ebx, 0x80000000                     ; we flip the sign bit to be zero
    movd    xmm0, ebx                           ; then we put back the number into XMM0

  .sign_write:
    mov     [edi], byte al                      ; add the sign character to the string
    inc     edi

    cvttss2si eax, xmm0                         ; convert the number to integer, with truncation
    call      _add_int                          ; add the integer part of the number to the string, as an integer, stored in EAX

    mov     al, '.'                             ; add the decimal point
    mov     [edi], al
    inc     edi

    roundss xmm1, xmm0, 3                       ; round XMM0 with truncation to XMM1
    subss   xmm0, xmm1                          ; keep the fraction of the number in XMM0
    mov     eax, 10
    cvtsi2ss    xmm1, eax                       ; XMM1 = 10.00

    .decimal:
        mulss       xmm0, xmm1                  ; bring a decimal to the left side of the decimal point
        cvttss2si   eax, xmm0                   ; convert the new integer part of XMM0 to integer, this way we get a single digit in EAX
        add         al, '0'                     ; convert the digit to character
        mov         [edi], al                   ; add the character to the string
        inc         edi
        roundss     xmm2, xmm0, 3               ; we cut the whole part of the number again, just like above
        subss       xmm0, xmm2
        loop        .decimal                    ; write as much decimal places as needed

    pop     ecx                                 ; load the saved registers
    pop     ebx
    pop     eax
    ret
; END (_add_float)

;======================================================================================================
; Concatenates a 32 bit positive integer from EAX to the string starting at memory location in EDI
_add_int:
    push    eax                                 ; save the used registers to the stack
    push    ebx
    push    edx
    push    ebp

    mov     ebp, esp                            ; save the stack pointer to EBP

    .cut_digit:
        mov     ebx, 10                         ; EBX = 10
        xor     edx, edx                        ; EDX = 0, it needs to be this because it will be the sign extension of EAX at the division
        idiv    ebx                             ; divide EDX:EAX with EBX, EDX = quotient, EAX = remainder
        push    dx                              ; push the 2 bytes containing the single digit, to the stack
        cmp     eax, 0                          ; if we ran out of digits we jump out
        jne     .cut_digit

    .write_digit:                               ; we loop through the just cut digits, but in reverse order
        cmp     esp, ebp                        ; if there are still digits in the stack
        je      .end_write
        pop     ax                              ; pop the digit from the stack
        add     al, '0'                         ; convert it to character
        mov     [edi], al                       ; add to the string
        inc     edi
        jmp     .write_digit                    ; loop through all the digits

  .end_write:
    pop     ebp                                 ; load the saved registers
    pop     edx
    pop     ebx
    pop     eax
    ret
; END (_add_int)

;======================================================================================================
section .data
    dbg_file_name           db              'debug.txt', 0          ; name of the debug file

    ; screen debug messages
    msg_success_open        db              'Debug file succesfully opened.', 10, 0
    msg_success_close       db              'Debug file succesfully closed.', 10, 0

    file_handle             dd              0                       ; memory pointer to file handler

	; debug state variable - first bit set to 1 if screen debug is enabled, second bit set to 1 if file debug is enabled
	; here the file debug bit will always be set, and the default value of the screen debug bit will be overriden upon the call of dbgfile_init
    fs_debug         		dd              0x1
; END (section .data)

;======================================================================================================
section .bss
    string                  resb            2048                    ; globally used string, which gets written to the file upon _put_string call
; END (section .bss)
