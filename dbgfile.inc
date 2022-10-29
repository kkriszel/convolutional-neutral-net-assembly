;======================================================================================================
; Initializes the writing to the output file
; In: EAX - debug parameters (the first bit of EAX is set if messages need to be printed to the screen)
extern dbgfile_init

;======================================================================================================
; In parameters:
; EAX - memory pointer to 32 bit float buffer, which needs to be written
; EBX - size (width and height) of map
; ECX - total channels of map
; In total, the buffer should contain size * size * channel 32 bit floating point numbers
extern dbgfile_write

;======================================================================================================
; Closes the opened debug file. No in/out parameters.
extern dbgfile_destroy