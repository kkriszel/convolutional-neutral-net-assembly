;======================================================================================================
; Initializes the window. No in/out parameters.
; Returns fail in CF. CF=0 if everything went well, CF=1 if error occoured.
extern win_init

;======================================================================================================
; Draws the pixel buffer to the window. No in/out parameters.
extern win_draw

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
extern win_update

;======================================================================================================
; Destroys the window, frees the memory of the allocated memory buffers, prints debug message if needed
extern win_destroy

;======================================================================================================
; Displays the welcome/help message to the screen. It is starting from memory address msg_help1,
; and there is a declaration for each line, but they don't contain the terminating character (0) and
; they are placed continuously in the memory, so they will be all printed at once.
extern win_disphelp