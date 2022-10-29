;======================================================================================================
; Initializes the structure of the net from the txt file, the parameters of the functions from the binary
; file, and prints the structure of the net to the screen. No in/out parameters.
extern net_init

;======================================================================================================
; Starts evaluating the net functions on the given input image. In: EBX - memory pointer to the beginning
; of the original image's buffer, ECX - size in pixels of the original image's size (width/height),
; EDX - debug state variable. Out: none.
extern net_start

;======================================================================================================
; Basically frees the memory allocated for the weights of the network. No in/out parameters.
extern net_destroy