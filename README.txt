Build the project with the 'build.bat' file.

Run the application with the 'run.bat' file.



Or type the following commands in the command prompt:

nasm -fwin32 main.asm
nasm -fwin32 window.asm
nasm -fwin32 net.asm
nasm -fwin32 dbgfile.asm
nlink main.obj window.obj net.obj dbgfile.obj -lio -lutil -lgfx -o main.exe
start main.exe