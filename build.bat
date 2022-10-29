@echo off

nasm -fwin32 main.asm
if %errorlevel% neq 0 exit /b %errorlevel%

nasm -fwin32 window.asm
if %errorlevel% neq 0 exit /b %errorlevel%

nasm -fwin32 net.asm
if %errorlevel% neq 0 exit /b %errorlevel%

nasm -fwin32 dbgfile.asm
if %errorlevel% neq 0 exit /b %errorlevel%

nlink main.obj window.obj net.obj dbgfile.obj -lio -lutil -lgfx -o main.exe
if %errorlevel% neq 0 exit /b %errorlevel%

echo Build succesful. Run 'run.bat' to open the application.

pause