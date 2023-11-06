@echo off

set base=%cd%

cd %base%\src

%base%\bin\nasm -fwin32 main.asm
if %errorlevel% neq 0 exit /b %errorlevel%

%base%\bin\nasm -fwin32 window.asm
if %errorlevel% neq 0 exit /b %errorlevel%

%base%\bin\nasm -fwin32 net.asm
if %errorlevel% neq 0 exit /b %errorlevel%

%base%\bin\nasm -fwin32 dbgfile.asm
if %errorlevel% neq 0 exit /b %errorlevel%

mkdir %base%\out

move *.obj %base%\out

cd %base%\out

%base%\bin\ld main.obj window.obj net.obj dbgfile.obj -lio -lutil -lgfx -o main.exe %base%\start.obj -L%base%\lib -lkernel32 --entry start
if %errorlevel% neq 0 exit /b %errorlevel%

move main.exe %base%

cd %base%
