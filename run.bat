@echo off

if exist main.exe (
    start /max main.exe
) else (
    echo 'main.exe' not found. Build it first by running the 'build.bat' file.
    pause
)