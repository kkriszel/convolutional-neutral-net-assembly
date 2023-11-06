@echo off

if not exist main.exe (
    build
)

start /max main.exe
