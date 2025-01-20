@echo off
set libs_win=user32.lib gdi32.lib OpenGL32.lib
cl /nologo -Iinclude src\main.c  /Fe./build/main.exe -Fo./build/main.obj
