
REM taken from:  http://stackoverflow.com/questions/33136486/program-in-c-compiled-with-embedded-r-dll-crashes-when-calling-standard-c-functi

setlocal
call "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" amd64
SET MACHINE=x64
SET LIBNAME=R
dumpbin /exports %LIBNAME%.dll > %LIBNAME%.exports
echo LIBRARY R > %LIBNAME%.def
echo EXPORTS >> %LIBNAME%.def
for /f "skip=19 tokens=4" %%A in (%LIBNAME%.exports) do echo %%A >> %LIBNAME%.def

REM manually edit R.def to remove 'printf' definition

start /wait notepad.exe %LIBNAME%.def
lib /def:%LIBNAME%.def /out:%LIBNAME%.lib /machine:%MACHINE%
endlocal