@setlocal
@set CO=/W3 /nologo /O2 /c /Fo:
@set LO=/nologo /Fe:
@set DO=/nologo /dll
@setlocal
call "%VS140COMNTOOLS%..\..\vc\vcvarsall.bat" x64
cl %CO% inject.obj inject.c
cl %CO% fsatrace.obj fsatrace.c
cl %LO% fsatrace.exe fsatrace.obj inject.obj
cl %CO% fsatrace64.obj fsatracedll.c
link %DO% /out:fsatrace64.dll fsatrace64.obj inject.obj ntdll.lib
@endlocal
@setlocal
call "%VS140COMNTOOLS%..\..\vc\vcvarsall.bat" x86
cl %CO% inject.obj inject.c
cl %CO% fsatracehelper.obj fsatracehelper.c
cl %LO% fsatracehelper.exe fsatracehelper.obj
cl %CO% fsatrace32.obj fsatracedll.c
link %DO% /out:fsatrace32.dll fsatrace32.obj inject.obj ntdll.lib
@endlocal
@endlocal

