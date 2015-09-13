@setlocal
call "%VS140COMNTOOLS%..\..\vc\vcvarsall.bat" x64
cl /W3 /nologo /Fe: fsatrace.exe fsatracewin.c
cl /W3 /nologo /c /Fo: fsatrace64.obj fsatracedll.c 
link /nologo /dll /out:fsatrace64.dll fsatrace64.obj kernel32.lib psapi.lib ntdll.lib
@endlocal
@setlocal
call "%VS140COMNTOOLS%..\..\vc\vcvarsall.bat" x86
cl /W3 /nologo /Fe: fsatracehelper.exe fsatracehelper.c
cl /W3 /nologo /c /Fo: fsatrace32.obj fsatracedll.c
link /nologo /dll /out:fsatrace32.dll fsatrace32.obj kernel32.lib psapi.lib ntdll.lib
@endlocal
