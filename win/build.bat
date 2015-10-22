@setlocal
@set CO=/DNTDDI_VERSION=0x60000000 /D_WIN32_WINNT=0x600 /MT /EHs-c- /W3 /nologo /O2 /c /Fo:
@set LO=/SUBSYSTEM:console /nologo ntdll.lib kernel32.lib libvcruntime.lib
@set DO=/nologo /dll ntdll.lib shell32.lib
@setlocal
call "%VS140COMNTOOLS%..\..\vc\vcvarsall.bat" x64
cl %CO% dbg.obj dbg.c
cl %CO% inject.obj inject.c
cl %CO% patch.obj patch.c
cl %CO% hooks.obj hooks.c
cl %CO% emit.obj emit.c
cl %CO% handle.obj handle.c
cl %CO% utf8.obj utf8.c
cl %CO% fsatrace.obj fsatrace.c
link %LO% /out:fsatrace.exe fsatrace.obj dbg.obj inject.obj shell32.lib
cl %CO% fsatrace64.obj fsatracedll.c
link %DO% /out:fsatrace64.dll fsatrace64.obj inject.obj  patch.obj hooks.obj emit.obj handle.obj utf8.obj dbg.obj
@endlocal
@setlocal
call "%VS140COMNTOOLS%..\..\vc\vcvarsall.bat" x86
cl %CO% dbg.obj dbg.c
cl %CO% inject.obj inject.c
cl %CO% patch.obj patch.c
cl %CO% hooks.obj hooks.c
cl %CO% emit.obj emit.c
cl %CO% handle.obj handle.c
cl %CO% utf8.obj utf8.c
cl %CO% fsatracehelper.obj fsatracehelper.c
link %LO% /out:fsatracehelper.exe fsatracehelper.obj
cl %CO% fsatrace32.obj fsatracedll.c
link %DO% /out:fsatrace32.dll fsatrace32.obj inject.obj  patch.obj hooks.obj emit.obj handle.obj utf8.obj dbg.obj
@endlocal
@endlocal

