# Filesystem Access Tracer

This tool injects code into other applications in order to trace file accesses.

## Why?

This can be useful for things like build systems, since it allows to
automatically generate dependencies in a toolchain-agnostic way or to ensure
declared dependencies match the real ones.

## Compiling

On Windows, type `build.bat` to compile `fsatrace.exe` and associated DLLs
`fsatrace32.dll` / `fsatrace64.dll`. The build script assumes VS 2015 is
installed, if that's not the case just edit it so it can find the correct
location of `vcvarsall.bat`.

On Unix, type `make` to generate a `fsatrace.so` object.

## Usage

On Windows, run fsatrace as follows:

    fsatrace accesses.fsa -- cmd /c "echo>foo && copy foo bar && ren bar baz && del foo baz"

On Darwin, inject the shared object setting `DYLD_INSERT_LIBRARIES` to the path of the `fsatrace.so` file and setting `DYLD_FORCE_FLAT_NAMESPACE` as follows:

    env FSAT_OUT=accesses.fsa DYLD_INSERT_LIBRARIES=fsatrace.so DYLD_FORCE_FLAT_NAMESPACE=1 sh -c 'touch foo && cp foo bar && mv bar baz && rm foo baz'

On Linux/NetBSD:

    env FSAT_OUT=accesses.fsa LD_PRELOAD=fsatrace.so sh -c 'touch foo && cp foo bar && mv bar baz && rm foo baz'

## Output format

Newline-separated sequence with the following possibilities:

* w:`path-to-file-opened-for-read`
* r:`path-to-file-opened-for-write`
* m:`path-to-destination-of-move`:`path-to-source-of-move`
* d:`path-to-deleted-file`

