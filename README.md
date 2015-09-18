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

Make sure the .dll or .so files are in the same path as the executable
and run:

	fsatrace <output-file> -- <command>

## Output format

Newline-separated sequence with the following possibilities:

* w|`path-to-file-opened-for-read`
* r|`path-to-file-opened-for-write`
* m|`path-to-destination-of-move`|`path-to-source-of-move`
* d|`path-to-deleted-file`
