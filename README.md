# Filesystem Access Tracer

This tool injects code into other applications in order to trace file accesses.

## Why?

This can be useful for things like build systems, since it allows to
automatically generate dependencies in a toolchain-agnostic way or to ensure
declared dependencies match the real ones.

## Compiling

On Unix, type `make` to generate the `fsatrace` executable and the
`fsatrace.so` shared library.

On Windows, you'll need recent 64-bit and 32-bit versions of
`mingw`. You can either adapt the `Makefile` to point to your
compilers or, alternatively, install
https://github.com/commercialhaskell/stack and run the following
sequence to get the required compilers:

    stack setup --resolver ghc-8.6.5 --arch=x86_64
    stack setup --resolver ghc-8.6.5 --arch=i386
    stack exec -- pacman -S make

After that, invoke:

    stack exec -- make

That should generate `fsatrace.exe`, `fsatracehelper.exe`,
`fsatrace32.dll` and `fsatrace64.dll`.

## Usage

Make sure the .dll or .so files are in the same path as the `fsatrace`
executable and run:

    fsatrace <options> <output-file> -- <command>

Options is a combination of the following characters:

* `v`: print args vector
* `r`: dump read operations
* `w`: dump write operations
* `m`: dump file move operations
* `d`: dump file delete operations
* `q`: dump file stat operations
* `t`: dump touch operations

### Environment Variables

* `FSAT_BUF_SIZE`: when set, overwrites size of buffer for trace output.

## macOS usage

In order to use `fsatrace` on systems newer than OS X 10.10, System Integrity Protection must be disabled as detailed in https://developer.apple.com/library/content/documentation/Security/Conceptual/System_Integrity_Protection_Guide/ConfiguringSystemIntegrityProtection/ConfiguringSystemIntegrityProtection.html

Use at your own risk!

## Output format

Newline-separated sequence with the following possibilities:

* `r`|`path-to-file-opened-for-write`
* `w`|`path-to-file-opened-for-read`
* `m`|`path-to-destination-of-move`|`path-to-source-of-move`
* `d`|`path-to-deleted-file`
* `q`|`path-to-queried-file`
* `t`|`path-to-touched-file`
