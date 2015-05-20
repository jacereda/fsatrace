# Filesystem Access Tracer

Shared object that can be injected into other applications to trace
file accesses.

## Why?

This can be useful for things like build systems, since it allows to
automatically generate dependencies in a language-agnostic way or
to ensure declared dependencies match the real ones.

## Compiling

Type `make` to generate a `fsatrace.so` object.

## Usage

On Darwin, inject the shared object setting `DYLD_INSERT_LIBRARIES` to the path of the `fsatrace.so` file and setting `DYLD_FORCE_FLAT_NAMESPACE` as follows:

    sh $ env FSAT_OUT=/dev/stdout DYLD_INSERT_LIBRARIES=fsatrace.so DYLD_FORCE_FLAT_NAMESPACE=1 sh -c 'touch /tmp/foo && cp /tmp/foo /tmp/bar && mv /tmp/bar /tmp/baz && rm /tmp/foo'

On Linux/NetBSD:

    sh $ env FSAT_OUT=/dev/stdout LD_PRELOAD=fsatrace.so sh -c 'touch /tmp/foo && cp /tmp/foo /tmp/bar && mv /tmp/bar /tmp/baz && rm /tmp/foo'

The above sequence generates the following list of accesses:

    w:/dev/tty
    r:/tmp/foo
    w:/tmp/bar
    m:/tmp/baz:/tmp/bar
    d:/tmp/foo

## Output format

Newline-separated sequence with the following possibilities:

* w:`path-to-file-opened-for-read`
* r:`path-to-file-opened-for-write`
* m:`path-to-destination-of-move`:`path-to-source-of-move`
* d:`path-to-deleted-file`

