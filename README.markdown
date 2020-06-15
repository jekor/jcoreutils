# Jekor's Core Utilities (jcoreutils)

These are additions and enhancements to GNU Core Utilities (coreutils).

## xtee

xtee ("cross-tee"/"expanded tee") is a program for building complex pipelines. It resembles the tee command, except that instead of copying stdin to stdout, it copies a file to stdout.

## map

Map a command over each line of stdin.

xargs is nice, but it builds commands. I've found no way to shuffle its input into another program's stdin without using echo (which is prone to errors). map is a simpler program with 1 purpose.

## chop

Take sections from each line of stdin.

Like cut, but supports indexing fields from the end.

## trans

Search and replace strings from stdin.

Like tr, but using strings instead of characters.

One nice feature of trans is that it doesn't operate on lines, so you can use it in more pipelines without blocking (like sed will).

WARNING: trans is using a naive implementation that will not match strings across read boundaries. There's a chance that some strings will not be translated. Consider this version experimental.

# Building

I build jcoreutils with [Nix](http://nixos.org/nix/) to try to ensure reproducible builds:

```
nix build
```
