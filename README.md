# findhttp

[![Hackage](https://img.shields.io/hackage/v/findhttp.svg)](https://hackage.haskell.org/package/findhttp)
[![GPL license](https://img.shields.io/badge/license-GPLv3+-brightgreen.svg)](https://www.gnu.org/licenses/gpl.html)

A simple `find` tool that supports http directories as well as local files.

It can be useful for example if you encountered a http directory file server
with html listing that truncates filenames.

## Usage

```shellsession
$ findhttp --version
0.1.1
$ findhttp --help
find for http

Usage: findhttp [--version] [-m|--maxdepth DEPTH]
                [(-f|--files) | (-d|--dirs) | (-s|--symlinks)] [-n|--name GLOB]
                [-u|--show-urls] URL/DIR
  Find files from an http "directory"

Available options:
  -h,--help                Show this help text
  --version                Show version
  -m,--maxdepth DEPTH      Maximum search depth (default 10)
  -f,--files               List files only
  -d,--dirs                List directories only
  -s,--symlinks            List symlinks only (not http)
  -n,--name GLOB           Limit files to glob matches
  -u,--show-urls           Prefix files with url

```

## Example

```shellsession
$ findhttp https://file.example.com/files/
dir1/
file1
file2
file3
```

## Installation

Build (from Hackage) with `cabal install findhttp` or `stack install findhttp`.

Build from source with `cabal install` or `stack install`.
