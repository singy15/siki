# siki

siki, standalone web based application environment.

## Build

1. sbcl --load build.lisp

## Usage

1. Execute "siki" (or "siki.exe" on Windows).

## Loading libraries

1. Get QuickLisp and put "quicklisp.lisp" in the same directory as "siki" placed.

2. Add (load "./quicklisp.lisp") to .lisp file

3. Add (load "./laven.lisp") to .lisp file (put in the below of the 2.)

4. Load library using laven (ex. (lvn:load "cl-ppcre"))

