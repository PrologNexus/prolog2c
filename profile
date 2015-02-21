#!/bin/scheme /grass/17/boot
;;;; compile + run program with profiling enabled -*- Scheme -*-


(require 'sh 'match)

;(run-verbose #t)
(define opts '("-DPROFILE"))
(define moreopts '())
(define args (command-line-arguments))
(define file (car args))
(define bfile (basename file))
(define xfile (strip-suffix bfile))
(define cfile (replace-suffix "c" bfile))

(let ((s (suffix file)))
  (unless (string=? "c" s)
    (run (./pc "-I" "." -q ,file -o ,cfile))))

(run (gcc -std=gnu99 -I. ,@opts ,@moreopts ,cfile -o ,xfile -lm -lrt))

(let ((status (run* (,(string-append "./" xfile) ,@(cdr args)))))
  (when (zero? status)
    (run (cat "`ls -t PROFILE.* | head -n1`")))
  (exit status))
