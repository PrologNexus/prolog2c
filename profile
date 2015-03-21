#!/bin/scheme /grass/17/boot
;;;; compile + run program with profiling enabled -*- Scheme -*-


(require 'sh 'match)

;(run-verbose #t)
(define opts '("-DPROFILE"))
(define moreopts '())
(define args (command-line-arguments))
(define gprof #f)

(match args
  (("-gprof" . more)
   (set! gprof #t)
   (set! opts '("-pg"))
   (pop! args))
  (("-M" . more)
   (set! opts '("-DPROFILE_MEMORY"))
   (pop! args))
  (_ #f))

(define file (car args))
(define bfile (basename file))
(define xfile (string-append "tmp/" (strip-suffix bfile)))
(define cfile (string-append "tmp/" (replace-suffix "c" bfile)))

(let ((s (suffix file)))
  (unless (string=? "c" s)
    (run (./pc -n -q ,file -o ,cfile))))

(run (gcc -std=gnu99 -Ilib -I. -g ,@opts ,@moreopts ,cfile -o ,xfile -lm -lrt))

(let ((status (run* (,(string-append "./" xfile) ,@(cdr args)))))
  (if gprof
      (run (gprof -b ,xfile))
      (let ((pfile (capture (ls -t PROFILE.* "|" head -n1))))
	(when (zero? status)
	  (run (cat ,pfile))
	  (delete-file pfile))
	(exit status))))
