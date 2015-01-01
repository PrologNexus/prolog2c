#!/bin/scheme /grass/19/boot
;;;; makefile for pc -*- Scheme -*-


(require 'sh 'make 'match)
(run-verbose #t)


(define source-files 
  '("settings.pl"
    "support.pl"
    "state.pl"
    "terms.pl"
    "dcg.pl"
    "macros.pl"
    "process.pl"
    "compile.pl"
    "assemble.pl"
    "main.pl"
    "lib/lists.pl"
    "lib/misc.pl"
    "lib/write.pl"
    "lib/rdtok.pl"
    "lib/op.pl"
    "lib/rdb.pl"
    "lib/io.pl"
    "lib/findall.pl"
    "lib/sets.pl"
    "lib/read.pl"
    "lib/cdb.pl"
    "main.pl"
    "pc.pl"))


(define (all) (pc1))

(define (pc1.c)
  (make/proc (list (list "pc1" source-files
			 (lambda ()
			   (run (./pc pc.pl -o pc1.c)))))
	     "pc1"))

(define (pc1)
  (pc1.c)
  (make (("pc1" ("pc1.c" "pc.h")
	  (run (gcc -std=gnu99 -I. -g pc1.c -lm -lrt -o pc1))))))

(define (tags)
  (run (etags -l prolog *.pl)))

(define (check)
  (let ((tests (string-split (capture (ls tests/*.pl)) "\n"))
	(ok 0)
	(not-ok 0))
    (for-each
     (lambda (fname)
       (if (zero? (run* (./check ,fname)))
	   (inc! ok)
	   (inc! not-ok)))
     tests)
    (print "\n----------------------------------------------------------------------------------------")
    (print ok " tests successful, " not-ok " tests failed.")
    (exit not-ok)))


;;

(when (file-exists? "config.scm")
  (load "config.scm"))

(cond-expand
  ((not interactive)
   (for-each 
    (o eval list string->symbol) 
    (let ((args (command-line-arguments)))
      (if (null? args)
	  '("all")
	  args))))
  (else))
