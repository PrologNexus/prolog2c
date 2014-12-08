#!/bin/scheme /grass/19/boot
;;;; makefile for pc -*- Scheme -*-


(require 'sh 'make 'match)
(run-verbose #t)


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
