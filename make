#!/bin/scheme /grass/19/boot
;;;; makefile for pc -*- Scheme -*-


(require 'sh 'make 'match 'list-of 'sort)
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

(define pc-compile-options
  '("-DTRACE"
    "-DTRAIL_STACK_SIZE=10000000"
    "-DCHOICE_POINT_STACK_SIZE=100000000"
    "-DHEAP_SIZE=250000000"))


(define (all) 
  (pc1)
  (pi))

(define (pc1.c)
  (make/proc (list (list "pc1.c" source-files
			 (lambda ()
			   (run (./pc pc.pl -o pc1.c)))))
	     "pc1.c"))

(define (pc1)
  (pc1.c)
  (make (("pc1" ("pc1.c" "pc.h")
	  (run (gcc -std=gnu99 -I. -g pc1.c -lm -o pc1 ,@pc-compile-options))))))

(define (pc1o)
  (pc1.c)
  (make (("pc1o" ("pc1.c" "pc.h")
	  (run (gcc -std=gnu99 -I. -O3 -fomit-frame-pointer -fno-strict-aliasing -fwrapv 
		    pc1.c -lm -o pc1o -DUNSAFE ,@pc-compile-options))))))

(define (pc2.c)
  (pc1)
  (make/proc (list (list "pc2.c" source-files
			 (lambda () (run (./pc1 pc.pl -o pc2.c)))))
	     "pc2.c"))

(define (tags)
  (run (etags -l prolog *.pl)))

(define check-pc "./pc")

(define (check)
  (let ((tests (string-split (capture (ls tests/*.pl)) "\n"))
	(ok 0)
	(not-ok 0))
    (for-each
     (lambda (fname)
       (if (zero? (run* (,(string-append "CHECK_PC=" check-pc) ./check ,fname)))
	   (inc! ok)
	   (inc! not-ok)))
     tests)
    (print "\n----------------------------------------------------------------------")
    (print ok " tests successful, " not-ok " tests failed.")
    (zero? not-ok)))

(define (check-pc1)
  (pc1)
  (fluid-let ((check-pc "./pc1"))
    (check)))

(define (check-optimized)
  (fluid-let ((check-pc "./pc -O"))
    (check)))

(define (check-pc1-optimized)
  (pc1)
  (fluid-let ((check-pc "./pc1 -O"))
    (check)))

(define (check-self-compile)
  (pc2.c)
  (or (zero? (run* (cmp pc1.c pc2.c)))
      #f))

(define (full-check)
  (let ((r (and (check-self-compile)
		(check-pc1)
		(check-pc1-optimmized))))
    (print "\n----------------------------------------------------------------------")
    (print (if r
	       "ALL CHECKS SUCCEEDED."
	       "SOME CHECS FAILED."))
    (print)))

(define (make-program src . more)
  (let-optionals more ((exe (strip-suffix src))
		       deps)
    (let ((c (replace-suffix "c" src)))
      (make/proc (list (list exe (list c)
			     (lambda ()
			       (run (gcc -std=gnu99 -I. -g -DTRACE ,c -lm -o ,exe))))
		       (list c (cons src deps)
			     (lambda ()
			       (run (./pc ,src -o ,c)))))))))

(define (system-predicates)
  (make-program 
   "generate-system-predicates.pl" 
   "generate-system-predicates"
   "pc.h")
  (make (("system_predicate.pl" ("generate-system-predicates" "system-predicates")
	  (run (./generate-system-predicates <system-predicates))))))

(define (pi)
  (system-predicates)
  (make-program "interp.pl" "pi" "system_predicate.pl" "call_primitive.pl"))

(define (bench)
  (define (runonce)
    (run (/usr/bin/time -f %U -o /tmp/pc1time ./pc1o pc.pl -o /dev/null >/dev/null))
    (with-input-from-file "/tmp/pc1time" read))
  (pc1o)
  (print (/ (apply + (butlast (cdr (sort (list-of (runonce) (i range 0 5)) <)))) 3)))

(define (clean)
  (run (rm -f system_predicate.pl call_primitive.pl pi pc1 pc1o)))


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
