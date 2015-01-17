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
    "index.pl"
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
  '("-DTRAIL_STACK_SIZE=10000000"
    "-DCHOICE_POINT_STACK_SIZE=100000000"
    "-DENVIRONMENT_STACK_SIZE=20000000"
    "-DHEAP_SIZE=250000000"))

(define gcc-compile-options
  '("-std=gnu99" "-g" "-I." "-fno-strict-aliasing" "-fwrapv" "-DTRACE"))

(define gcc-optimized-compile-options
  '("-std=gnu99" "-I." "-fno-strict-aliasing" "-fwrapv" "-O2" "-fomit-frame-pointer"))

(define gcc-reckless-compile-options
  '("-std=gnu99" "-I." "-fno-strict-aliasing" "-fwrapv" "-O3" "-fomit-frame-pointer"
    "-DUNSAFE"))

(define manifest
  `("Makefile"
    "README"
    "lib/sorts.pl" "lib/ordset.pl"
    "pc.c"
    "pc.h"
    "pi.c"
    "g-s-p.pl" "system-predicates"
    "pi.pl" "lib/interp.pl" "call_primitive.pl" "evaluate_op.pl" "system_predicate.pl"
    ,@source-files))


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
	  (run (gcc ,@gcc-compile-options pc1.c -lm -lrt -o pc1 ,@pc-compile-options))))))

(define (pc1o)
  (pc1.c)
  (make (("pc1o" ("pc1.c" "pc.h")
	  (run (gcc ,@gcc-optimized-compile-options pc1.c -lm -lrt -o pc1o
		    ,@pc-compile-options))))))

(define (pc2.c)
  (pc1)
  (make/proc (list (list "pc2.c" source-files
			 (lambda () (run (./pc1 pc.pl -o pc2.c)))))
	     "pc2.c"))

(define (tags)
  (run (etags -l prolog *.pl)))

(define check-pc "./pc")
(define check-options '())

(define (check)
  (let ((tests (string-split (capture (ls tests/*.pl)) "\n"))
	(ok 0)
	(not-ok 0))
    (for-each
     (lambda (fname)
       (if (zero? (run* (,(string-append "CHECK_PC=" check-pc)
			 ./check ,@check-options ,fname)))
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
  (fluid-let ((check-options '("-O")))
    (check)))

(define (check-pc1-optimized)
  (pc1)
  (fluid-let ((check-options '("-O"))
	      (check-pc "./pc1"))
    (check)))

(define (check-self-compile)
  (pc2.c)
  (zero? (run* (cmp pc1.c pc2.c))))

(define (check-embedded)
  (make (("tmp/embed" ("embed.c" "embedded.o" "pc.h")
	  (run (gcc ,@gcc-compile-options embed.c embedded.o -lm -lrt -o tmp/embed)))
	 ("embedded.o" ("embedded.c" "pc.h")
	  (run (gcc ,@gcc-compile-options -c embedded.c -o embedded.o -DEMBEDDED)))
	 ("embedded.c" ("embedded.pl")
	  (run (./pc embedded.pl -o embedded.c)))))
  (run (tmp/embed)))

(define (full-check)
  (let ((ok #t))
    (cond ((check-self-compile)
	   (set! ok #f)
	   (unless (check-pc1) (set! ok #f))
	   (unless (check-pc1-optimized) (set! ok #f))
	   (unless (check-embedded) (set! ok #f)))
	  (else (set! ok #f)))
    (print "\n----------------------------------------------------------------------")
    (print (if ok
	       "ALL CHECKS SUCCEEDED."
	       "SOME CHECKS FAILED."))
    (print)))

(define (make-program src . more)
  (let-optionals more ((exe (strip-suffix src))
		       deps)
    (let ((c (replace-suffix "c" src)))
      (make/proc (list (list exe (list c)
			     (lambda ()
			       (run (gcc ,@gcc-compile-options ,c -lm -lrt -o ,exe))))
		       (list c (cons src deps)
			     (lambda ()
			       (run (./pc ,src -o ,c)))))))))

(define (system-predicates)
  (make-program 
   "g-s-p.pl" 
   "g-s-p"
   "pc.h")
  (make (("system_predicate.pl" ("g-s-p" "system-predicates")
	  (run (./g-s-p <system-predicates))))))

(define (pi)
  (system-predicates)
  (make-program "pi.pl" "pi" "lib/interp.pl" "system_predicate.pl" "call_primitive.pl"))

(define (bench)
  (define (runonce)
    (run (/usr/bin/time -f %U -o /tmp/pc1time ./pc1o pc.pl -o /dev/null >/dev/null))
    (with-input-from-file "/tmp/pc1time" read))
  (pc1o)
  (print (/ (apply + (butlast (cdr (sort (list-of (runonce) (i range 0 5)) <)))) 3)))

(define (clean)
  (run (rm -f system_predicate.pl call_primitive.pl pi pc1 pc1o)))

(define (dist)
  (pc2.c)
  (let* ((date (capture (date +%Y-%m-%d)))
	 (dname "pc.tar.gz")
	 (ddir (string-append "pc-" date)))
    (run (cp pc2.c pc.c))
    (run (rm -fr ,ddir ,dname))
    (run (mkdir ,ddir))
    (run (mkdir -p ,(string-append ddir "/lib")))
    (for-each
     (lambda (fn)
       (run (cp ,fn ,(string-append ddir "/" fn))))
     manifest)
    (run (tar cfz ,dname ,ddir))
    (run (rm -fr ,ddir))))


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
