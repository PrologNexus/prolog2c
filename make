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
  '("-std=gnu99" "-g" "-I." "-fno-strict-aliasing" "-fwrapv" "-DTRACE" "-DDEBUG_GC"))

(define gcc-optimized-compile-options
  '("-std=gnu99" "-I." "-fno-strict-aliasing" "-fwrapv" "-O2" "-fomit-frame-pointer" "-DNDEBUG"))

(define gcc-reckless-compile-options
  '("-std=gnu99" "-I." "-fno-strict-aliasing" "-fwrapv" "-O3" "-fomit-frame-pointer" "-DUNSAFE"))

(define manifest
  `("Makefile"
    "README"
    "lib/sorts.pl" "lib/ordset.pl" "lib/writef.pl" "lib/arith.pl"
    "pc.c"
    "pc.h"
    "pi.c"
    "pb.c"
    "g-s-p.pl" "system-predicates"
    "pb.pl"
    "pi.pl" "lib/interp.pl" "pi_call_primitive.pl" "pi_evaluate_op.pl" "pi_system_predicate.pl"
    ,@source-files))


(define (all) 
  (pc1)
  (pi)
  (pb))

(define (pc1.c)
  (make/proc (list (list "pc1.c" source-files
			 (lambda ()
			   (run (./pc "-I" "." pc.pl -o pc1.c)))))
	     "pc1.c"))

(define (pc1)
  (pc1.c)
  (make (("pc1" ("pc1.c" "pc.h")
	  (run (gcc ,@gcc-compile-options pc1.c -lm -lrt -o pc1 ,@pc-compile-options))))))

(define (pc32)
  (pc1.c)
  (make (("pc32" ("pc1.c" "pc.h")
	  (run (gcc -m32 ,@gcc-compile-options pc1.c -lm -lrt -o pc32 ,@pc-compile-options))))))

(define (pc1o)
  (pc1.c)
  (make (("pc1o" ("pc1.c" "pc.h")
	  (run (gcc ,@gcc-optimized-compile-options pc1.c -lm -lrt -o pc1o
		    ,@pc-compile-options))))))

(define (pc2.c)
  (pc1)
  (make/proc (list (list "pc2.c" (cons "pc.h" source-files)
			 (lambda () (run (./pc1 "-I" "." pc.pl -o pc2.c)))))
	     "pc2.c"))

(define (tags)
  (run (etags -l prolog *.pl)))

(define check-pc "./pc")
(define check-options '())

(define (make-binding fname)
  (let* ((file (basename (strip-suffix fname)))
	 (bname (string-append "tmp/" file))
	 (ok #f))
    (make/proc (list (list (string-append fname ".pl")
			   (list "pb" fname)
			   (lambda () 
			     (set! ok
			       (zero? (run* (./pb -o ,fname ,fname))))))))
    ok))

(define (check)
  (let ((tests (string-split (capture (ls tests/*.pl)) "\n"))
	(ok 0)
	(not-ok 0))
    (for-each
     (lambda (fname)
       (let* ((hasb (file-exists? (replace-suffix "bind" fname)))
	      (bok (if hasb (make-binding hasb) #t)))
	 (if (and bok (zero? (run* (,(string-append "CHECK_PC=" check-pc)
				    ./check ,@check-options ,fname))))
	     (inc! ok)
	     (inc! not-ok))
	 (when hasb (run (rm -f ,(string-append hasb ".*"))))))
     tests)
    (print "\n----------------------------------------------------------------------")
    (print ok " tests successful, " not-ok " tests failed.")
    (zero? not-ok)))

(define (check-pc1)
  (pc1)
  (fluid-let ((check-pc "./pc1"))
    (check)))

(define (check-pi)
  (pi)
  (fluid-let ((check-options '("-i")))
    (check)))

(define (check-m32)
  (fluid-let ((check-options '("-m32")))
    (check)))

(define (check-pc32)
  (pc32)
  (fluid-let ((check-pc "./pc32")
	      (check-options '("-m32")))
    (check)))

(define (check-dist)
  (dist)
  (run (mkdir -p tmp ";" rm -fr tmp/pc-* ";" cp pc.tar.gz tmp ";" cd tmp ";" tar xfz pc.tar.gz))
  (run (/usr/bin/make -C tmp/pc-* all check)))

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
  (pc1)
  (make (("tmp/embed" ("embed.c" "embedded.o" "pc.h")
	  (run (gcc ,@gcc-compile-options embed.c embedded.o -lm -lrt -o tmp/embed)))
	 ("embedded.o" ("embedded.c" "pc.h")
	  (run (gcc ,@gcc-compile-options -c embedded.c -o embedded.o -DEMBEDDED)))
	 ("embedded.c" ("embedded.pl" "pc1")
	  (run (./pc1 "-I" "." embedded.pl -o embedded.c)))))
  (run (tmp/embed)))

(define (full-check)
  (let ((ok #t))
    (cond ((check-self-compile)
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
      (make/proc (list (list exe (list "pc.h" c)
			     (lambda ()
			       (run (gcc ,@gcc-compile-options ,c -lm -lrt -o ,exe))))
		       (list c (cons src deps)
			     (lambda ()
			       (run (./pc "-I" "." ,src -o ,c)))))))))

(define (system-predicates)
  (make-program "g-s-p.pl" "g-s-p")
  (make (("pi_system_predicate.pl" ("g-s-p" "system-predicates")
	  (run (./g-s-p <system-predicates))))))

(define (pi.c)
  (system-predicates)
  (make/proc (list (list "pi.c" 
			 (list "lib/interp.pl" "pi_system_predicate.pl" "pi_call_primitive.pl" 
			       "pi_evaluate_op.pl" "dcg.pl")
			 (lambda ()
			   (run (./pc "-I" "." pi.pl -o pi.c)))))))

(define (pi)
  (pi.c)
  (make (("pi" ("pi.c" "pc.h")
	  (run (gcc ,@gcc-compile-options pi.c -lm -lrt -o pi ,@pc-compile-options))))))

(define (pio)
  (pi.c)
  (make (("pio" ("pi.c" "pc.h")
	  (run (gcc ,@gcc-optimized-compile-options pi.c -lm -lrt -o pio ,@pc-compile-options))))))

(define (pb)
  (fluid-let ((gcc-compile-options (append gcc-compile-options pc-compile-options)))
    (make-program "pb.pl" "pb")))

(define (bench)
  (let ((tests (string-split (capture (ls benchmarks/*.pl)) "\n")))
    (with-temporary-files
     (lambda ()
       (let ((out (temporary-file)))
	 (for-each
	  (lambda (fname)
	    (run (echo ,fname >> ,out))
	    (run (./bench ,fname "2>&1" "|" tee -a ,out)))
	  tests)
	 (run (echo "----------------------------------------" >> benchmarks.txt))
	 (run (date >> benchmarks.txt))
	 (run (git rev-parse --short HEAD >> benchmarks.txt))
	 (run (cat ,out >> benchmarks.txt)))))))

(define (clean)
  (run (rm -f pi_system_predicate.pl pi_call_primitive.pl pi_evaluate_op.pl pi pc1 pc2.c pc1.c pc1o)))

(define (dist)
  (pc2.c)
  (pi)
  (pb)
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

(define (upload)
  (run (upload -d prolog pc.tar.gz README)))

(define (install)
  (pc1o)
  (pio)
  (run (./install)))


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
