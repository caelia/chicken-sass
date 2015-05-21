(use test)
(use srfi-1)
(use (prefix sass sass:))

(include "test-support.scm")

(current-test-comparator file=?)
(define test-data-root (make-parameter "test-cases"))

(define disabled-tests '())

(define (test-disabled? path-spec #!optional (dis-map disabled-tests))
  (let ((found
          (find
            (lambda (elt)
              (or (and (string? elt)
                       (string=? elt (car path-spec)))
                  (string=? (car elt) (car path-spec))))
            dis-map)))
    (cond
      ((not found) #f)
      ((string? found) #t)
      ((or (null? (cdr found)) (eqv? (cdr found) #:all)) #t)
      (else (test-disabled? (cdr path-spec) (cdr found))))))

(define (test-case? parent-path path)
  (find
    (lambda (elt) (string=? elt "input.scss"))
    (directory (make-pathname parent-path path))))

;(define (run-test path-spec #!optional (label #f))
(define (run-test parent-path test-subpath #!optional (label #f))
  (let* ((label (or label test-subpath))
         (test-path (make-pathname parent-path test-subpath))
         (ref-file (make-pathname test-path "expected_output.css"))
         (outfile (make-pathname test-path "test_output.css"))
         (infile (make-pathname test-path "input.scss")))
    (sass:compile-file infile output: outfile input-path: infile output-path: outfile)
    (test label ref-file outfile)))

(define (run-test-group parent-path group-subpath)
  (let ((group-path (make-pathname parent-path group-subpath)))
    (test-group group-subpath
      (for-each
        (lambda (subpath)
          (if (test-case? group-path subpath)
            (run-test group-path subpath)
            (run-test-group group-path subpath)))
        (sort (directory group-path) string<?))))) 

(define (run-tests path)
  (for-each
    (lambda (subpath)
      (if (test-case? path subpath)
        (run-test path subpath subpath)
        (run-test-group path subpath)))
    (sort (directory path) string<?)))

; (run-tests "fake-test-cases")
(run-tests (test-data-root))
