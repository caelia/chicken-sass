;; Test the support procedures used for testing

(use test)
(include "test-support.scm")

(define default-comparator (make-parameter (current-test-comparator)))

(test-group "[1] file=?"
  (current-test-comparator file=?)
  (test "[1.01] Two empty files"
    "tst-data/01a.txt" "tst-data/01b.txt")
  (test "[1.02] Two identical files containing only whitespace"
    "tst-data/02a.txt" "tst-data/02b.txt")
  (test "[1.03] Two non-identical files containing only whitespace"
    "tst-data/03a.txt" "tst-data/03b.txt")
  (test "[1.04] Two identical files containing non-whitespace"
    "tst-data/04a.txt" "tst-data/04b.txt")
  (test "[1.05] Two files with non-whitespace, and different whitespace"
    "tst-data/05a.txt" "tst-data/05b.txt")
  (test "[1.06] Two files with non-whitespace, and different whitespace"
    "tst-data/06a.txt" "tst-data/06b.txt")
  (current-test-comparator (default-comparator))
  (test "[1.07] Two different files"
    #f
    (file=? "tst-data/07a.txt" "tst-data/07b.txt")))
