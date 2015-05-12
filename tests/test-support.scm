(use utils)

; Are files equal, excluding whitespace differences?
(define (file=? file1 file2)
  (let ((normalize-whitespace
         (lambda (str)
           (string-intersperse
             (string-split
               (string-trim-both str))
             " "))))
    (string=? (normalize-whitespace (read-all file1))
              (normalize-whitespace (read-all file2)))))
