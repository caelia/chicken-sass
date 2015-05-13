;;; csass.scm -- A command-line SASS compiler in Chicken Scheme
;;;
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(use ports)
(import extras)
(use (prefix sass sass:))
(use args)


(cond-expand
  (windows (define-constant PATH-SEP ";"))
  (else (define-constant PATH-SEP ":")))

(define *default-import-paths* (make-parameter #f))
(define *default-precision* (make-parameter 5))
(define *default-output-style* (make-parameter 'nested))

(define (output err-status err-msg output-str output-file)
  (cond
    ((not (zero? err-status))
     (error (or err-msg "Unknown error.")))
    ((and output-str output-file)
     (with-output-to-file output-file
       (lambda () (display output-str))))
    (output-str
      (display output-str))
    (else
      (print "No output."))))

(define (compile-stdin options outfile)
  (let ((source-string (with-output-to-string read-all)))
    (if (string=? source-string "")
      (error "No input.")
      (let* ((data-ctx (sass:make-data-context source-string))
             (output-ctx (sass:data-context-get-context data-ctx)))
        (sass:data-context-set-options data-ctx options)

        (sass:compile-data-context data-ctx)

        (output (sass:context-get-error-status ctx)
                (sass:context-get-error-message ctx)
                (sass:context-get-output-string ctx)
                outfile)

        (sass:delete-data-context data-ctx)))))

(define (compile-file options infile outfile)
  (let* ((file-ctx (sass:make-file-context infile))
         (output-ctx (sass:file-context-get-context file-ctx)))
    (when outfile
      (sass:option-set-output-path! options outfile))
    (sass:option-set-input-path! options infile)
    (sass:file-context-set-options! file-ctx options)

    (sass:compile-file-context file-ctx)

    (output (sass:context-get-error-status output-ctx)
            (sass:context-get-error-message output-ctx)
            (sass:context-get-output-string output-ctx)
            outfile)

    (let ((map-file (sass:option-get-source-map-file options)))
      (when map-file
        (output (sass:context-get-error-status output-ctx)
                (sass:context-get-error-message output-ctx)
                (sass:context-get-source-map-string output-ctx)
                (sass:context-get-source-map-file output-ctx))))

    (sass:delete-file-context file-ctx)))

(define opts
  `(,(args:make-option (s stdin) #:none
                       "Read input from stdin.")
    ,(args:make-option (o output-file) (required: "FILENAME")
                       "Write the output to FILENAME.")
    ,(args:make-option (I load-path) (required: "PATH")
                       "Set the import path to PATH.")
    ,(args:make-option (t style) (required: "STYLENAME")
                       "Set the output style to STYLENAME.
                        Can be: nested, expanded, compact, compressed.")
    ,(args:make-option (l line-numbers line-comments) #:none
                       "Emit comments showing line numbers in input file.")
    ,(args:make-option (m sourcemap) #:none
                       "Emit source map.")
    ,(args:make-option (M omit-map-comment) #:none
                       "Omit the source map url comment.")
    ,(args:make-option (p precision) (required: "DIGITS")
                       "Set the numerical precision to DIGITS.")
    ,(args:make-option (v version) #:none
                       "Display compiled versions.")
    ,(args:make-option (h help) #:none
                       "Display this help message.")))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (car (argv)) " [options...] [input-file]")
      (newline)
      (print (args:usage opts)))))

(define (version)
  (printf "libsass version: ~A\n" (sass:libsass-version))
  (printf "chicken-sass version: ~A\n" (sass:%version%)))

(define (have-option? key options)
  (let loop ((opts options))
    (cond
      ((null? opts) #f)
      ((eqv? (caar opts) key) #t)
      (else (loop (cdr opts))))))

(define (run)
  (receive (options operands)
    (args:parse (command-line-arguments) opts)
    (cond
      ((have-option? 'h options) (usage) (exit 0))
      ((have-option? 'v options) (version) (exit 0))
      (else
        (let ((sass-opts (sass:make-options)))
          (let* ((output-style*
                   (alist-ref 't options))
                 (output-style
                   (if output-style*
                     (string->symbol output-style*)
                     (*default-output-style*))))
            (if (member output-style '(nested expanded compact compressed))
              (sass:option-set-output-style! sass-opts output-style)
              (error "Output style must be one of: nested, expanded, compact, or compressed.")))
          (let ((precision (alist-ref 'p options)))
            (sass:option-set-precision! sass-opts (if precision (string->number precision)
                                                    (*default-precision*))))
          (let* ((cl-paths (alist-ref 'I options))
                 (default-paths (*default-import-paths*))
                 (import-paths
                   (cond
                     ((and cl-paths default-paths)
                      (string-append cl-import-paths PATH-SEP default-paths))
                     (cl-paths
                       cl-paths)
                     (default-paths
                       default-paths)
                     (else
                       ""))))
            (sass:option-set-include-path! sass-opts import-paths))
          (if (have-option? 'l options)
            (sass:option-set-source-comments! sass-opts #t)
            (sass:option-set-source-comments! sass-opts #f))
          (if (have-option? 'M options)
            (sass:option-set-omit-source-map-url! sass-opts #t)
            (sass:option-set-omit-source-map-url! sass-opts #f))
          (let ((infile (and (= (length operands) 1) (car operands)))
                (outfile (alist-ref 'o options))
                (from-stdin (have-option? 's options))
                (emit-source-map (have-option? 'm options)))
            (when (and outfile emit-source-map)
              (sass:option-set-source-map-file! sass-opts (string-append outfile ".map"))) 
            (cond
              ((and from-stdin (not infile))
               (compile-stdin sass-opts outfile))
              ((and infile (not from-stdin))
               (compile-file sass-opts infile outfile))
              (else
                (error "Please specify an input file OR the -s option (and not both).")))))))))

(run)
