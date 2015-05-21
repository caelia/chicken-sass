;;; sass.scm -- A wrapper for libsass.
;;;
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module sass
        (%version% string-quote string-unquote
         compile-file compile-string compile-stdin
         libsass-version)
        (import scheme chicken)
        (import foreign)
        (import data-structures)
        (use foreigners)
        (use utils)
        (require-library sass-values sass-functions sass-context)
        (reexport sass-context)

(foreign-declare "#include <sass.h>")

(include "sass-common.scm")

(define *default-include-paths* (make-parameter #f))
(define *default-precision* (make-parameter 5))
(define *default-output-style* (make-parameter 'nested))

(cond-expand
  (windows (define-constant PATH-SEP ";"))
  (else (define-constant PATH-SEP ":")))

(define (%version%)
  (let* ((ext-info (extension-information 'sass))
         (version-datum (and ext-info (alist-ref 'version ext-info))))
    (if version-datum
      (car version-datum)
      "[UNKNOWN]")))

;; Some convenient string helper function
(define string-quote
  (foreign-lambda c-string sass_string_quote c-string char))
; char* sass_string_quote (const char *str, const char quote_mark);
(define string-unquote
  (foreign-lambda c-string sass_string_unquote c-string))
; char* sass_string_unquote (const char *str);

;; Get compiled libsass version
(define libsass-version
  (foreign-lambda c-string libsass_version))
; const char* libsass_version(void);

(define (compile input-ctx #!key (output 'stdout) (precision #f) (output-style #f) (source-comments 'undefined)
                                 (source-map-embed 'undefined) (source-map-contents 'undefined)
                                 (omit-source-map-url 'undefined) (is-indented-syntax-src 'undefined)
                                 (indent #f) (linefeed #f) (input-path #f) (output-path #f)
                                 (plugin-path #f) (include-path #f) (source-map-file #f)
                                 (source-map-root #f) (c-headers #f) (c-importers #f) (c-functions #f))

  (let* ((output-ctx (get-context input-ctx))
         (options (make-options))
         (cleanup (lambda () (delete-input-context input-ctx)))
         (default-inc-paths (*default-include-paths*))
         (all-include-paths
           (cond
             ((and include-path default-inc-paths)
              (string-append include-path PATH-SEP default-inc-paths))
             (include-path
               include-path)
             (default-inc-paths
               default-inc-paths)
             (else
               ""))))

    (set-options! options
                  precision: (or precision (*default-precision*))
                  output-style: (or output-style (*default-output-style*))
                  source-comments: source-comments
                  source-map-embed: source-map-embed
                  source-map-contents: source-map-contents
                  omit-source-map-url: omit-source-map-url
                  is-indented-syntax-src: is-indented-syntax-src
                  indent: indent
                  linefeed: linefeed
                  input-path: input-path
                  output-path: output-path
                  plugin-path: plugin-path
                  include-path: all-include-paths
                  source-map-file: source-map-file
                  source-map-root: source-map-root
                  c-headers: c-headers
                  c-importers: c-importers
                  c-functions: c-functions)

    (set-ctx-options! input-ctx options)
    (compile-input-context input-ctx)

    (unless (zero? (error-status output-ctx))
      (cleanup)
      (error (or (error-message output-ctx) "Unknown error.")))

    (let ((output-str (output-string output-ctx))
          (map-file (and (eqv? (input-context-type input-ctx) 'file)
                         (source-map-file options))))
      (cond
        ((and output-str (eqv? output 'stdout))
         (display output-str))
        (output-str
          (with-output-to-file output (lambda () (display output-str)))
        (else
          (print "No output."))))

      (when map-file
        (let ((map-string (source-map-string output-ctx)))
          (if map-string
            (with-output-to-file map-file (lambda () (display map-string)))
            (display "ERROR: No content available for source map file."
                     (current-error-port))))))

    (cleanup)))
  

(define (compile-file filename . kwargs)
  (apply compile `(,(make-file-context filename) ,@kwargs)))

(define (compile-string data . kwargs)
  (apply compile `(,(make-data-context data) ,@kwargs)))

(define (compile-stdin . kwargs)
  (let ((data (read-all)))
    (when (string=? data "")
      (error "No input."))
    (apply compile `(,(make-data-context data) ,@kwargs))))

) ; END MODULE
