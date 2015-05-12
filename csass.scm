;;; csass.scm -- A command-line SASS compiler in Chicken Scheme
;;;   This version is really just a toy example. It is translated
;;;   from the example at
;;;   https://github.com/sass/libsass/wiki/API-Sass-Context-Example

(use (prefix sass sass:))

(define (main)
  (let* ((args (cdr (argv)))
         (input (if (null? args)
                    "styles.scss"
                    (car args)))
         (file-ctx (sass:make-file-context input))
         (ctx (sass:file-context-get-context file-ctx))
         (ctx-opt (sass:context-get-options ctx)))
    (sass:option-set-precision! ctx-opt 10)
    (let ((status (sass:compile-file-context file-ctx)))
      (if (zero? status)
        (display (sass:context-get-output-string ctx))
        (display (sass:context-get-error-message ctx))))
    (sass:delete-file-context file-ctx)))

(main)
