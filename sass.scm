;;; sass.scm -- A wrapper for libsass.
;;;
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module sass
        *
        (import scheme chicken)
        (import foreign)
        (import data-structures)
        (use foreigners)
        (require-library sass-values sass-functions sass-context)
        (reexport sass-values sass-functions sass-context)

(foreign-declare "#include <sass.h>")

(include "sass-common-types.scm")

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

) ; END MODULE
