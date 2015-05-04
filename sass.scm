;;; sass.scm -- A wrapper for libsass.
;;;
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module sass
        *
        (import foreign)
        (import foreigners)

(foreign-declare "#include <sass.h>")

(define-foreign-enum-type (output-style int)
  (output-style->int int->output-style)
  ((nested style/nested) SASS_STYLE_NESTED)
  ((expanded style/expanded) SASS_STYLE_EXPANDED)
  ((compact style/compact) SASS_STYLE_COMPACT)
  ((compressed style/compressed) SASS_STYLE_COMPRESSED))

;; Some convenient string helper function
; char* sass_string_quote (const char *str, const char quote_mark);
; char* sass_string_unquote (const char *str);

;; Get compiled libsass version
; const char* libsass_version(void);

) ; END MODULE
