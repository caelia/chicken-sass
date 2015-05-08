(foreign-declare "#include <sass_values.h>")
(foreign-declare "#include <sass_functions.h>")

(define-foreign-type sass-value (union "Sass_Value"))

(define-foreign-type sass-compiler (struct "Sass_Compiler"))
;;; Forward declaration
; struct Sass_Compiler;

(define-foreign-type sass-options (struct "Sass_Options"))
; struct Sass_Options; // base struct
