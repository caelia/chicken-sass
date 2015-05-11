;;; sass-common-types.scm -- Shared type definitions for libsass wrapper.
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(foreign-declare "#include <sass_values.h>")
(foreign-declare "#include <sass_functions.h>")

(define-foreign-type sass-value (union "Sass_Value"))

(define-foreign-type sass-compiler (struct "Sass_Compiler"))
;;; Forward declaration
; struct Sass_Compiler;

(define-foreign-type sass-options (struct "Sass_Options"))
; struct Sass_Options; // base struct

(define-foreign-enum-type (output-style int)
  (output-style->int int->output-style)
  ((nested style/nested) SASS_STYLE_NESTED)
  ((expanded style/expanded) SASS_STYLE_EXPANDED)
  ((compact style/compact) SASS_STYLE_COMPACT)
  ((compressed style/compressed) SASS_STYLE_COMPRESSED))

;;; Typedef helpers for import lists
(define-foreign-type import-entry (c-pointer (struct "Sass_Import")))
; typedef struct Sass_Import (*Sass_Import_Entry);
(define-foreign-type import-list (c-pointer (c-pointer (struct "Sass_Import"))))
; typedef struct Sass_Import* (*Sass_Import_List);
;;; Typedef helpers for custom importer lists
(define-foreign-type importer-entry (c-pointer (struct "Sass_Importer")))
; typedef struct Sass_Importer (*Sass_Importer_Entry);
(define-foreign-type importer-list (c-pointer (c-pointer (struct "Sass_Importer"))))
; typedef struct Sass_Importer* (*Sass_Importer_List);

;;; Typedef helpers for custom functions lists
(define-foreign-type function-entry (c-pointer (struct "Sass_Function")))
; typedef struct Sass_Function (*Sass_Function_Entry);
(define-foreign-type function-list (c-pointer (c-pointer (struct "Sass_Function"))))
; typedef struct Sass_Function* (*Sass_Function_List);
