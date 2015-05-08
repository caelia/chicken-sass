;;; sass-functions.scm -- Functions module for libsass binding.
;;;
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module sass-functions
        *
        (import foreign)
        (use foreigners)

(foreign-declare "#include <sass_functions.h>")

(include "sass-common-types.scm")

;;; Forward declaration
;; Possibly don't need to do anything with these
; struct Sass_Import;
; struct Sass_Importer;
; struct Sass_Function;

;;; Typedef helpers for import lists
(define-foreign-type import-entry (struct "Sass_Import_Entry"))
; typedef struct Sass_Import (*Sass_Import_Entry);
; typedef struct Sass_Import* (*Sass_Import_List);
;;; Typedef helpers for custom importer lists
(define-foreign-type importer-entry (struct "Sass_Importer_Entry"))
; typedef struct Sass_Importer (*Sass_Importer_Entry);
; typedef struct Sass_Importer* (*Sass_Importer_List);
;;; Typedef defining importer signature and return type
(define-foreign-type importer-fn
  (function (c-pointer (c-pointer import-entry))
            (c-string (c-pointer importer-entry) (c-pointer sass-compiler))))
; typedef Sass_Import_List (*Sass_Importer_Fn)
;   (const char* url, Sass_Importer_Entry cb, struct Sass_Compiler* compiler);

;;; Typedef helpers for custom functions lists
(define-foreign-type function-entry
  (struct "Sass_Function_Entry"))
; typedef struct Sass_Function (*Sass_Function_Entry);
; typedef struct Sass_Function* (*Sass_Function_List);
;;; Typedef defining function signature and return type
(define-foreign-type function-fn
  (function (c-pointer sass-value)
            ((c-pointer sass-value) (c-pointer function-entry) options)))
; typedef union Sass_Value* (*Sass_Function_Fn)
  ; (const union Sass_Value*, Sass_Function_Entry cb, struct Sass_Options* options);

;;; Creator for sass custom importer return argument list
(define make-importer-list
  (foreign-lambda (c-pointer (c-pointer importer-entry))
                  sass_make_importer_list
                  size_t))
; Sass_Importer_List sass_make_importer_list (size_t length);
(define importer-get-list-entry
  (foreign-lambda (c-pointer importer-entry)
                  sass_importer_get_list_entry
                  (c-pointer (c-pointer importer-entry))
                  size_t))
; Sass_Importer_Entry sass_importer_get_list_entry (Sass_Importer_List list, size_t idx);
(define importer-set-list-entry
  (foreign-lambda void
                  sass_importer_set_list_entry
                  (c-pointer (c-pointer importer-entry))
                  size_t
                  (c-pointer importer-entry)))
; void sass_importer_set_list_entry (Sass_Importer_List list, size_t idx, Sass_Importer_Entry entry);

;;; Creators for custom importer callback (with some additional pointer)
;;; The pointer is mostly used to store the callback into the actual binding
(define make-importer
  (foreign-lambda (c-pointer importer-entry)
                  sass_make_importer
                  importer-fn
                  double
                  c-pointer))
; Sass_Importer_Entry sass_make_importer (Sass_Importer_Fn importer, double priority, void* cookie);

;;; Getters for import function descriptors
(define importer-get-function
  (foreign-lambda importer-fn
                  sass_importer_get_function
                  (c-pointer importer-entry)))
; Sass_Importer_Fn sass_importer_get_function (Sass_Importer_Entry cb);
(define importer-get-priority
  (foreign-lambda double
                  "sass_importer_get_priority"
                  (c-pointer importer-entry)))
; double sass_importer_get_priority (Sass_Importer_Entry cb);
(define importer-get-cookie
  ((foreign-lambda void* "sass_importer_get_cookie" (c-pointer importer-entry) ))
; void* sass_importer_get_cookie (Sass_Importer_Entry cb);

;;; Deallocator for associated memory
(define delete-importer
  ((foreign-lambda void "sass_delete_importer" (c-pointer importer-entry) ))
; void sass_delete_importer (Sass_Importer_Entry cb);

;;; Creator for sass custom importer return argument list
(define make-import-list
  ((foreign-lambda Sass_Import_List "sass_make_import_list" (size_t length)) ))
; Sass_Import_List sass_make_import_list (size_t length);
;;; Creator for a single import entry returned by the custom importer inside the list
(define make-import-entry
  ((foreign-lambda Sass_Import_Entry "sass_make_import_entry" (const char* path, char* source, char* srcmap)) ))
; Sass_Import_Entry sass_make_import_entry (const char* path, char* source, char* srcmap);
(define make-import
  ((foreign-lambda Sass_Import_Entry "sass_make_import" (const char* path, const char* base, char* source, char* srcmap)) ))
; Sass_Import_Entry sass_make_import (const char* path, const char* base, char* source, char* srcmap);
;;; set error message to abort import and to print out a message (path from existing object is used in output)
(define import-set-error(Sass-Import_Entry
  ((foreign-lambda Sass_Import_Entry "sass_import_set_error(Sass_Import_Entry" import, const char* message, size_t line, size_t col)) ))
; Sass_Import_Entry sass_import_set_error(Sass_Import_Entry import, const char* message, size_t line, size_t col);

;;; Setters to insert an entry into the import list (you may also use [] access directly)
;;; Since we are dealing with pointers they should have a guaranteed and fixed size
(define import-set-list-entry
  ((foreign-lambda void "sass_import_set_list_entry" (Sass_Import_List list, size_t idx, Sass_Import_Entry entry)) ))
; void sass_import_set_list_entry (Sass_Import_List list, size_t idx, Sass_Import_Entry entry);
(define import-get-list-entry
  ((foreign-lambda Sass_Import_Entry "sass_import_get_list_entry" (Sass_Import_List list, size_t idx)) ))
; Sass_Import_Entry sass_import_get_list_entry (Sass_Import_List list, size_t idx);

;;; Getters for import entry
(define import-get-path
  ((foreign-lambda const "char*" sass_import_get_path (Sass_Import_Entry)) ))
; const char* sass_import_get_path (Sass_Import_Entry);
(define import-get-base
  ((foreign-lambda const "char*" sass_import_get_base (Sass_Import_Entry)) ))
; const char* sass_import_get_base (Sass_Import_Entry);
(define import-get-source
  ((foreign-lambda const "char*" sass_import_get_source (Sass_Import_Entry)) ))
; const char* sass_import_get_source (Sass_Import_Entry);
(define import-get-srcmap
  ((foreign-lambda const "char*" sass_import_get_srcmap (Sass_Import_Entry)) ))
; const char* sass_import_get_srcmap (Sass_Import_Entry);
;;; Explicit functions to take ownership of these items
;;; The property on our struct will be reset to NULL
(define import-take-source
  ((foreign-lambda char* "sass_import_take_source" (Sass_Import_Entry)) ))
; char* sass_import_take_source (Sass_Import_Entry);
(define import-take-srcmap
  ((foreign-lambda char* "sass_import_take_srcmap" (Sass_Import_Entry)) ))
; char* sass_import_take_srcmap (Sass_Import_Entry);
;;; Getters from import error entry
(define import-get-error-line
  ((foreign-lambda size_t "sass_import_get_error_line" (Sass_Import_Entry)) ))
; size_t sass_import_get_error_line (Sass_Import_Entry);
(define import-get-error-column
  ((foreign-lambda size_t "sass_import_get_error_column" (Sass_Import_Entry)) ))
; size_t sass_import_get_error_column (Sass_Import_Entry);
(define import-get-error-message
  ((foreign-lambda const "char*" sass_import_get_error_message (Sass_Import_Entry)) ))
; const char* sass_import_get_error_message (Sass_Import_Entry);

;;; Deallocator for associated memory (incl. entries)
(define delete-import-list
  ((foreign-lambda void "sass_delete_import_list" (Sass_Import_List)) ))
; void sass_delete_import_list (Sass_Import_List);
;;; Just in case we have some stray import structs
(define delete-import
  ((foreign-lambda void "sass_delete_import" (Sass_Import_Entry)) ))
; void sass_delete_import (Sass_Import_Entry);


;;; Creators for sass function list and function descriptors
(define make-function-list
  ((foreign-lambda Sass_Function_List "sass_make_function_list" (size_t length)) ))
; Sass_Function_List sass_make_function_list (size_t length);
(define make-function
  ((foreign-lambda Sass_Function_Entry "sass_make_function" (const char* signature, Sass_Function_Fn cb, void* cookie)) ))
; Sass_Function_Entry sass_make_function (const char* signature, Sass_Function_Fn cb, void* cookie);

;;; Setters and getters for callbacks on function lists
(define function-get-list-entry(Sass_Function_List
  ((foreign-lambda Sass_Function_Entry "sass_function_get_list_entry(Sass_Function_List" list, size_t pos)) ))
; Sass_Function_Entry sass_function_get_list_entry(Sass_Function_List list, size_t pos);
(define function-set-list-entry(Sass_Function_List
  ((foreign-lambda void "sass_function_set_list_entry(Sass_Function_List" list, size_t pos, Sass_Function_Entry cb)) ))
; void sass_function_set_list_entry(Sass_Function_List list, size_t pos, Sass_Function_Entry cb);

;;; Getters for custom function descriptors
(define function-get-signature
  ((foreign-lambda const "char*" sass_function_get_signature (Sass_Function_Entry cb)) ))
; const char* sass_function_get_signature (Sass_Function_Entry cb);
(define function-get-function
  ((foreign-lambda Sass_Function_Fn "sass_function_get_function" (Sass_Function_Entry cb)) ))
; Sass_Function_Fn sass_function_get_function (Sass_Function_Entry cb);
(define function-get-cookie
  ((foreign-lambda void* "sass_function_get_cookie" (Sass_Function_Entry cb)) ))
; void* sass_function_get_cookie (Sass_Function_Entry cb);

) ; END MODULE
