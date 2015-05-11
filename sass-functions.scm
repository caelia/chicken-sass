;;; sass-functions.scm -- Functions module for libsass binding.
;;;
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module sass-functions
        *
        (import scheme chicken)
        (import foreign)
        (use foreigners)

(include "sass-common-types.scm")

;;; Forward declaration
;; Possibly don't need to do anything with these
; struct Sass_Import;
; struct Sass_Importer;
; struct Sass_Function;

;;; Typedef defining importer signature and return type
(define-foreign-type importer-fn
  (function import-list
            (c-string importer-entry (c-pointer sass-compiler))))
; typedef Sass_Import_List (*Sass_Importer_Fn)
;   (const char* url, Sass_Importer_Entry cb, struct Sass_Compiler* compiler);

;;; Typedef defining function signature and return type
(define-foreign-type function-fn
  (function (c-pointer sass-value)
            ((c-pointer sass-value) function-entry (c-pointer sass-options))))
; typedef union Sass_Value* (*Sass_Function_Fn)
  ; (const union Sass_Value*, Sass_Function_Entry cb, struct Sass_Options* options);

;;; Creator for sass custom importer return argument list
(define make-importer-list
  (foreign-lambda importer-list
                  sass_make_importer_list
                  size_t))
; Sass_Importer_List sass_make_importer_list (size_t length);
(define importer-get-list-entry
  (foreign-lambda importer-entry
                  sass_importer_get_list_entry
                  importer-list
                  size_t))
; Sass_Importer_Entry sass_importer_get_list_entry (Sass_Importer_List list, size_t idx);
(define importer-set-list-entry!
  (foreign-lambda void
                  sass_importer_set_list_entry
                  importer-list
                  size_t
                  importer-entry))
; void sass_importer_set_list_entry (Sass_Importer_List list, size_t idx, Sass_Importer_Entry entry);

;;; Creators for custom importer callback (with some additional pointer)
;;; The pointer is mostly used to store the callback into the actual binding
(define make-importer
  (foreign-lambda importer-entry
                  sass_make_importer
                  ; importer-fn
                  c-pointer
                  double
                  c-pointer))
; Sass_Importer_Entry sass_make_importer (Sass_Importer_Fn importer, double priority, void* cookie);

;;; Getters for import function descriptors
(define importer-get-function
  (foreign-lambda importer-fn
                  sass_importer_get_function
                  importer-entry))
; Sass_Importer_Fn sass_importer_get_function (Sass_Importer_Entry cb);
(define importer-get-priority
  (foreign-lambda double
                  sass_importer_get_priority
                  importer-entry))
; double sass_importer_get_priority (Sass_Importer_Entry cb);
(define importer-get-cookie
  (foreign-lambda c-pointer
                  sass_importer_get_cookie
                  importer-entry))
; void* sass_importer_get_cookie (Sass_Importer_Entry cb);

;;; Deallocator for associated memory
(define delete-importer
  (foreign-lambda void
                  sass_delete_importer
                  importer-entry))
; void sass_delete_importer (Sass_Importer_Entry cb);

;;; Creator for sass custom importer return argument list
(define make-import-list
  (foreign-lambda import-list
                  sass_make_import_list
                  size_t))
; Sass_Import_List sass_make_import_list (size_t length);
;;; Creator for a single import entry returned by the custom importer inside the list
(define make-import-entry
  (foreign-lambda import-entry
                  sass_make_import_entry
                  c-string
                  c-string
                  c-string))
; Sass_Import_Entry sass_make_import_entry (const char* path, char* source, char* srcmap);
(define make-import
  (foreign-lambda import-entry
                  sass_make_import
                  c-string
                  c-string
                  c-string
                  c-string))
; Sass_Import_Entry sass_make_import (const char* path, const char* base, char* source, char* srcmap);
;;; set error message to abort import and to print out a message (path from existing object is used in output)
(define import-set-error!
  (foreign-lambda import-entry
                  sass_import_set_error
                  import-entry
                  c-string
                  size_t
                  size_t))
; Sass_Import_Entry sass_import_set_error(Sass_Import_Entry import, const char* message, size_t line, size_t col);

;;; Setters to insert an entry into the import list (you may also use [] access directly)
;;; Since we are dealing with pointers they should have a guaranteed and fixed size
(define import-set-list-entry!
  (foreign-lambda void
                  sass_import_set_list_entry
                  import-list
                  size_t
                  import-entry))
; void sass_import_set_list_entry (Sass_Import_List list, size_t idx, Sass_Import_Entry entry);
(define import-get-list-entry
  (foreign-lambda import-entry
                  sass_import_get_list_entry
                  import-list
                  size_t))
; Sass_Import_Entry sass_import_get_list_entry (Sass_Import_List list, size_t idx);

;;; Getters for import entry
(define import-get-path
  (foreign-lambda c-string
                  sass_import_get_path
                  import-entry))
; const char* sass_import_get_path (Sass_Import_Entry);
(define import-get-base
  (foreign-lambda c-string
                  sass_import_get_base
                  import-entry))
; const char* sass_import_get_base (Sass_Import_Entry);
(define import-get-source
  (foreign-lambda c-string
                  sass_import_get_source
                  import-entry))
; const char* sass_import_get_source (Sass_Import_Entry);
(define import-get-srcmap
  (foreign-lambda c-string
                  sass_import_get_srcmap
                  import-entry))
; const char* sass_import_get_srcmap (Sass_Import_Entry);
;;; Explicit functions to take ownership of these items
;;; The property on our struct will be reset to NULL
(define import-take-source
  (foreign-lambda c-string
                  sass_import_take_source
                  import-entry))
; char* sass_import_take_source (Sass_Import_Entry);
(define import-take-srcmap
  (foreign-lambda c-string
                  sass_import_take_srcmap
                  import-entry))
; char* sass_import_take_srcmap (Sass_Import_Entry);
;;; Getters from import error entry
(define import-get-error-line
  (foreign-lambda size_t
                  sass_import_get_error_line
                  import-entry))
; size_t sass_import_get_error_line (Sass_Import_Entry);
(define import-get-error-column
  (foreign-lambda size_t
                  sass_import_get_error_column
                  import-entry))
; size_t sass_import_get_error_column (Sass_Import_Entry);
(define import-get-error-message
  (foreign-lambda c-string
                  sass_import_get_error_message
                  import-entry))
; const char* sass_import_get_error_message (Sass_Import_Entry);

;;; Deallocator for associated memory (incl. entries)
(define delete-import-list
  (foreign-lambda void
                  sass_delete_import_list
                  import-list))
; void sass_delete_import_list (Sass_Import_List);
;;; Just in case we have some stray import structs
(define delete-import
  (foreign-lambda void
                  sass_delete_import
                  import-entry))
; void sass_delete_import (Sass_Import_Entry);


;;; Creators for sass function list and function descriptors
(define make-function-list
  (foreign-lambda function-list
                  sass_make_function_list
                  size_t))
; Sass_Function_List sass_make_function_list (size_t length);
(define make-function
  (foreign-lambda function-entry
                  sass_make_function
                  c-string
                  ; function-fn
                  c-pointer
                  c-pointer))
; Sass_Function_Entry sass_make_function (const char* signature, Sass_Function_Fn cb, void* cookie);

;;; Setters and getters for callbacks on function lists
(define function-get-list-entry
  (foreign-lambda function-entry
                  sass_function_get_list_entry
                  function-list
                  size_t))
; Sass_Function_Entry sass_function_get_list_entry(Sass_Function_List list, size_t pos);
(define function-set-list-entry!
  (foreign-lambda void
                  sass_function_set_list_entry
                  function-list
                  size_t
                  function-entry))
; void sass_function_set_list_entry(Sass_Function_List list, size_t pos, Sass_Function_Entry cb);

;;; Getters for custom function descriptors
(define function-get-signature
  (foreign-lambda c-string
                  sass_function_get_signature
                  function-entry))
; const char* sass_function_get_signature (Sass_Function_Entry cb);
(define function-get-function
  (foreign-lambda function-fn
                  sass_function_get_function
                  function-entry))
; Sass_Function_Fn sass_function_get_function (Sass_Function_Entry cb);
(define function-get-cookie
  (foreign-lambda c-pointer
                  sass_function_get_cookie
                  function-entry))
; void* sass_function_get_cookie (Sass_Function_Entry cb);

) ; END MODULE
