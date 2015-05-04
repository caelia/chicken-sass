;;; sass-functions.h -- Functions module for libsass binding.
;;;
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module sass-functions
        *
        (import foreign)
        (use foreigners)

(foreign-declare "#include <sass_functions.h")

;;; Forward declaration
; struct Sass_Import;

;;; Forward declaration
; struct Sass_C_Import_Descriptor;

;;; Typedef defining the custom importer callback
; typedef struct Sass_C_Import_Descriptor (*Sass_C_Import_Callback);
;;; Typedef defining the importer c function prototype
; typedef struct Sass_Import** (*Sass_C_Import_Fn) (const char* url, const char* prev, void* cookie);

;;; Creators for custom importer callback (with some additional pointer)
;;; The pointer is mostly used to store the callback into the actual binding
; Sass_C_Import_Callback sass_make_importer (Sass_C_Import_Fn, void* cookie);

;;; Getters for import function descriptors
; Sass_C_Import_Fn sass_import_get_function (Sass_C_Import_Callback fn);
; void* sass_import_get_cookie (Sass_C_Import_Callback fn);

;;; Deallocator for associated memory
; void sass_delete_importer (Sass_C_Import_Callback fn);

;;; Creator for sass custom importer return argument list
; struct Sass_Import** sass_make_import_list (size_t length);
;;; Creator for a single import entry returned by the custom importer inside the list
; struct Sass_Import* sass_make_import_entry (const char* path, char* source, char* srcmap);
; struct Sass_Import* sass_make_import (const char* path, const char* base, char* source, char* srcmap);
;;; set error message to abort import and to print out a message (path from existing object is used in output)
; struct Sass_Import* sass_import_set_error(struct Sass_Import* import, const char* message, size_t line, size_t col);

;;; Setters to insert an entry into the import list (you may also use [] access directly)
;;; Since we are dealing with pointers they should have a guaranteed and fixed size
; void sass_import_set_list_entry (struct Sass_Import** list, size_t idx, struct Sass_Import* entry);
; struct Sass_Import* sass_import_get_list_entry (struct Sass_Import** list, size_t idx);

;;; Getters for import entry
; const char* sass_import_get_path (struct Sass_Import*);
; const char* sass_import_get_base (struct Sass_Import*);
; const char* sass_import_get_source (struct Sass_Import*);
; const char* sass_import_get_srcmap (struct Sass_Import*);
;;; Explicit functions to take ownership of these items
;;; The property on our struct will be reset to NULL
; char* sass_import_take_source (struct Sass_Import*);
; char* sass_import_take_srcmap (struct Sass_Import*);
;;; Getters from import error entry
; size_t sass_import_get_error_line (struct Sass_Import*);
; size_t sass_import_get_error_column (struct Sass_Import*);
; const char* sass_import_get_error_message (struct Sass_Import*);

;;; Deallocator for associated memory (incl. entries)
; void sass_delete_import_list (struct Sass_Import**);
;;; Just in case we have some stray import structs
; void sass_delete_import (struct Sass_Import*);


;;; Forward declaration
; struct Sass_C_Function_Descriptor;

;;; Typedef defining null terminated list of custom callbacks
; typedef struct Sass_C_Function_Descriptor* (*Sass_C_Function_List);
; typedef struct Sass_C_Function_Descriptor (*Sass_C_Function_Callback);
;;; Typedef defining custom function prototype and its return value type
; typedef union Sass_Value*(*Sass_C_Function) (const union Sass_Value*, void* cookie);


;;; Creators for sass function list and function descriptors
; Sass_C_Function_List sass_make_function_list (size_t length);
; Sass_C_Function_Callback sass_make_function (const char* signature, Sass_C_Function fn, void* cookie);

;;; Setters and getters for callbacks on function lists
; Sass_C_Function_Callback sass_function_get_list_entry(Sass_C_Function_List list, size_t pos);
; void sass_function_set_list_entry(Sass_C_Function_List list, size_t pos, Sass_C_Function_Callback cb);

;;; Getters for custom function descriptors
; const char* sass_function_get_signature (Sass_C_Function_Callback fn);
; Sass_C_Function sass_function_get_function (Sass_C_Function_Callback fn);
; void* sass_function_get_cookie (Sass_C_Function_Callback fn);

) ; END MODULE
