;;; sass.scm -- A wrapper for libsass.
;;;
;;;   Copyright © 2014 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module sass
        *
        (use lazy-ffi)
        (import foreign)
        (import foreigners)

#~"libsass.so.0"

(define SASS_STYLE_NESTED     0)
(define SASS_STYLE_EXPANDED   1)
(define SASS_STYLE_COMPACT    2)
(define SASS_STYLE_COMPRESSED 3)

(define SASS_SOURCE_COMMENTS_NONE 0)
(define SASS_SOURCE_COMMENTS_DEFAULT 1)
(define SASS_SOURCE_COMMENTS_MAP 2)

(define-foreign-record (options "struct sass_options")
  int output_style;
  int source_comments; // really want a bool, but C doesn't have them
  const char* include_paths;
  const char* image_path;

(define-foreign-record (context "struct sass_context")
  const char* source_string;
  char* output_string;
  struct sass_options options;
  int error_status;
  char* error_message;
  struct Sass_C_Function_Data* c_functions;
  char** included_files;
  int num_included_files;

(define-foreign-record (file-context "struct sass_file_context")
  const char* input_path;
  char* output_string;
  char* source_map_string;
  char* source_map_file;
  struct sass_options options;
  int error_status;
  char* error_message;
  struct Sass_C_Function_Data* c_functions;
  char** included_files;
  int num_included_files;

(define-foreign-record (folder-context "struct sass_folder_context")
  const char* search_path;
  const char* output_path;
  struct sass_options options;
  int error_status;
  char* error_message;
  struct Sass_C_Function_Data* c_functions;
  char** included_files;
  int num_included_files;


) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

