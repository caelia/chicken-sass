;;; sass-context.scm -- Context module for libsass binding.
;;;
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module sass-context
        ( make-options make-file-context make-data-context
          input-context-type compile-input-context make-compiler
          compiler-parse compiler-execute delete-compiler
          delete-input-context get-context get-options
          set-ctx-options! opt-precision opt-source-comments
          opt-source-map-embed opt-source-map-contents opt-omit-source-map-url
          opt-is-indented-syntax-src opt-indent opt-linefeed
          opt-input-path opt-output-path opt-plugin-path
          opt-include-path opt-source-map-file opt-source-map-root
          opt-c-headers opt-c-importers opt-c-functions
          opt-precision-set! opt-output-style-set! opt-source-comments-set!
          opt-source-map-embed-set! opt-source-map-contents-set! opt-omit-source-map-url-set!
          opt-is-indented-syntax-src-set! opt-indent-set! opt-linefeed-set!
          opt-input-path-set! opt-output-path-set! opt-plugin-path-set!
          opt-include-path-set! opt-source-map-file-set! opt-source-map-root-set!
          opt-c-headers-set! opt-c-importers-set! opt-c-functions-set!
          set-options! output-string error-status
          error-json error-text error-message
          error-file error-src error-line
          error-column source-map-string included-files
          included-files-size take-error-json take-error-text
          take-error-message take-error-file take-output-string
          take-source-map-string take-included-files compiler-get-state
          compiler-get-context compiler-get-import-stack-size compiler-get-last-import
          compiler-get-import-entry push-plugin-path push-include-path )
        
        (import scheme chicken)
        (import foreign)
        (use foreigners)
        ;(import sass-values sass-functions)
        (use sass-values sass-functions)

(foreign-declare "#include <sass_context.h>")

(include "sass-common.scm")

(define-foreign-type sass-context (struct "Sass_Context"))
(define-foreign-type sass-file-context (struct "Sass_File_Context"))
(define-foreign-type sass-data-context (struct "Sass_Data_Context"))
;;; Forward declaration
; struct Sass_Context; // : Sass_Options
; struct Sass_File_Context; // : Sass_Context
; struct Sass_Data_Context; // : Sass_Context
(define-record-type context
  (make-context ptr)
  context?
  (ptr context-ptr))
(define-record-type input-context
  (make-input-context type ctx ptr)
  input-context?
  (type input-context-type)
  (ptr input-context-ptr))

(define-foreign-enum-type (compiler-state int)
  (compiler-state->int int->compiler-state)
  ((created compiler/created) SASS_COMPILER_CREATED)
  ((parsed compiler/parsed) SASS_COMPILER_PARSED)
  ((executed compiler/executed) SASS_COMPILER_EXECUTED))

;;; Compiler states
; enum Sass_Compiler_State {
;   SASS_COMPILER_CREATED,
;   SASS_COMPILER_PARSED,
;   SASS_COMPILER_EXECUTED
; };

(define (context-type ctx)
  (cond
    ((context? ctx) 'context)
    ((input-context? ctx) (input-context-type ctx))
    (else (eprintf "~A is not a context object." ctx))))

(define-syntax ictx-handler
  (ir-macro-transformer
    (lambda (exp inject compare?)
      (let ((file-handler (cadr exp))
            (data-handler (caddr exp)))
        `(case (input-context-type ,(inject 'ictx))
          ((file) ,file-handler)
          ((data) ,data-handler))))))

(define-syntax ctx-handler
  (ir-macro-transformer
    (lambda (exp inject compare?)
      (let ((ctx-handler (cadr exp))
            (file-handler (caddr exp))
            (data-handler (cadddr exp)))
        `(case (context-type ,(inject 'ctx))
          ((context) ,ctx-handler)
          ((file) ,file-handler)
          ((data) ,data-handler))))))
          
(define (get-ctx-ptr ctx)
  (case (context-type ctx)
    ((context) (context-ptr ctx))
    ((file data) (input-context-ptr ctx))))


;;; Create and initialize an option struct
(define make-options  ; EXPORT
  (foreign-lambda (c-pointer sass-options)
                  sass_make_options))
; struct Sass_Options* sass_make_options (void);
;;; Create and initialize a specific context
(define %make-file-context
  (foreign-lambda (c-pointer sass-file-context)
                  sass_make_file_context
                  c-string))
; struct Sass_File_Context* sass_make_file_context (const char* input_path);
(define (make-file-context filename)  ; EXPORT
  (let ((ptr (%make-file-context filename)))
    (make-input-context 'file ptr))) 

(define %make-data-context
  (foreign-lambda (c-pointer sass-data-context)
                  sass_make_data_context
                  c-string))
; struct Sass_Data_Context* sass_make_data_context (char* source_string);
(define (make-data-context data)  ; EXPORT
  (let ((ptr (%make-data-context data)))
    (make-input-context 'data ptr)))

;;; Call the compilation step for the specific context
(define %compile-file-context
  (foreign-lambda int
                  sass_compile_file_context
                  (c-pointer sass-file-context)))
; int sass_compile_file_context (struct Sass_File_Context* ctx);
(define %compile-data-context
  (foreign-lambda int
                  sass_compile_data_context
                  (c-pointer sass-data-context)))
; int sass_compile_data_context (struct Sass_Data_Context* ctx);

(define (compile-input-context ictx)   ; EXPORT
  ((ictx-handler %compile-file-context %compile-data-context) (input-context-ptr ictx)))

;;; Create a sass compiler instance for more control
(define %make-file-compiler
  (foreign-lambda (c-pointer sass-compiler)
                  sass_make_file_compiler
                  (c-pointer sass-file-context)))
; struct Sass_Compiler* sass_make_file_compiler (struct Sass_File_Context* file_ctx);
(define %make-data-compiler
  (foreign-lambda (c-pointer sass-compiler)
                  sass_make_data_compiler
                  (c-pointer sass-data-context)))
; struct Sass_Compiler* sass_make_data_compiler (struct Sass_Data_Context* data_ctx);
(define (make-compiler ictx)  ; EXPORT
  ((ictx-handler %make-file-compiler %make-data-compiler) (input-context-ptr ictx)))

;;; Execute the different compilation steps individually
;;; Usefull if you only want to query the included files
(define compiler-parse  ; EXPORT
  (foreign-lambda int sass_compiler_parse (c-pointer sass-compiler)))
; int sass_compiler_parse(struct Sass_Compiler* compiler);
(define compiler-execute  ; EXPORT
  (foreign-lambda int sass_compiler_execute (c-pointer sass-compiler)))
; int sass_compiler_execute(struct Sass_Compiler* compiler);

;;; Release all memory allocated with the compiler
;;; This does _not_ include any contexts or options
(define delete-compiler   ; EXPORT
  (foreign-lambda void sass_delete_compiler (c-pointer sass-compiler)))
; void sass_delete_compiler(struct Sass_Compiler* compiler);

;;; Release all memory allocated and also ourself
(define %delete-file-context
  (foreign-lambda void sass_delete_file_context (c-pointer sass-file-context)))
; void sass_delete_file_context (struct Sass_File_Context* ctx);
(define %delete-data-context
  (foreign-lambda void sass_delete_data_context (c-pointer sass-data-context)))
; void sass_delete_data_context (struct Sass_Data_Context* ctx);
(define (delete-input-context ictx)   ; EXPORT
  ((ictx-handler %delete-file-context %delete-data-context) (input-context-ptr ictx)))

;;; Getters for context from specific implementation
(define %file-context-get-context
  (foreign-lambda (c-pointer sass-context)
                  sass_file_context_get_context
                  (c-pointer sass-file-context)))

(define %data-context-get-context
  (foreign-lambda (c-pointer sass-context)
                  sass_data_context_get_context
                  (c-pointer sass-data-context)))
; struct Sass_Context* sass_file_context_get_context (struct Sass_File_Context* file_ctx);
; struct Sass_Context* sass_data_context_get_context (struct Sass_Data_Context* data_ctx);

(define (get-context ictx)   ; EXPORT
  (make-context
    ((ictx-handler %file-context-get-context %data-context-get-context)
      (input-context-ptr ictx))))

;;; Getters for Context_Options from Sass_Context
(define %context-get-options
  (foreign-lambda (c-pointer sass-options)
                  sass_context_get_options
                  (c-pointer sass-context)))
; struct Sass_Options* sass_context_get_options (struct Sass_Context* ctx);
(define %file-context-get-options
  (foreign-lambda (c-pointer sass-options)
                  sass_file_context_get_options
                  (c-pointer sass-file-context)))
; struct Sass_Options* sass_file_context_get_options (struct Sass_File_Context* file_ctx);
(define %data-context-get-options
  (foreign-lambda (c-pointer sass-options)
                  sass_data_context_get_options
                  (c-pointer sass-data-context)))
; struct Sass_Options* sass_data_context_get_options (struct Sass_Data_Context* data_ctx);

(define (get-options ctx)   ; EXPORT
  ((ctx-handler %context-get-options %file-context-get-options %data-context-get-options)
    (get-ctx-ptr ctx)))

; (define (get-options-ptr obj)   ; EXPORT
  ; (options-ptr (if (options? obj) obj (get-options obj))))
    

(define %file-context-set-options!
  (foreign-lambda void
                  sass_file_context_set_options
                  (c-pointer sass-file-context) (c-pointer sass-options)))
; void sass_file_context_set_options (struct Sass_File_Context* file_ctx, struct Sass_Options* opt);
(define %data-context-set-options!
  (foreign-lambda void
                  sass_data_context_set_options
                  (c-pointer sass-data-context) (c-pointer sass-options)))
; void sass_data_context_set_options (struct Sass_Data_Context* data_ctx, struct Sass_Options* opt);

(define (set-ctx-options! ictx options)    ; EXPORT
  ((ictx-handler %file-context-set-options! %data-context-set-options!)
    (input-context-ptr ictx) options))

;;; Getters for Context_Option values
(define %option-get-precision
  (foreign-lambda int sass_option_get_precision (c-pointer sass-options)))
; int sass_option_get_precision (struct Sass_Options* options);
(define (opt-precision obj)   ; EXPORT
  (%option-get-precision obj))

(define opt-output-style
  (foreign-lambda output-style sass_option_get_output_style (c-pointer sass-options)))
; enum Sass_Output_Style sass_option_get_output_style (struct Sass_Options* options);

(define opt-source-comments
  (foreign-lambda bool sass_option_get_source_comments (c-pointer sass-options)))
; bool sass_option_get_source_comments (struct Sass_Options* options);

(define opt-source-map-embed
  (foreign-lambda bool sass_option_get_source_map_embed (c-pointer sass-options)))
; bool sass_option_get_source_map_embed (struct Sass_Options* options);

(define opt-source-map-contents
  (foreign-lambda bool sass_option_get_source_map_contents (c-pointer sass-options)))
; bool sass_option_get_source_map_contents (struct Sass_Options* options);

(define opt-omit-source-map-url
  (foreign-lambda bool sass_option_get_omit_source_map_url (c-pointer sass-options)))
; bool sass_option_get_omit_source_map_url (struct Sass_Options* options);

(define opt-is-indented-syntax-src
  (foreign-lambda bool sass_option_get_is_indented_syntax_src (c-pointer sass-options)))
; bool sass_option_get_is_indented_syntax_src (struct Sass_Options* options);

(define opt-indent
  (foreign-lambda c-string sass_option_get_indent (c-pointer sass-options)))
; const char* sass_option_get_indent (struct Sass_Options* options);

(define opt-linefeed
  (foreign-lambda c-string sass_option_get_linefeed (c-pointer sass-options)))
; const char* sass_option_get_linefeed (struct Sass_Options* options);

(define opt-input-path
  (foreign-lambda c-string sass_option_get_input_path (c-pointer sass-options)))
; const char* sass_option_get_input_path (struct Sass_Options* options);

(define opt-output-path
  (foreign-lambda c-string sass_option_get_output_path (c-pointer sass-options)))
; const char* sass_option_get_output_path (struct Sass_Options* options);

(define opt-plugin-path
  (foreign-lambda c-string sass_option_get_plugin_path (c-pointer sass-options)))
; const char* sass_option_get_plugin_path (struct Sass_Options* options);

(define opt-include-path
  (foreign-lambda c-string sass_option_get_include_path (c-pointer sass-options)))
; const char* sass_option_get_include_path (struct Sass_Options* options);

(define opt-source-map-file
  (foreign-lambda c-string sass_option_get_source_map_file (c-pointer sass-options)))
; const char* sass_option_get_source_map_file (struct Sass_Options* options);

(define opt-source-map-root
  (foreign-lambda c-string sass_option_get_source_map_root (c-pointer sass-options)))
; const char* sass_option_get_source_map_root (struct Sass_Options* options);

(define opt-c-headers
  (foreign-lambda importer-list sass_option_get_c_headers (c-pointer sass-options)))
; Sass_Importer_List sass_option_get_c_headers (struct Sass_Options* options);

(define opt-c-importers
  (foreign-lambda importer-list sass_option_get_c_importers (c-pointer sass-options)))
; Sass_Importer_List sass_option_get_c_importers (struct Sass_Options* options);

(define opt-c-functions
  (foreign-lambda function-list sass_option_get_c_functions (c-pointer sass-options)))
; Sass_Function_List sass_option_get_c_functions (struct Sass_Options* options);


;;; Setters for Context_Option values
(define opt-precision-set!
  (foreign-lambda void sass_option_set_precision (c-pointer sass-options) int))
; void sass_option_set_precision (struct Sass_Options* options, int precision);

(define opt-output-style-set!
  (foreign-lambda void sass_option_set_output_style (c-pointer sass-options) output-style))
; void sass_option_set_output_style (struct Sass_Options* options, enum Sass_Output_Style output_style);

(define opt-source-comments-set!
  (foreign-lambda void sass_option_set_source_comments (c-pointer sass-options) bool))
; void sass_option_set_source_comments (struct Sass_Options* options, bool source_comments);

(define opt-source-map-embed-set!
  (foreign-lambda void sass_option_set_source_map_embed (c-pointer sass-options) bool))
; void sass_option_set_source_map_embed (struct Sass_Options* options, bool source_map_embed);

(define opt-source-map-contents-set!
  (foreign-lambda void sass_option_set_source_map_contents (c-pointer sass-options) bool))
; void sass_option_set_source_map_contents (struct Sass_Options* options, bool source_map_contents);

(define opt-omit-source-map-url-set!
  (foreign-lambda void sass_option_set_omit_source_map_url (c-pointer sass-options) bool))
; void sass_option_set_omit_source_map_url (struct Sass_Options* options, bool omit_source_map_url);

(define opt-is-indented-syntax-src-set!
  (foreign-lambda void sass_option_set_is_indented_syntax_src (c-pointer sass-options) bool))
; void sass_option_set_is_indented_syntax_src (struct Sass_Options* options, bool is_indented_syntax_src);

(define opt-indent-set!
  (foreign-lambda void sass_option_set_indent (c-pointer sass-options) c-string))
; void sass_option_set_indent (struct Sass_Options* options, const char* indent);

(define opt-linefeed-set!
  (foreign-lambda void sass_option_set_linefeed (c-pointer sass-options) c-string))
; void sass_option_set_linefeed (struct Sass_Options* options, const char* linefeed);

(define opt-input-path-set!
  (foreign-lambda void sass_option_set_input_path (c-pointer sass-options) c-string))
; void sass_option_set_input_path (struct Sass_Options* options, const char* input_path);

(define opt-output-path-set!
  (foreign-lambda void sass_option_set_output_path (c-pointer sass-options) c-string))
; void sass_option_set_output_path (struct Sass_Options* options, const char* output_path);

(define opt-plugin-path-set!
  (foreign-lambda void sass_option_set_plugin_path (c-pointer sass-options) c-string))
; void sass_option_set_plugin_path (struct Sass_Options* options, const char* plugin_path);

(define opt-include-path-set!
  (foreign-lambda void sass_option_set_include_path (c-pointer sass-options) c-string))
; void sass_option_set_include_path (struct Sass_Options* options, const char* include_path);

(define opt-source-map-file-set!
  (foreign-lambda void sass_option_set_source_map_file (c-pointer sass-options) c-string))
; void sass_option_set_source_map_file (struct Sass_Options* options, const char* source_map_file);

(define opt-source-map-root-set!
  (foreign-lambda void sass_option_set_source_map_root (c-pointer sass-options) c-string))
; void sass_option_set_source_map_root (struct Sass_Options* options, const char* source_map_root);

(define opt-c-headers-set!
  (foreign-lambda void sass_option_set_c_headers (c-pointer sass-options) importer-list))
; void sass_option_set_c_headers (struct Sass_Options* options, Sass_Importer_List c_headers);

(define opt-c-importers-set!
  (foreign-lambda void sass_option_set_c_importers (c-pointer sass-options) importer-list))
; void sass_option_set_c_importers (struct Sass_Options* options, Sass_Importer_List c_importers);

(define opt-c-functions-set!
  (foreign-lambda void sass_option_set_c_functions (c-pointer sass-options) function-list))
; void sass_option_set_c_functions (struct Sass_Options* options, Sass_Function_List c_functions);

(define (set-options! opts #!key (precision #f) (output-style #f) (source-comments 'undefined)
                      (source-map-embed 'undefined) (source-map-contents 'undefined)
                      (omit-source-map-url 'undefined) (is-indented-syntax-src 'undefined)
                      (indent #f) (linefeed #f) (input-path #f) (output-path #f)
                      (plugin-path #f) (include-path #f) (source-map-file #f)
                      (source-map-root #f) (c-headers #f) (c-importers #f) (c-functions #f))
  (let ((defined? (lambda (kwarg) (not (eqv? kwarg 'undefined)))))
    (when precision (opt-precision-set! opts precision))
    (when output-style (opt-output-style-set! opts output-style))
    (when (defined? source-comments) (opt-source-comments-set! opts source-comments))
    (when (defined? source-map-embed) (opt-source-map-embed-set! opts source-map-embed))
    (when (defined? source-map-contents) (opt-source-map-contents-set! opts source-map-contents))
    (when (defined? omit-source-map-url) (opt-omit-source-map-url-set! opts omit-source-map-url))
    (when (defined? is-indented-syntax-src) (opt-is-indented-syntax-src-set! opts is-indented-syntax-src))
    (when indent (opt-indent-set! opts indent))
    (when linefeed (opt-linefeed-set! opts linefeed))
    (when input-path (opt-input-path-set! opts input-path))
    (when output-path (opt-output-path-set! opts output-path))
    (when plugin-path (opt-plugin-path-set! opts plugin-path))
    (when include-path (opt-include-path-set! opts include-path))
    (when source-map-file (opt-source-map-file-set! opts source-map-file))
    (when source-map-root (opt-source-map-root-set! opts source-map-root))
    (when c-headers (opt-c-headers-set! opts c-headers))
    (when c-importers (opt-c-importers-set! opts c-importers))
    (when c-functions (opt-c-functions-set! opts c-functions))))

;;; Getters for Sass_Context values
(define %context-get-output-string
  (foreign-lambda c-string sass_context_get_output_string (c-pointer sass-context)))
; const char* sass_context_get_output_string (struct Sass_Context* ctx);
(define (output-string ctx)      ; EXPORT
  (%context-get-output-string (context-ptr ctx)))

(define %context-get-error-status
  (foreign-lambda int sass_context_get_error_status (c-pointer sass-context)))
; int sass_context_get_error_status (struct Sass_Context* ctx);
(define (error-status ctx)      ; EXPORT
  (%context-get-error-status (context-ptr ctx)))

(define %context-get-error-json
  (foreign-lambda c-string sass_context_get_error_json (c-pointer sass-context)))
; const char* sass_context_get_error_json (struct Sass_Context* ctx);
(define (error-json ctx)      ; EXPORT
  (%context-get-error-json (context-ptr ctx)))

(define %context-get-error-text
  (foreign-lambda c-string sass_context_get_error_text (c-pointer sass-context)))
; const char* sass_context_get_error_text (struct Sass_Context* ctx);
(define (error-text ctx)      ; EXPORT
  (%context-get-error-text (context-ptr ctx)))

(define %context-get-error-message
  (foreign-lambda c-string sass_context_get_error_message (c-pointer sass-context)))
; const char* sass_context_get_error_message (struct Sass_Context* ctx);
(define (error-message ctx)      ; EXPORT
  (%context-get-error-message (context-ptr ctx)))

(define %context-get-error-file
  (foreign-lambda c-string sass_context_get_error_file (c-pointer sass-context)))
; const char* sass_context_get_error_file (struct Sass_Context* ctx);
(define (error-file ctx)      ; EXPORT
  (%context-get-error-file (context-ptr ctx)))

(define %context-get-error-src
  (foreign-lambda c-string sass_context_get_error_src (c-pointer sass-context)))
; const char* sass_context_get_error_src (struct Sass_Context* ctx);
(define (error-src ctx)      ; EXPORT
  (%context-get-error-src (context-ptr ctx)))

(define %context-get-error-line
  (foreign-lambda size_t sass_context_get_error_line (c-pointer sass-context)))
; size_t sass_context_get_error_line (struct Sass_Context* ctx);
(define (error-line ctx)      ; EXPORT
  (%context-get-error-line (context-ptr ctx)))

(define %context-get-error-column
  (foreign-lambda size_t sass_context_get_error_column (c-pointer sass-context)))
; size_t sass_context_get_error_column (struct Sass_Context* ctx);
(define (error-column ctx)      ; EXPORT
  (%context-get-error-column (context-ptr ctx)))

(define %context-get-source-map-string
  (foreign-lambda c-string sass_context_get_source_map_string (c-pointer sass-context)))
; const char* sass_context_get_source_map_string (struct Sass_Context* ctx);
(define (source-map-string ctx)      ; EXPORT
  (%context-get-source-map-string (context-ptr ctx)))

(define %context-get-included-files
  (foreign-lambda c-string-list sass_context_get_included_files (c-pointer sass-context)))
; char** sass_context_get_included_files (struct Sass_Context* ctx);
(define (included-files ctx)      ; EXPORT
  (%context-get-included-files (context-ptr ctx)))

;;; Calculate the size of the stored null terminated array
(define %context-get-included-files-size
  (foreign-lambda size_t sass_context_get_included_files_size (c-pointer sass-context)))
; size_t sass_context_get_included_files_size (struct Sass_Context* ctx);
(define (included-files-size ctx)      ; EXPORT
  (%context-get-included-files-size (context-ptr ctx)))


;;; Take ownership of memory (value on context is set to 0)
(define %context-take-error-json
  (foreign-lambda c-string sass_context_take_error_json (c-pointer sass-context)))
; char* sass_context_take_error_json (struct Sass_Context* ctx);
(define (take-error-json ctx)      ; EXPORT
  (%context-take-error-json (context-ptr ctx)))

(define %context-take-error-text
  (foreign-lambda c-string sass_context_take_error_text (c-pointer sass-context)))
; char* sass_context_take_error_text (struct Sass_Context* ctx);
(define (take-error-text ctx)      ; EXPORT
  (%context-take-error-text (context-ptr ctx)))

(define %context-take-error-message
  (foreign-lambda c-string sass_context_take_error_message (c-pointer sass-context)))
; char* sass_context_take_error_message (struct Sass_Context* ctx);
(define (take-error-message ctx)      ; EXPORT
  (%context-take-error-message (context-ptr ctx)))

(define %context-take-error-file
  (foreign-lambda c-string sass_context_take_error_file (c-pointer sass-context)))
; char* sass_context_take_error_file (struct Sass_Context* ctx);
(define (take-error-file ctx)      ; EXPORT
  (%context-take-error-file (context-ptr ctx)))

(define %context-take-output-string
  (foreign-lambda c-string sass_context_take_output_string (c-pointer sass-context)))
; char* sass_context_take_output_string (struct Sass_Context* ctx);
(define (take-output-string ctx)      ; EXPORT
  (%context-take-output-string (context-ptr ctx)))

(define %context-take-source-map-string
  (foreign-lambda c-string sass_context_take_source_map_string (c-pointer sass-context)))
; char* sass_context_take_source_map_string (struct Sass_Context* ctx);
(define (take-source-map-string ctx)      ; EXPORT
  (%context-take-source-map-string (context-ptr ctx)))

(define %context-take-included-files
  (foreign-lambda c-string-list sass_context_take_included_files (c-pointer sass-context)))
; char** sass_context_take_included_files (struct Sass_Context* ctx);
(define (take-included-files ctx)      ; EXPORT
  (%context-take-included-files (context-ptr ctx)))


;;; Getters for Sass_Compiler options
(define compiler-get-state    ; EXPORT
  (foreign-lambda compiler-state sass_compiler_get_state (c-pointer sass-compiler)))
; enum Sass_Compiler_State sass_compiler_get_state(struct Sass_Compiler* compiler);
(define compiler-get-context    ; EXPORT
  (foreign-lambda (c-pointer sass-context) sass_compiler_get_context (c-pointer sass-compiler)))
; struct Sass_Context* sass_compiler_get_context(struct Sass_Compiler* compiler);
(define compiler-get-import-stack-size    ; EXPORT
  (foreign-lambda size_t sass_compiler_get_import_stack_size (c-pointer sass-compiler)))
; size_t sass_compiler_get_import_stack_size(struct Sass_Compiler* compiler);
(define compiler-get-last-import    ; EXPORT
  (foreign-lambda import-entry sass_compiler_get_last_import (c-pointer sass-compiler)))
; Sass_Import_Entry sass_compiler_get_last_import(struct Sass_Compiler* compiler);
(define compiler-get-import-entry   ; EXPORT
  (foreign-lambda import-entry sass_compiler_get_import_entry (c-pointer sass-compiler) size_t))
; Sass_Import_Entry sass_compiler_get_import_entry(struct Sass_Compiler* compiler, size_t idx);

;;; Push function for paths (no manipulation support for now)
(define %option-push-plugin-path
  (foreign-lambda void sass_option_push_plugin_path (c-pointer sass-options) c-string))
; void sass_option_push_plugin_path (struct Sass_Options* options, const char* path);
(define (push-plugin-path opts path)    ; EXPORT
  (%option-push-plugin-path opts path))

(define %option-push-include-path
  (foreign-lambda void sass_option_push_include_path (c-pointer sass-options) c-string))
; void sass_option_push_include_path (struct Sass_Options* options, const char* path);
(define (push-include-path opts path)   ; EXPORT
  (%option-push-include-path opts path))

) ; END MODULE
