;;; sass-context.scm -- Context module for libsass binding.
;;;
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module sass-context
        *
        (import scheme chicken)
        (import foreign)
        (use foreigners)

(foreign-declare "#include <sass_context.h>")

(include "sass-common-types.scm")

(define-foreign-type sass-context (struct "Sass_Context"))
(define-foreign-type sass-file-context (struct "Sass_File_Context"))
(define-foreign-type sass-data-context (struct "Sass_Data_Context"))
;;; Forward declaration
; struct Sass_Context; // : Sass_Options
; struct Sass_File_Context; // : Sass_Context
; struct Sass_Data_Context; // : Sass_Context

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

;;; Create and initialize an option struct
(define make-options
  (foreign-lambda (c-pointer sass-options) sass_make_options))
; struct Sass_Options* sass_make_options (void);
;;; Create and initialize a specific context
(define (make-file-context )
  ((foreign-lambda (struct "Sass_File_Context*") sass_make_file_context (const char* input_path)) ))
; struct Sass_File_Context* sass_make_file_context (const char* input_path);
(define (make-data-context )
  ((foreign-lambda (struct "Sass_Data_Context*") sass_make_data_context (char* source_string)) ))
; struct Sass_Data_Context* sass_make_data_context (char* source_string);

;;; Call the compilation step for the specific context
(define (compile-file-context )
  ((foreign-lambda int sass_compile_file_context (struct Sass_File_Context* ctx)) ))
; int sass_compile_file_context (struct Sass_File_Context* ctx);
(define (compile-data-context )
  ((foreign-lambda int sass_compile_data_context (struct Sass_Data_Context* ctx)) ))
; int sass_compile_data_context (struct Sass_Data_Context* ctx);

;;; Create a sass compiler instance for more control
(define (make-file-compiler )
  ((foreign-lambda (struct "Sass_Compiler*") sass_make_file_compiler (struct Sass_File_Context* file_ctx)) ))
; struct Sass_Compiler* sass_make_file_compiler (struct Sass_File_Context* file_ctx);
(define (make-data-compiler )
  ((foreign-lambda (struct "Sass_Compiler*") sass_make_data_compiler (struct Sass_Data_Context* data_ctx)) ))
; struct Sass_Compiler* sass_make_data_compiler (struct Sass_Data_Context* data_ctx);

;;; Execute the different compilation steps individually
;;; Usefull if you only want to query the included files
(define (compiler-parse )
  ((foreign-lambda int sass_compiler_parse(struct Sass_Compiler* compiler)) ))
; int sass_compiler_parse(struct Sass_Compiler* compiler);
(define (compiler-execute )
  ((foreign-lambda int sass_compiler_execute(struct Sass_Compiler* compiler)) ))
; int sass_compiler_execute(struct Sass_Compiler* compiler);

;;; Release all memory allocated with the compiler
;;; This does _not_ include any contexts or options
(define (delete-compiler )
  ((foreign-lambda void sass_delete_compiler(struct Sass_Compiler* compiler)) ))
; void sass_delete_compiler(struct Sass_Compiler* compiler);

;;; Release all memory allocated and also ourself
(define (delete-file-context )
  ((foreign-lambda void sass_delete_file_context (struct Sass_File_Context* ctx)) ))
; void sass_delete_file_context (struct Sass_File_Context* ctx);
(define (delete-data-context )
  ((foreign-lambda void sass_delete_data_context (struct Sass_Data_Context* ctx)) ))
; void sass_delete_data_context (struct Sass_Data_Context* ctx);

;;; Getters for context from specific implementation
(define (file-context-get-context )
  ((foreign-lambda (struct "Sass_Context*") sass_file_context_get_context (struct Sass_File_Context* file_ctx)) ))
; struct Sass_Context* sass_file_context_get_context (struct Sass_File_Context* file_ctx);
(define (data-context-get-context )
  ((foreign-lambda (struct "Sass_Context*") sass_data_context_get_context (struct Sass_Data_Context* data_ctx)) ))
; struct Sass_Context* sass_data_context_get_context (struct Sass_Data_Context* data_ctx);

;;; Getters for Context_Options from Sass_Context
(define (context-get-options )
  ((foreign-lambda (c-pointer sass-options) sass_context_get_options (struct Sass_Context* ctx)) ))
; struct Sass_Options* sass_context_get_options (struct Sass_Context* ctx);
(define (file-context-get-options )
  ((foreign-lambda (c-pointer sass-options) sass_file_context_get_options (struct Sass_File_Context* file_ctx)) ))
; struct Sass_Options* sass_file_context_get_options (struct Sass_File_Context* file_ctx);
(define (data-context-get-options )
  ((foreign-lambda (c-pointer sass-options) sass_data_context_get_options (struct Sass_Data_Context* data_ctx)) ))
; struct Sass_Options* sass_data_context_get_options (struct Sass_Data_Context* data_ctx);
(define (file-context-set-options )
  ((foreign-lambda void sass_file_context_set_options (struct Sass_File_Context* file_ctx, (c-pointer sass-options) opt)) ))
; void sass_file_context_set_options (struct Sass_File_Context* file_ctx, struct Sass_Options* opt);
(define (data-context-set-options )
  ((foreign-lambda void sass_data_context_set_options (struct Sass_Data_Context* data_ctx, (c-pointer sass-options) opt)) ))
; void sass_data_context_set_options (struct Sass_Data_Context* data_ctx, struct Sass_Options* opt);


;;; Getters for Context_Option values
(define (option-get-precision )
  ((foreign-lambda int sass_option_get_precision (c-pointer sass-options)) ))
; int sass_option_get_precision (struct Sass_Options* options);
(define (option-get-output-style )
  ((foreign-lambda (enum "Sass_Output_Style") sass_option_get_output_style (c-pointer sass-options)) ))
; enum Sass_Output_Style sass_option_get_output_style (struct Sass_Options* options);
(define (option-get-source-comments )
  ((foreign-lambda bool sass_option_get_source_comments (c-pointer sass-options)) ))
; bool sass_option_get_source_comments (struct Sass_Options* options);
(define (option-get-source-map-embed )
  ((foreign-lambda bool sass_option_get_source_map_embed (c-pointer sass-options)) ))
; bool sass_option_get_source_map_embed (struct Sass_Options* options);
(define (option-get-source-map-contents )
  ((foreign-lambda bool sass_option_get_source_map_contents (c-pointer sass-options)) ))
; bool sass_option_get_source_map_contents (struct Sass_Options* options);
(define (option-get-omit-source-map-url )
  ((foreign-lambda bool sass_option_get_omit_source_map_url (c-pointer sass-options)) ))
; bool sass_option_get_omit_source_map_url (struct Sass_Options* options);
(define (option-get-is-indented-syntax-src )
  ((foreign-lambda bool sass_option_get_is_indented_syntax_src (c-pointer sass-options)) ))
; bool sass_option_get_is_indented_syntax_src (struct Sass_Options* options);
(define (option-get-indent )
  ((foreign-lambda (const char*) sass_option_get_indent (c-pointer sass-options)) ))
; const char* sass_option_get_indent (struct Sass_Options* options);
(define (option-get-linefeed )
  ((foreign-lambda (const char*) sass_option_get_linefeed (c-pointer sass-options)) ))
; const char* sass_option_get_linefeed (struct Sass_Options* options);
(define (option-get-input-path )
  ((foreign-lambda (const char*) sass_option_get_input_path (c-pointer sass-options)) ))
; const char* sass_option_get_input_path (struct Sass_Options* options);
(define (option-get-output-path )
  ((foreign-lambda (const char*) sass_option_get_output_path (c-pointer sass-options)) ))
; const char* sass_option_get_output_path (struct Sass_Options* options);
(define (option-get-plugin-path )
  ((foreign-lambda (const char*) sass_option_get_plugin_path (c-pointer sass-options)) ))
; const char* sass_option_get_plugin_path (struct Sass_Options* options);
(define (option-get-include-path )
  ((foreign-lambda (const char*) sass_option_get_include_path (c-pointer sass-options)) ))
; const char* sass_option_get_include_path (struct Sass_Options* options);
(define (option-get-source-map-file )
  ((foreign-lambda (const char*) sass_option_get_source_map_file (c-pointer sass-options)) ))
; const char* sass_option_get_source_map_file (struct Sass_Options* options);
(define (option-get-source-map-root )
  ((foreign-lambda (const char*) sass_option_get_source_map_root (c-pointer sass-options)) ))
; const char* sass_option_get_source_map_root (struct Sass_Options* options);
(define (option-get-c-headers )
  ((foreign-lambda Sass_Importer_List sass_option_get_c_headers (c-pointer sass-options)) ))
; Sass_Importer_List sass_option_get_c_headers (struct Sass_Options* options);
(define (option-get-c-importers )
  ((foreign-lambda Sass_Importer_List sass_option_get_c_importers (c-pointer sass-options)) ))
; Sass_Importer_List sass_option_get_c_importers (struct Sass_Options* options);
(define (option-get-c-functions )
  ((foreign-lambda Sass_Function_List sass_option_get_c_functions (c-pointer sass-options)) ))
; Sass_Function_List sass_option_get_c_functions (struct Sass_Options* options);

;;; Setters for Context_Option values
(define (option-set-precision )
  ((foreign-lambda void sass_option_set_precision (c-pointer sass-options) int precision)) ))
; void sass_option_set_precision (struct Sass_Options* options, int precision);
(define (option-set-output-style )
  ((foreign-lambda void sass_option_set_output_style (c-pointer sass-options) enum Sass_Output_Style output_style)) ))
; void sass_option_set_output_style (struct Sass_Options* options, enum Sass_Output_Style output_style);
(define (option-set-source-comments )
  ((foreign-lambda void sass_option_set_source_comments (c-pointer sass-options) bool source_comments)) ))
; void sass_option_set_source_comments (struct Sass_Options* options, bool source_comments);
(define (option-set-source-map-embed )
  ((foreign-lambda void sass_option_set_source_map_embed (c-pointer sass-options) bool source_map_embed)) ))
; void sass_option_set_source_map_embed (struct Sass_Options* options, bool source_map_embed);
(define (option-set-source-map-contents )
  ((foreign-lambda void sass_option_set_source_map_contents (c-pointer sass-options) bool source_map_contents)) ))
; void sass_option_set_source_map_contents (struct Sass_Options* options, bool source_map_contents);
(define (option-set-omit-source-map-url )
  ((foreign-lambda void sass_option_set_omit_source_map_url (c-pointer sass-options) bool omit_source_map_url)) ))
; void sass_option_set_omit_source_map_url (struct Sass_Options* options, bool omit_source_map_url);
(define (option-set-is-indented-syntax-src )
  ((foreign-lambda void sass_option_set_is_indented_syntax_src (c-pointer sass-options) bool is_indented_syntax_src)) ))
; void sass_option_set_is_indented_syntax_src (struct Sass_Options* options, bool is_indented_syntax_src);
(define (option-set-indent )
  ((foreign-lambda void sass_option_set_indent (c-pointer sass-options) const char* indent)) ))
; void sass_option_set_indent (struct Sass_Options* options, const char* indent);
(define (option-set-linefeed )
  ((foreign-lambda void sass_option_set_linefeed (c-pointer sass-options) const char* linefeed)) ))
; void sass_option_set_linefeed (struct Sass_Options* options, const char* linefeed);
(define (option-set-input-path )
  ((foreign-lambda void sass_option_set_input_path (c-pointer sass-options) const char* input_path)) ))
; void sass_option_set_input_path (struct Sass_Options* options, const char* input_path);
(define (option-set-output-path )
  ((foreign-lambda void sass_option_set_output_path (c-pointer sass-options) const char* output_path)) ))
; void sass_option_set_output_path (struct Sass_Options* options, const char* output_path);
(define (option-set-plugin-path )
  ((foreign-lambda void sass_option_set_plugin_path (c-pointer sass-options) const char* plugin_path)) ))
; void sass_option_set_plugin_path (struct Sass_Options* options, const char* plugin_path);
(define (option-set-include-path )
  ((foreign-lambda void sass_option_set_include_path (c-pointer sass-options) const char* include_path)) ))
; void sass_option_set_include_path (struct Sass_Options* options, const char* include_path);
(define (option-set-source-map-file )
  ((foreign-lambda void sass_option_set_source_map_file (c-pointer sass-options) const char* source_map_file)) ))
; void sass_option_set_source_map_file (struct Sass_Options* options, const char* source_map_file);
(define (option-set-source-map-root )
  ((foreign-lambda void sass_option_set_source_map_root (c-pointer sass-options) const char* source_map_root)) ))
; void sass_option_set_source_map_root (struct Sass_Options* options, const char* source_map_root);
(define (option-set-c-headers )
  ((foreign-lambda void sass_option_set_c_headers (c-pointer sass-options) Sass_Importer_List c_headers)) ))
; void sass_option_set_c_headers (struct Sass_Options* options, Sass_Importer_List c_headers);
(define (option-set-c-importers )
  ((foreign-lambda void sass_option_set_c_importers (c-pointer sass-options) Sass_Importer_List c_importers)) ))
; void sass_option_set_c_importers (struct Sass_Options* options, Sass_Importer_List c_importers);
(define (option-set-c-functions )
  ((foreign-lambda void sass_option_set_c_functions (c-pointer sass-options) Sass_Function_List c_functions)) ))
; void sass_option_set_c_functions (struct Sass_Options* options, Sass_Function_List c_functions);


;;; Getters for Sass_Context values
(define (context-get-output-string )
  ((foreign-lambda (const char*) sass_context_get_output_string (struct Sass_Context* ctx)) ))
; const char* sass_context_get_output_string (struct Sass_Context* ctx);
(define (context-get-error-status )
  ((foreign-lambda int sass_context_get_error_status (struct Sass_Context* ctx)) ))
; int sass_context_get_error_status (struct Sass_Context* ctx);
(define (context-get-error-json )
  ((foreign-lambda (const char*) sass_context_get_error_json (struct Sass_Context* ctx)) ))
; const char* sass_context_get_error_json (struct Sass_Context* ctx);
(define (context-get-error-text )
  ((foreign-lambda (const char*) sass_context_get_error_text (struct Sass_Context* ctx)) ))
; const char* sass_context_get_error_text (struct Sass_Context* ctx);
(define (context-get-error-message )
  ((foreign-lambda (const char*) sass_context_get_error_message (struct Sass_Context* ctx)) ))
; const char* sass_context_get_error_message (struct Sass_Context* ctx);
(define (context-get-error-file )
  ((foreign-lambda (const char*) sass_context_get_error_file (struct Sass_Context* ctx)) ))
; const char* sass_context_get_error_file (struct Sass_Context* ctx);
(define (context-get-error-src )
  ((foreign-lambda (const char*) sass_context_get_error_src (struct Sass_Context* ctx)) ))
; const char* sass_context_get_error_src (struct Sass_Context* ctx);
(define (context-get-error-line )
  ((foreign-lambda size_t sass_context_get_error_line (struct Sass_Context* ctx)) ))
; size_t sass_context_get_error_line (struct Sass_Context* ctx);
(define (context-get-error-column )
  ((foreign-lambda size_t sass_context_get_error_column (struct Sass_Context* ctx)) ))
; size_t sass_context_get_error_column (struct Sass_Context* ctx);
(define (context-get-source-map-string )
  ((foreign-lambda (const char*) sass_context_get_source_map_string (struct Sass_Context* ctx)) ))
; const char* sass_context_get_source_map_string (struct Sass_Context* ctx);
(define (context-get-included-files )
  ((foreign-lambda char** sass_context_get_included_files (struct Sass_Context* ctx)) ))
; char** sass_context_get_included_files (struct Sass_Context* ctx);

;;; Calculate the size of the stored null terminated array
(define (context-get-included-files-size )
  ((foreign-lambda size_t sass_context_get_included_files_size (struct Sass_Context* ctx)) ))
; size_t sass_context_get_included_files_size (struct Sass_Context* ctx);

;;; Take ownership of memory (value on context is set to 0)
(define (context-take-error-json )
  ((foreign-lambda char* sass_context_take_error_json (struct Sass_Context* ctx)) ))
; char* sass_context_take_error_json (struct Sass_Context* ctx);
(define (context-take-error-text )
  ((foreign-lambda char* sass_context_take_error_text (struct Sass_Context* ctx)) ))
; char* sass_context_take_error_text (struct Sass_Context* ctx);
(define (context-take-error-message )
  ((foreign-lambda char* sass_context_take_error_message (struct Sass_Context* ctx)) ))
; char* sass_context_take_error_message (struct Sass_Context* ctx);
(define (context-take-error-file )
  ((foreign-lambda char* sass_context_take_error_file (struct Sass_Context* ctx)) ))
; char* sass_context_take_error_file (struct Sass_Context* ctx);
(define (context-take-output-string )
  ((foreign-lambda char* sass_context_take_output_string (struct Sass_Context* ctx)) ))
; char* sass_context_take_output_string (struct Sass_Context* ctx);
(define (context-take-source-map-string )
  ((foreign-lambda char* sass_context_take_source_map_string (struct Sass_Context* ctx)) ))
; char* sass_context_take_source_map_string (struct Sass_Context* ctx);
(define (context-take-included-files )
  ((foreign-lambda char** sass_context_take_included_files (struct Sass_Context* ctx)) ))
; char** sass_context_take_included_files (struct Sass_Context* ctx);

;;; Getters for Sass_Compiler options
(define (compiler-get-state )
  ((foreign-lambda compiler-state sass_compiler_get_state(struct Sass_Compiler* compiler)) ))
; enum Sass_Compiler_State sass_compiler_get_state(struct Sass_Compiler* compiler);
(define (compiler-get-context )
  ((foreign-lambda (struct "Sass_Context*") sass_compiler_get_context(struct Sass_Compiler* compiler)) ))
; struct Sass_Context* sass_compiler_get_context(struct Sass_Compiler* compiler);
(define (compiler-get-import-stack-size )
  ((foreign-lambda size_t sass_compiler_get_import_stack_size(struct Sass_Compiler* compiler)) ))
; size_t sass_compiler_get_import_stack_size(struct Sass_Compiler* compiler);
(define (compiler-get-last-import )
  ((foreign-lambda Sass_Import_Entry sass_compiler_get_last_import(struct Sass_Compiler* compiler)) ))
; Sass_Import_Entry sass_compiler_get_last_import(struct Sass_Compiler* compiler);
(define (compiler-get-import-entry )
  ((foreign-lambda Sass_Import_Entry sass_compiler_get_import_entry(struct Sass_Compiler* compiler, size_t idx)) ))
; Sass_Import_Entry sass_compiler_get_import_entry(struct Sass_Compiler* compiler, size_t idx);

;;; Push function for paths (no manipulation support for now)
(define (option-push-plugin-path )
  ((foreign-lambda void sass_option_push_plugin_path (c-pointer sass-options) const char* path)) ))
; void sass_option_push_plugin_path (struct Sass_Options* options, const char* path);
(define (option-push-include-path )
  ((foreign-lambda void sass_option_push_include_path (c-pointer sass-options) const char* path)) ))
; void sass_option_push_include_path (struct Sass_Options* options, const char* path);

) ; END MODULE
