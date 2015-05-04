;;; sass-values.scm -- Values module for libsass binding.
;;;
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module sass-values
        *
        (import foreign)
        (use foreigners)

(foreign-declare "#include <sass_values.h>")

(define-foreign-enum-type (sass-tag int)
  (sass-tag->int int->sass-tag)
  ((boolean tag/boolean) SASS_BOOLEAN)
  ((number tag/number) SASS_NUMBER)
  ((color tag/color) SASS_COLOR)
  ((string tag/string) SASS_STRING)
  ((list tag/list) SASS_LIST)
  ((map tag/map) SASS_MAP)
  ((null tag/null) SASS_NULL)
  ((error tag/error) SASS_ERROR)
  ((warning tag/warning) SASS_WARNING))

(define-foreign-enum-type (separator int)
  (separator->int int->separator)
  ((comma separator/comma) SASS_COMMA)
  ((space separator/space) SASS_SPACE))

;; Return the sass tag for a generic sass value
;; Check is needed before accessing specific values!
(define (get-tag value)
  ((foreign-lambda (enum "Sass_Tag") sass_value_get_tag (c-pointer "Sass_Value")) value))
; enum Sass_Tag sass_value_get_tag (const union Sass_Value* v);

;; Check value to be of a specific type
;; Can also be used before accessing properties!
(define (null? value)
  ((foreign-lambda bool sass_value_is_null (c-pointer "Sass_Value")) value))
; bool sass_value_is_null (const union Sass_Value* v);
(define (number? value)
  ((foreign-lambda bool sass_value_is_number (c-pointer "Sass_Value")) value))
; bool sass_value_is_number (const union Sass_Value* v);
(define (string? value)
  ((foreign-lambda bool sass_value_is_string (c-pointer "Sass_Value")) value))
; bool sass_value_is_string (const union Sass_Value* v);
(define (boolean? value)
  ((foreign-lambda bool sass_value_is_boolean (c-pointer "Sass_Value")) value))
; bool sass_value_is_boolean (const union Sass_Value* v);
(define (color? value)
  ((foreign-lambda bool sass_value_is_color (c-pointer "Sass_Value")) value))
; bool sass_value_is_color (const union Sass_Value* v);
(define (list? value)
  ((foreign-lambda bool sass_value_is_list (c-pointer "Sass_Value")) value))
; bool sass_value_is_list (const union Sass_Value* v);
(define (map? value)
  ((foreign-lambda bool sass_value_is_map (c-pointer "Sass_Value")) value))
; bool sass_value_is_map (const union Sass_Value* v);
(define (error? value)
  ((foreign-lambda bool sass_value_is_error (c-pointer "Sass_Value")) value))
; bool sass_value_is_error (const union Sass_Value* v);
(define (warning? value)
  ((foreign-lambda bool sass_value_is_warning (c-pointer "Sass_Value")) value))
; bool sass_value_is_warning (const union Sass_Value* v);

;; Getters and setters for Sass_Number
(define (get_value )
  ((foreign-lambda double sass_number_get_value (c-pointer "Sass_Value")))
; double sass_number_get_value (const union Sass_Value* v);
(define (set_value )
  ((foreign-lambda void sass_number_set_value ((c-pointer "Sass_Value"), double)))
; void sass_number_set_value (union Sass_Value* v, double value);
(define (get_unit )
  ((foreign-lambda c-string sass_number_get_unit (c-pointer "Sass_Value")))
; const char* sass_number_get_unit (const union Sass_Value* v);
(define (set_unit )
  ((foreign-lambda void sass_number_set_unit ((c-pointer "Sass_Value"), c-string)))
; void sass_number_set_unit (union Sass_Value* v, char* unit);

;; Getters and setters for Sass_String
(define (get_value )
  ((foreign-lambda c-string sass_string_get_value (c-pointer "Sass_Value")))
; const char* sass_string_get_value (const union Sass_Value* v);
(define (set_value )
  ((foreign-lambda void sass_string_set_value ((c-pointer "Sass_Value"), c-string)))
; void sass_string_set_value (union Sass_Value* v, char* value);

;; Getters and setters for Sass_Boolean
(define (get_value )
  ((foreign-lambda bool sass_boolean_get_value (c-pointer "Sass_Value")))
; bool sass_boolean_get_value (const union Sass_Value* v);
(define (set_value )
  ((foreign-lambda void sass_boolean_set_value ((c-pointer "Sass_Value"), bool)))
; void sass_boolean_set_value (union Sass_Value* v, bool value);

;; Getters and setters for Sass_Color
(define (get_r )
  ((foreign-lambda double sass_color_get_r (c-pointer "Sass_Value")))
; double sass_color_get_r (const union Sass_Value* v);
(define (set_r )
  ((foreign-lambda void sass_color_set_r ((c-pointer "Sass_Value"), double)))
; void sass_color_set_r (union Sass_Value* v, double r);
(define (get_g )
  ((foreign-lambda double sass_color_get_g (c-pointer "Sass_Value")))
; double sass_color_get_g (const union Sass_Value* v);
(define (set_g )
  ((foreign-lambda void sass_color_set_g ((c-pointer "Sass_Value"), double)))
; void sass_color_set_g (union Sass_Value* v, double g);
(define (get_b )
  ((foreign-lambda double sass_color_get_b (c-pointer "Sass_Value")))
; double sass_color_get_b (const union Sass_Value* v);
(define (set_b )
  ((foreign-lambda void sass_color_set_b ((c-pointer "Sass_Value"), double)))
; void sass_color_set_b (union Sass_Value* v, double b);
(define (get_a )
  ((foreign-lambda double sass_color_get_a (c-pointer "Sass_Value")))
; double sass_color_get_a (const union Sass_Value* v);
(define (set_a )
  ((foreign-lambda void sass_color_set_a ((c-pointer "Sass_Value"), double)))
; void sass_color_set_a (union Sass_Value* v, double a);

;; Getter for the number of items in list
(define (get_length )
  ((foreign-lambda size_t sass_list_get_length (c-pointer "Sass_Value")))
; size_t sass_list_get_length (const union Sass_Value* v);
;; Getters and setters for Sass_List
(define (get_separator )
  ((foreign-lambda (enum "Sass_Separator") sass_list_get_separator (c-pointer "Sass_Value")))
; enum Sass_Separator sass_list_get_separator (const union Sass_Value* v);
(define (set_separator )
  ((foreign-lambda void sass_list_set_separator ((c-pointer "Sass_Value"), enum Sass_Separator value)))
; void sass_list_set_separator (union Sass_Value* v, enum Sass_Separator value);
;; Getters and setters for Sass_List values
(define (get_value )
  ((foreign-lambda (union "Sass_Value*") sass_list_get_value (const union Sass_Value* v, size_t)))
; union Sass_Value* sass_list_get_value (const union Sass_Value* v, size_t i);
(define (set_value )
  ((foreign-lambda void sass_list_set_value ((c-pointer "Sass_Value"), size_t, union Sass_Value* value)))
; void sass_list_set_value (union Sass_Value* v, size_t i, union Sass_Value* value);

;; Getter for the number of items in map
(define (get_length )
  ((foreign-lambda size_t sass_map_get_length (c-pointer "Sass_Value")))
; size_t sass_map_get_length (const union Sass_Value* v);
;; Getters and setters for Sass_Map keys and values
(define (get_key )
  ((foreign-lambda (union "Sass_Value*") sass_map_get_key (const union Sass_Value* v, size_t)))
; union Sass_Value* sass_map_get_key (const union Sass_Value* v, size_t i);
(define (set_key )
  ((foreign-lambda void sass_map_set_key ((c-pointer "Sass_Value"), size_t, union Sass_Value*)))
; void sass_map_set_key (union Sass_Value* v, size_t i, union Sass_Value*);
(define (get_value )
  ((foreign-lambda (union "Sass_Value*") sass_map_get_value (const union Sass_Value* v, size_t)))
; union Sass_Value* sass_map_get_value (const union Sass_Value* v, size_t i);
(define (set_value )
  ((foreign-lambda void sass_map_set_value ((c-pointer "Sass_Value"), size_t, union Sass_Value*)))
; void sass_map_set_value (union Sass_Value* v, size_t i, union Sass_Value*);

;; Getters and setters for Sass_Error
(define (get_message )
  ((foreign-lambda c-string sass_error_get_message (c-pointer "Sass_Value")))
; char* sass_error_get_message (const union Sass_Value* v);
(define (set_message )
  ((foreign-lambda void sass_error_set_message ((c-pointer "Sass_Value"), c-string)))
; void sass_error_set_message (union Sass_Value* v, char* msg);

;; Getters and setters for Sass_Warning
(define (get_message )
  ((foreign-lambda c-string sass_warning_get_message (c-pointer "Sass_Value")))
; char* sass_warning_get_message (const union Sass_Value* v);
(define (set_message )
  ((foreign-lambda void sass_warning_set_message ((c-pointer "Sass_Value"), c-string msg)))
; void sass_warning_set_message (union Sass_Value* v, char* msg);

;; Creator functions for all value types
(define (null )
  ((foreign-lambda (union "Sass_Value*") sass_make_null    (void)))
; union Sass_Value* sass_make_null    (void);
(define (boolean )
  ((foreign-lambda (union "Sass_Value*") sass_make_boolean (bool)))
; union Sass_Value* sass_make_boolean (bool val);
(define (string )
  ((foreign-lambda (union "Sass_Value*") sass_make_string  (c-string)))
; union Sass_Value* sass_make_string  (const char* val);
(define (number )
  ((foreign-lambda (union "Sass_Value*") sass_make_number  (double, c-string)))
; union Sass_Value* sass_make_number  (double val, const char* unit);
(define (color )
  ((foreign-lambda (union "Sass_Value*") sass_make_color   (double, double, double, double)))
; union Sass_Value* sass_make_color   (double r, double g, double b, double a);
(define (list )
  ((foreign-lambda (union "Sass_Value*") sass_make_list    (size_t, enum Sass_Separator sep)))
; union Sass_Value* sass_make_list    (size_t len, enum Sass_Separator sep);
(define (map )
  ((foreign-lambda (union "Sass_Value*") sass_make_map     (size_t)))
; union Sass_Value* sass_make_map     (size_t len);
(define (error )
  ((foreign-lambda (union "Sass_Value*") sass_make_error   (c-string)))
; union Sass_Value* sass_make_error   (const char* msg);
(define (warning )
  ((foreign-lambda (union "Sass_Value*") sass_make_warning (c-string)))
; union Sass_Value* sass_make_warning (const char* msg);

;; Generic destructor function for all types
;; Will release memory of all associated Sass_Values
;; Means we will delete recursively for lists and maps
(define (value )
  ((foreign-lambda void sass_delete_value (union Sass_Value* val)))
; void sass_delete_value (union Sass_Value* val);

;; Make a deep cloned copy of the given sass value
(define (value )
  ((foreign-lambda (union "Sass_Value*") sass_clone_value (const union Sass_Value* val)))
; union Sass_Value* sass_clone_value (const union Sass_Value* val);
