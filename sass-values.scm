;;; sass-values.scm -- Values module for libsass binding.
;;;
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module sass-values
        *
        (import scheme chicken)
        (import foreign)
        (use foreigners)

(foreign-declare "#include <sass_values.h>")

(include "sass-common-types.scm")

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

(define-record sass-null ptr)
(define-record sass-number ptr)
(define-record sass-string ptr)
(define-record sass-boolean ptr)
(define-record sass-color ptr)
(define-record sass-list ptr)
(define-record sass-map ptr)
(define-record sass-error ptr)
(define-record sass-warning ptr)

(define (sass-value-tag sv-ptr)
  ((foreign-lambda sass-tag sass_value_get_tag (c-pointer sass-value)) sv-ptr))

(define (svp->sass-value ptr)
  (case (sass-value-tag ptr)
    ((null) (make-sass-null ptr))
    ((number) (make-sass-number ptr))
    ((string) (make-sass-string ptr))
    ((boolean) (make-sass-boolean ptr))
    ((color) (make-sass-color ptr))
    ((list) (make-sass-list ptr))
    ((map) (make-sass-map ptr))
    ((error) (make-sass-error ptr))
    ((warning) (make-sass-warning ptr))))

(define (sass-value-ptr val)
  (cond
    ((sass-null? val) (sass-null-ptr val))
    ((sass-number? val) (sass-number-ptr val))
    ((sass-string? val) (sass-string-ptr val))
    ((sass-boolean? val) (sass-boolean-ptr val))
    ((sass-color? val) (sass-color-ptr val))
    ((sass-list? val) (sass-list-ptr val))
    ((sass-map? val) (sass-map-ptr val))
    ((sass-error? val) (sass-error-ptr val))
    ((sass-warning? val) (sass-warning-ptr val))))

;; Getters and setters for Sass_Number
(define (get-num-value num)
  ((foreign-lambda double sass_number_get_value (c-pointer sass-value))
   (sass-number-ptr num)))
; double sass_number_get_value (const union Sass_Value* v);
(define (set-num-value! num val)
  ((foreign-lambda void sass_number_set_value (c-pointer sass-value) double)
   (sass-number-ptr num) val))
; void sass_number_set_value (union Sass_Value* v, double value);
(define (get-num-unit num)
  ((foreign-lambda c-string sass_number_get_unit (c-pointer sass-value))
   (sass-number-ptr num)))
; const char* sass_number_get_unit (const union Sass_Value* v);
(define (set-num-unit! num uni)
  ((foreign-lambda void sass_number_set_unit (c-pointer sass-value) c-string)
   (sass-number-ptr num) uni))
; void sass_number_set_unit (union Sass_Value* v, char* unit);

;; Getters and setters for Sass_String
(define (get-string-value str)
  ((foreign-lambda c-string sass_string_get_value (c-pointer sass-value))
   (sass-string-ptr str)))
; const char* sass_string_get_value (const union Sass_Value* v);
(define (set-string-value! str val)
  ((foreign-lambda void sass_string_set_value (c-pointer sass-value) c-string)
   (sass-string-ptr str) val))
; void sass_string_set_value (union Sass_Value* v, char* value);

;; Getters and setters for Sass_Boolean
(define (get-bool-value bool)
  ((foreign-lambda bool sass_boolean_get_value (c-pointer sass-value))
   (sass-boolean-ptr bool)))
; bool sass_boolean_get_value (const union Sass_Value* v);
(define (set-bool-value! bool val)
  ((foreign-lambda void sass_boolean_set_value (c-pointer sass-value) bool)
   (sass-boolean-ptr bool) val))
; void sass_boolean_set_value (union Sass_Value* v, bool value);

;; Getters and setters for Sass_Color
(define (get-red color)
  ((foreign-lambda double sass_color_get_r (c-pointer sass-value))
   (sass-color-ptr color)))
; double sass_color_get_r (const union Sass_Value* v);
(define (set-red! color val)
  ((foreign-lambda void sass_color_set_r (c-pointer sass-value) double)
   (sass-color-ptr color) val))
; void sass_color_set_r (union Sass_Value* v, double r);
(define (get-green color)
  ((foreign-lambda double sass_color_get_g (c-pointer sass-value))
   (sass-color-ptr color)))
; double sass_color_get_g (const union Sass_Value* v);
(define (set-green! color val)
  ((foreign-lambda void sass_color_set_g (c-pointer sass-value) double)
   (sass-color-ptr color) val))
; void sass_color_set_g (union Sass_Value* v, double r);
(define (get-blue color)
  ((foreign-lambda double sass_color_get_b (c-pointer sass-value))
   (sass-color-ptr color)))
; double sass_color_get_b (const union Sass_Value* v);
(define (set-blue! color val)
  ((foreign-lambda void sass_color_set_b (c-pointer sass-value) double)
   (sass-color-ptr color) val))
; void sass_color_set_b (union Sass_Value* v, double r);
(define (get-alpha color)
  ((foreign-lambda double sass_color_get_a (c-pointer sass-value))
   (sass-color-ptr color)))
; double sass_color_get_a (const union Sass_Value* v);
(define (set-alpha! color val)
  ((foreign-lambda void sass_color_set_a (c-pointer sass-value) double)
   (sass-color-ptr color) val))
; void sass_color_set_a (union Sass_Value* v, double r);

;; Getter for the number of items in list
(define (get-list-length lst)
  ((foreign-lambda size_t sass_list_get_length (c-pointer sass-value))
   (sass-list-ptr lst)))
; size_t sass_list_get_length (const union Sass_Value* v);
;; Getters and setters for Sass_List
(define (get-list-separator lst)
  ((foreign-lambda (enum "Sass_Separator") sass_list_get_separator (c-pointer sass-value))
   (sass-list-ptr lst)))
; enum Sass_Separator sass_list_get_separator (const union Sass_Value* v);
(define (set-list-separator! lst sep)
  ((foreign-lambda void sass_list_set_separator (c-pointer sass-value) separator)
   (sass-list-ptr lst) sep))
; void sass_list_set_separator (union Sass_Value* v, enum Sass_Separator value);
;; Getters and setters for Sass_List values
(define (get-list-value lst idx)
  (let ((raw ((foreign-lambda (c-pointer sass-value)
                              sass_list_get_value
                              (c-pointer sass-value)
                              size_t)
              (sass-list-ptr lst) idx)))
    (svp->sass-value raw)))
; union Sass_Value* sass_list_get_value (const union Sass_Value* v, size_t i);
(define (set-list-value! lst idx val)
  ((foreign-lambda void sass_list_set_value (c-pointer sass-value) size_t (c-pointer sass-value))
   (sass-list-ptr lst) idx (sass-value-ptr val)))
; void sass_list_set_value (union Sass_Value* v, size_t i, union Sass_Value* value);

;; Getter for the number of items in map
(define (get-map-length mapp)
  ((foreign-lambda size_t sass_map_get_length (c-pointer sass-value))
   (sass-map-ptr mapp)))
; size_t sass_map_get_length (const union Sass_Value* v);
;; Getters and setters for Sass_Map keys and values
(define (get-map-key mapp idx)
  (let ((raw ((foreign-lambda (c-pointer sass-value)
                              sass_map_get_key
                              (c-pointer sass-value)
                              size_t)
              (sass-map-ptr mapp) idx)))
    (svp->sass-value raw)))
; union Sass_Value* sass_map_get_key (c-pointer sass-value) v, size_t i);
(define (set-map-key! mapp idx val)
  ((foreign-lambda void sass_map_set_key (c-pointer sass-value) size_t (c-pointer sass-value))
   (sass-map-ptr mapp) idx (sass-value-ptr val)))
; void sass_map_set_key (union Sass_Value* v, size_t i, union Sass_Value*);
(define (get-map-value mapp idx)
  (let ((raw ((foreign-lambda (c-pointer sass-value)
                              sass_map_get_value
                              (c-pointer sass-value)
                              size_t)
              (sass-map-ptr mapp) idx)))
    (svp->sass-value raw)))
; union Sass_Value* sass_map_get_value (c-pointer sass-value) v, size_t i);
(define (set-map-value! mapp idx val)
  ((foreign-lambda void sass_map_set_value (c-pointer sass-value) size_t (c-pointer sass-value))
   (sass-map-ptr mapp) idx (sass-value-ptr val)))
; void sass_map_set_value (union Sass_Value* v, size_t i, union Sass_Value*);

;; Getters and setters for Sass_Error
(define (get-error-message err)
  ((foreign-lambda c-string sass_error_get_message (c-pointer sass-value))
   (sass-error-ptr err)))
; char* sass_error_get_message (const union Sass_Value* v);
(define (set-error-message! err msg)
  ((foreign-lambda void sass_error_set_message (c-pointer sass-value) c-string)
   (sass-error-ptr err) msg))
; void sass_error_set_message (union Sass_Value* v, char* msg);

;; Getters and setters for Sass_Warning
(define (get-warning-message warg)
  ((foreign-lambda c-string sass_warning_get_message (c-pointer sass-value))
   (sass-warning-ptr warg)))
; char* sass_warning_get_message (const union Sass_Value* v);
(define (set-warning-message! warg msg)
  ((foreign-lambda void sass_warning_set_message (c-pointer sass-value) c-string)
   (sass-warning-ptr warg) msg))
; void sass_warning_set_message (union Sass_Value* v, char* msg);

;; Creator functions for all value types
(define (make-null)
  (let ((ptr ((foreign-lambda (c-pointer sass-value)
                              sass_make_null))))
    (make-sass-null ptr)))
; union Sass_Value* sass_make_null    (void);
(define (make-boolean bool)
  (let ((ptr ((foreign-lambda (c-pointer sass-value)
                              sass_make_boolean
                              bool)
              bool)))
    (make-sass-boolean ptr)))
; union Sass_Value* sass_make_boolean (bool val);
(define (make-sstring str)
  (let ((ptr ((foreign-lambda (c-pointer sass-value)
                              sass_make_string
                              c-string)
              str)))
    (make-sass-string ptr)))
; union Sass_Value* sass_make_string  (const char* val);
(define (make-number num un)
  (let ((ptr ((foreign-lambda (c-pointer sass-value)
                              sass_make_number
                              double c-string)
              num un)))
    (make-sass-number ptr)))
; union Sass_Value* sass_make_number  (double val, const char* unit);
(define (make-color r g b a)
  (let ((ptr ((foreign-lambda (c-pointer sass-value)
                              sass_make_color
                              double double double double)
              r g b a)))
    (make-sass-color ptr)))
; union Sass_Value* sass_make_color   (double r, double g, double b, double a);
(define (make-list len sep)
  (let ((ptr ((foreign-lambda (c-pointer sass-value)
                              sass_make_list
                              size_t separator)
              len sep)))
    (make-sass-list ptr)))
; union Sass_Value* sass_make_list    (size_t len, enum Sass_Separator sep);
(define (make-map len)
  (let ((ptr ((foreign-lambda (c-pointer sass-value)
                              sass_make_map
                              size_t)
              len)))
    (make-sass-map ptr)))
; union Sass_Value* sass_make_map     (size_t len);
(define (make-error msg)
  (let ((ptr ((foreign-lambda (c-pointer sass-value)
                              sass_make_error
                              c-string)
              msg)))
    (make-sass-error ptr)))
; union Sass_Value* sass_make_error   (const char* msg);
(define (make-warning msg)
  (let ((ptr ((foreign-lambda (c-pointer sass-value)
                              sass_make_warning
                              c-string)
              msg)))
    (make-sass-warning ptr)))
; union Sass_Value* sass_make_warning (const char* msg);

;; Generic destructor function for all types
;; Will release memory of all associated Sass_Values
;; Means we will delete recursively for lists and maps
(define (delete val)
  ((foreign-lambda void sass_delete_value (c-pointer sass-value))
   (sass-value-ptr val)))
; void sass_delete_value (union Sass_Value* val);

;; Make a deep cloned copy of the given sass value
(define (clone val)
  ((foreign-lambda (c-pointer sass-value) sass_clone_value (c-pointer sass-value))
   (sass-value-ptr val)))
; union Sass_Value* sass_clone_value (const union Sass_Value* val);

) ; END MODULE
