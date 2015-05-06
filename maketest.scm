(import foreign)
(use foreigners)

(foreign-declare "#include <sass_values.h>")

(define-foreign-type sass-value (union "Sass_Value"))

(define-foreign-record-type sass_number
  (int n_tag tag)
  (double n_val value set-value!)
  (c-string n_unit unit set-unit!))

(define (mk-number num unt)
  ((foreign-lambda (c-pointer sass-value) sass_make_number  double c-string) num unt))

(define (get-num-value val)
  ((foreign-lambda double sass_number_get_value (c-pointer sass-value)) val))
(define (set-num-value! valp newval)
  ((foreign-lambda void sass_number_set_value (c-pointer sass-value) double) valp newval))

(define (get-num-unit val)
  ((foreign-lambda c-string sass_number_get_unit (c-pointer sass-value)) val))
(define (set-num-unit! valp unt)
  ((foreign-lambda void sass_number_set_unit (c-pointer sass-value) c-string) valp unt))
