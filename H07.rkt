;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname H07) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
( define-struct traits ( op init fct pred ) )   ;  siehe Kapitel 04b Abschnitt "Structs"
    
;; Type: traits -> ((list of ANY) -> number)
;; Returns: function applying reduction op to init (of traits) 
;;   and the filtered and mapped (pred, fct) people 
( define ( create-function-with-filter-map-and-fold tr )
   ( lambda ( lst )
      ( foldl
        ( traits-op tr )
        ( traits-init tr )
        ( map
          ( traits-fct tr )
          ( filter
            ( traits-pred tr )
            lst ) ) ) ) )

( define-struct person ( last-name first-name street house-number postal-code ) )

( define person-list (list
                      (make-person "mustermann" "max" "musterweg" 1 54321)
                      (make-person "musterfrau" "meike" "musterweg" 2 12345)
                      (make-person "Def" "A" "B" 6 4)
                      (make-person "Abc" "A" "B" 7 5)
                      ))

;; Type: string -> ((list of person) -> number)
;; Returns: function taking in people, filtering out only 
;;   those with last-name of name, mapping them to their postal 
;;   code and adds them all (and one for each addition) to 357
( define ( create-strange-function name )
   ( create-function-with-filter-map-and-fold
     ( make-traits
       ( lambda ( x y ) ( + x y 1 ) )
       357
       ( lambda ( pers ) ( person-postal-code pers ) )
       ( lambda ( pers ) ( eq? ( person-last-name pers ) name ) ) ) ) )

( ( create-strange-function "Abc" ) person-list )   ;  Wie person-list zustande kommt, ist hier ausgelassen.

( define my-strange-function ( create-strange-function "Def" ) )

( my-strange-function person-list )

( define-struct traits2 ( op init fct pred combine ) ) 

( define ( create-function-with-adjacent tr )
       ( lambda ( lst )
                ( foldl
                   ( traits2-op tr )
                   ( traits2-init tr )
                   (over-adjacent 
                       ( traits2-combine tr ) 
                       ( map
                          ( traits2-fct tr )
                          ( filter
                             ( traits2-pred tr )
                              lst ) ) ) ) ) )
                          
;; Type: (X X -> Y) (list of X) -> (list of Y)
;; Returns: a list as such that element i of the resulting list is 
;;   f applied to the i-th and (i+1)-th element
( define ( over-adjacent f lst )
   ( cond [(empty? lst ) empty]
          [( empty? ( rest lst ) ) empty]
          [else ( cons ( f ( first lst ) ( second lst ) ) ( over-adjacent f ( rest lst )) )] ) )
;; Type: (X X -> Y) (list of X) -> (list of Y)
;; Returns: a list as such that element i of the resulting list is 
;;   f applied to the i-th and (i+1)-th element
( define ( over-adjacent2 f lst )
   ( cond [( empty? lst ) empty]
          ; map used with a function of arity two and two input lists
          [else ( map f ( reverse ( rest ( reverse lst ) ) ) ( rest lst ) )] ) )

; An example usage
((create-function-with-adjacent ( make-traits2
       ( lambda ( x y ) ( + x y 1 ) )
       0
       ( lambda ( x ) ( * x x ) )
       ( lambda ( x ) ( > x 0 ) )
       ( lambda ( x y ) ( * x y ) )
       ) ) (list -2 -1 0 1 2 3 4 5))