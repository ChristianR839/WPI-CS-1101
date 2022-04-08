;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rua-christian-hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Christian Rua
;; cmrua

; PROBLEMS 1 + 2 -------------------------------------------------------------------------------------------------;

(define-struct menu-item (name kind vegetarian? quantity cost))
;; a Menu-item is a (make-menu-item String String Boolean Natural Number)
;; interp: represents a Menu-item where:
;;   name is the name of the menu item
;;   kind is the type of menu item (one of "appetizer", "entree", "dessert", or "beverage")
;;   vegetarian? is true when the menu item is a vegetarian option
;;   quantity is the number of items ordered
;;   cost is the price of a single item

(define CHEESEBURGER      (make-menu-item "Cheeseburger"      "entree"    #false 1 10.00))
(define CHICKEN-FINGERS   (make-menu-item "Chicken Fingers"   "entree"    #false 1 8.00))
(define ICE-CREAM         (make-menu-item "Ice Cream"         "dessert"   #true  4 3.50))
(define COOKIE            (make-menu-item "Cookie"            "dessert"   #true  3 1.50))
(define LEMONADE          (make-menu-item "Lemonade"          "beverage"  #true  2 2.25))
(define SODA              (make-menu-item "Soda"              "beverage"  #true  1 2.75))
(define NACHOS            (make-menu-item "Nachos"            "appetizer" #true  1 8.75))
(define MOZZARELLA-STICKS (make-menu-item "Mozzarella Sticks" "appetizer" #true  2 5.00))

;; menu-item-fcn: Menu-item -> ...
;; ...
;; (define (menu-item-fcn a-menu-item)
;;   (... (menu-item-name        a-menu-item)    ;String
;;        (menu-item-kind        a-menu-item)    ;String
;;        (menu-item-vegetarian? a-menu-item)    ;Boolean
;;        (menu-item-quantity    a-menu-item)    ;Natural
;;        (menu-item-cost        a-menu-item)))  ;Natural

; PROBLEMS 3 + 4 -------------------------------------------------------------------------------------------------;

;; an Order is one of
;;   empty
;;   (cons Menu-item empty)
;; interp: Order represents a list of menu items

(define ORDER-1 (cons CHEESEBURGER (cons LEMONADE (cons COOKIE empty))))
(define ORDER-2 (cons CHICKEN-FINGERS (cons SODA (cons NACHOS (cons ICE-CREAM empty)))))
(define ORDER-3 (cons NACHOS (cons MOZZARELLA-STICKS (cons ICE-CREAM empty))))
(define ORDER-4 (cons LEMONADE (cons SODA empty)))
(define ORDER-5 (cons CHEESEBURGER (cons CHICKEN-FINGERS (cons ICE-CREAM (cons LEMONADE (cons SODA empty))))))

;; order-fcn: Order -> ...
;; ...
;; (define (order-fcn an-order)
;;   (cond [(empty? an-order) (...)]
;;         [(cons? an-order)  (... (order-fcn (first an-order))
;;                                 (orderfcn (rest an-order)))]))

; PROBLEM 5 -------------------------------------------------------------------------------------------------;

;; appetizer?: Menu-item -> Boolean
;; consumes a Menu-item and produces true if that Menu-item is an appetizer

(define (appetizer? item)
  (string=? "appetizer" (menu-item-kind item)))

(check-expect (appetizer? ICE-CREAM) #false)
(check-expect (appetizer? NACHOS) #true)

;; count-appetizers: Order -> Natural
;; consumes an Order and produces the number of appetizers that are in the Order

(define (count-appetizers order)
  (cond [(empty? order) 0]
        [(cons? order) (if (appetizer? (first order))
                           (+ 1 (count-appetizers (rest order)))
                           (+ 0 (count-appetizers (rest order))))]))

(check-expect (count-appetizers empty) 0)
(check-expect (count-appetizers ORDER-1) 0)
(check-expect (count-appetizers ORDER-2) 1)
(check-expect (count-appetizers ORDER-3) 2)

; PROBLEM 6 -------------------------------------------------------------------------------------------------;

;; vegetarian?: Menu-item -> Boolean
;; consumes a Menu-item and produces true if that Menu-item is vegetarian

(define (vegetarian? item)
  (menu-item-vegetarian? item))

(check-expect (vegetarian? LEMONADE) #true)
(check-expect (vegetarian? CHEESEBURGER) #false)

;; list-expensive-vegetarian: Order Number -> Order
;; consumes an Order and a Number and produces an Order containing only the items from the given Order that cost more than the given Number

(define (list-expensive-vegetarian order price)
  (cond [(empty? order) empty]
        [(cons? order) (if (and (vegetarian? (first order)) (> (menu-item-cost (first order)) price))
                           (cons (first order) (list-expensive-vegetarian (rest order) price))
                           (list-expensive-vegetarian (rest order) price))]))

(check-expect (list-expensive-vegetarian empty 0.00) empty)
(check-expect (list-expensive-vegetarian ORDER-2 5.00) (cons NACHOS empty))
(check-expect (list-expensive-vegetarian ORDER-5 2.25) (cons ICE-CREAM (cons SODA empty)))

; PROBLEM 7 -------------------------------------------------------------------------------------------------;

;; item-cost: Menu-item -> Number
;; consumes a Menu-item and produces its total cost, taking quantity into account

(define (item-cost item)
  (* (menu-item-quantity item) (menu-item-cost item)))

(check-expect (item-cost COOKIE) 4.50)
(check-expect (item-cost CHICKEN-FINGERS) 8.00)

;; order-total: Order -> Number
;; consumes an Order and produces the total cost of the order, taking quantity into account

(define (order-total order)
  (cond [(empty? order) 0]
        [(cons? order) (+ (item-cost (first order)) (order-total (rest order)))]))

(check-expect (order-total empty) 0.00)
(check-expect (order-total ORDER-1) 19.00)
(check-expect (order-total ORDER-3) 32.75)

; PROBLEM 8 -------------------------------------------------------------------------------------------------;

;; beverage?: Menu-item -> Boolean
;; consumes a Menu-item and produces true if that Menu-item is a beverage

(define (beverage? item)
  (string=? "beverage" (menu-item-kind item)))

(check-expect (beverage? LEMONADE) #true)
(check-expect (beverage? CHEESEBURGER) #false)

;; beverage-total: Order -> Number
;; consumes an Order and produces the total cost of all the beverages in the order, taking quantity into account

(define (beverage-total order)
  (cond [(empty? order) 0]
        [(cons? order) (+ (if (beverage? (first order))
                              (item-cost (first order))
                              0)
                          (beverage-total (rest order)))]))

(check-expect (beverage-total empty) 0.00)
(check-expect (beverage-total ORDER-4) 7.25)
(check-expect (beverage-total ORDER-5) 7.25)

; PROBLEM 9 -------------------------------------------------------------------------------------------------;

;; remove-beverages: Order -> Order
;; consumes an Order and produces the Order without beverages

(define (remove-beverages order)
  (cond [(empty? order) empty]
        [(cons? order) (if (not (string=? (menu-item-kind (first order)) "beverage"))
                           (cons (first order) (remove-beverages (rest order)))
                           (remove-beverages (rest order)))]))

(check-expect (remove-beverages empty) empty)
(check-expect (remove-beverages ORDER-1) (cons CHEESEBURGER (cons COOKIE empty)))
(check-expect (remove-beverages ORDER-3) ORDER-3)
(check-expect (remove-beverages ORDER-4) empty)

;; cost-with-tip: Order Number -> Number
;; consumes an Order and a Number and produces the total cost with the given Number representing the percent tip which is only applied to non-beverages

(define (cost-with-tip order percent)
  (cond [(empty? order) 0]
        [(cons? order) (+ (* (/ percent 100) (order-total (remove-beverages order))) (order-total (remove-beverages order)) (beverage-total order))]))
                              
(check-expect (cost-with-tip empty 15) 0)
(check-expect (cost-with-tip ORDER-2 15) 38.1125)
(check-expect (cost-with-tip ORDER-5 20) 45.65)