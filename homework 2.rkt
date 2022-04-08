;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rua-christian-hw2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Christian Rua
;; cmrua

;PROBLEMS 1 + 2 -------------------------------------------------------------------------------------------------;

(define-struct hurricane (name category winds velocity heading))
;; a Hurricane is a (make-hurricane String Natural Natural Natural String)
;; interp: represents a Hurricane where:
;;   name is the name of the hurricane
;;   category is the category (1 through 5 inclusive) of the hurricane
;;   winds is the maximum sustained winds in miles per hour
;;   velocity is the velocity of the storm in miles per hour
;;   heading is the direction the storm is heading in cardinal directions (ex: NNW)

(define BRADY  (make-hurricane "Brady"  1 80  85  "N"))
(define MIKE   (make-hurricane "Mike"   2 100 103 "WNW"))
(define ADDY   (make-hurricane "Addy"   3 120 128 "SW"))
(define CONNOR (make-hurricane "Connor" 4 150 154 "SEE"))
(define ZEV    (make-hurricane "Zev"    5 160 168 "NE"))

;; hurricane-fcn: Hurricane -> ...
;; ...
;; (define (hurricane-fcn a-hurricane)
;;   (... (hurricane-name     a-hurricane)    ;String
;;        (hurricane-category a-hurricane)    ;Natural
;;        (hurricane-winds    a-hurricane)    ;Natural
;;        (hurricane-velocity a-hurricane)    ;Natural
;;        (hurricane-heading  a-hurricane)))  ;String

(define-struct thunderstorm (rainfall gust velocity heading))
;; a Thunderstorm is a (make-thunderstorm Natural Natural Natural String)
;; interp: represents a Thunderstorm where:
;;   rainfall is the number of inches of rainfall
;;   gust is the maximum wind gust in miles per hour
;;   velocity is the velocity of the storm in miles per hour
;;   heading is the direction the storm is heading in cardinal directions (ex: NNW)

;; thunderstorm-fcn: Thunderstorm -> ...
;; ...
;; (define (thunderstorm-fcm a-thunderstorm)
;;   (... (thunderstorm-rainfall a-thunderstorm)    ;Natural
;;        (thunderstorm-gust     a-thunderstorm)    ;Natural
;;        (thunderstorm-velocity a-thunderstorm)    ;Natural
;;        (thunderstorm-heading  a-thunderstorm)))  ;String

(define SMALL-THUNDER  (make-thunderstorm 2 40 50 "S"))
(define MEDIUM-THUNDER (make-thunderstorm 4 55 60 "NEN"))
(define BIG-THUNDER    (make-thunderstorm 7 70 70 "NW"))

(define-struct fire (sq-miles days people))
;; a Fire is a (make-fire Natural Natural Natural)
;; interp: represents a Fire where:
;;   sq-miles is the number of square miles it covers
;;   days is the number of days it has been raging
;;   people is the number of people it has displaced

;; fire-fcn: Fire -> ...
;; ...
;; (define (fire-fcn a-fire)
;;   (... (fire-sq-miles a-fire)    ;Natural
;;        (fire-days     a-fire)    ;Natural
;;        (fire-people   a-fire)))  ;Natural

(define SMALL-FIRE  (make-fire 5  1  20))
(define MEDIUM-FIRE (make-fire 25 7  100))
(define BIG-FIRE    (make-fire 60 30 1500))

;; a Storm is one of
;;   Hurricane
;;   Thunderstorm
;;   Fire
;; interp: Storm represents a type of storm

;; storm-fcn: Storm -> ...
;; ...
;; (define (storm-fcn a-storm)
;;   (cond [(hurricane?    a-storm) (... (hurricane-name        a-hurricane)       ;String
;;                                       (hurricane-category    a-hurricane)       ;Natural
;;                                       (hurricane-winds       a-hurricane)       ;Natural
;;                                       (hurricane-velocity    a-hurricane)       ;Natural
;;                                       (hurricane-heading     a-hurricane))]     ;String
;;         [(thunderstorm? a-storm) (... (thunderstorm-rainfall a-thunderstorm)    ;Natural
;;                                       (thunderstorm-gust     a-thunderstorm)    ;Natural
;;                                       (thunderstorm-velocity a-thunderstorm)    ;Natural
;;                                       (thunderstorm-heading  a-thunderstorm))]  ;String
;;         [(fire?         a-storm) (... (fire-sq-miles         a-fire)            ;Natural
;;                                       (fire-days             a-fire)            ;Natural
;;                                       (fire-people           a-fire))]))        ;Natural

;PROBLEM 3 -------------------------------------------------------------------------------------------------;

;; high-impact?: Storm -> Boolean
;; consumes a storm and produces true if the storm is a category 4 or 5 hurricane, a thunderstorm with more than
;;   3in of rainfall and winds exceeding 60mph, or a fire covering at least 50 square miles

(define (high-impact? storm)
  (cond [(hurricane? storm) (or (= (hurricane-category storm) 4) (= (hurricane-category storm) 5))]
        [(thunderstorm? storm) (and (> (thunderstorm-rainfall storm) 3) (> (thunderstorm-gust storm) 60))]
        [(fire? storm) (> (fire-sq-miles storm) 50)]))

(check-expect (high-impact? MIKE) false)
(check-expect (high-impact? ZEV) true)
(check-expect (high-impact? MEDIUM-THUNDER) false)
(check-expect (high-impact? BIG-THUNDER) true)
(check-expect (high-impact? SMALL-FIRE) false)
(check-expect (high-impact? BIG-FIRE) true)

;PROBLEM 4 -------------------------------------------------------------------------------------------------;

;; change-heading: Storm Heading -> Storm
;; consumes a storm and a heading and produces a storm with the new heading if they didn't match

(define (change-heading storm heading)
  (cond [(hurricane? storm)
         (make-hurricane (hurricane-name storm)
                         (hurricane-category storm)
                         (hurricane-winds storm)
                         (hurricane-velocity storm)
                         heading)]
        [(thunderstorm? storm)
         (make-thunderstorm (thunderstorm-rainfall storm)
                            (thunderstorm-gust storm)
                            (thunderstorm-velocity storm)
                            heading)]
        [(fire? storm) storm]))

(check-expect (change-heading CONNOR "N") (make-hurricane "Connor" 4 150 154 "N"))
(check-expect (change-heading SMALL-THUNDER "WSW") (make-thunderstorm 2 40 50 "WSW"))
(check-expect (change-heading MEDIUM-FIRE "SE") MEDIUM-FIRE)

;PROBLEM 5 -------------------------------------------------------------------------------------------------;

;; a ListOfString is one of
;;   empty
;;   (cons String ListOfString)
;; interp:  ListOfString represents a list of strings

(define LETTERS (cons "E" (cons "W" (cons "C" (cons "F" (cons "Z" empty))))))
(define DINING (cons "Daka" (cons "Goat's Head" (cons "Campus Center" empty))))
(define REALLY-LONG-STRING (cons "THIS IS A REALLY REALLY REALLY REALLY REALLY REALLY REALLY LONG STRING!" empty))

;; character-count: ListOfString -> Natural
;; consumes a ListOfString and produces the total number of characters between all the strings

(define (character-count los)
  (cond [(empty? los) 0]
        [(cons? los) (+ (string-length (first los)) (character-count (rest los)))]))

(check-expect (character-count empty) 0)
(check-expect (character-count LETTERS) 5)
(check-expect (character-count DINING) 28)
(check-expect (character-count REALLY-LONG-STRING) 71)

;PROBLEM 6 -------------------------------------------------------------------------------------------------;

(define ONLY-LETTERS (cons "ABC" (cons "DEF" (cons "GHI" empty))))
(define ONLY-NUMBERS (cons "123" (cons "456" (cons "789" empty))))
(define MIXED-CHARS  (cons "ABC" (cons "123" (cons "D4F" empty))))

;; numeric-strings: ListOfString -> ListOfString
;; consumes a ListOfString and produces a ListOfString that contains only the items from the first ListOfString that contain all numbers

(define (numeric-strings los)
  (cond [(empty? los) empty]
        [(cons? los) (if (string-numeric? (first los))
                         (cons (first los) (numeric-strings (rest los)))
                         (numeric-strings (rest los)))]))

(check-expect (numeric-strings ONLY-LETTERS) empty)
(check-expect (numeric-strings ONLY-NUMBERS) ONLY-NUMBERS)
(check-expect (numeric-strings MIXED-CHARS) (cons "123" empty))
(check-expect (numeric-strings empty) empty)

;PROBLEM 7 -------------------------------------------------------------------------------------------------;

;; a ListOfNatural is one of
;;   empty
;;   (cons Natural empty)
;; interp:  ListOfNatural represents a list of natural numbers

;; lengths-of-strings: ListOfString -> ListOfNatural
;; consumes a ListOfString and produces a ListOfNatural where each item is the number of characters in that item in the first ListOfString

(define (lengths-of-strings los)
  (cond [(empty? los) empty]
        [(cons? los) (cons (string-length (first los)) (lengths-of-strings (rest los)))]))

(check-expect (lengths-of-strings ONLY-LETTERS) (cons 3 (cons 3 (cons 3 empty))))
(check-expect (lengths-of-strings DINING) (cons 4 (cons 11 (cons 13 empty))))
(check-expect (lengths-of-strings empty) empty)