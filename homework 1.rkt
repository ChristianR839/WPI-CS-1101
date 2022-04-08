;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rua-c-hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Christian Rua
;; cmrua

;PROBLEMS 1 + 2 -------------------------------------------------------------------------------------------------;

(define-struct date (year month day))
;; a Date is a (make-date Natural Natural Natural)
;; interp: represents a Date where:
;;   year is the year
;;   month is the month within the year (must be a Natural between 1 and 12)
;;   day is the day within the month (must be a Natural between 1 and 31)

;;make-date: Natural Natural Natural -> Date
;;consume a date's year, month, and day and produce a date

;;date-year: Date -> Natural
;;consume a date and produce its year

;;date-month: Date -> Natural
;;consume a date and produce its month

;;date-day: Date -> Natural
;;consume a date and produce its day

;;date?: any type -> Boolean
;;consume anything and produce true only when it's a date

(define TODAY (make-date 2021 9 1))
(define BIRTHDAY (make-date 2003 7 22))
(define MOON-LANDING (make-date 1969 7 20))
(define MONTH-BEFORE-INCEPTION (make-date 2010 6 13))

(define STAR-WARS-RELEASE (make-date 1977 5 25))
(define ENDGAME-RELEASE (make-date 2019 4 26))
(define INCEPTION-RELEASE (make-date 2010 7 13))
(define JOKER-RELEASE (make-date 2019 10 4))
(define G-RATED-FILM-RELEASE (make-date 2005 10 21))

(define-struct film (title genre rating running-time opening-date receipts-collected))
;; a Film is a (make-film String String String Natural Date Natural)
;; interp: represents a film where:
;;   title is the title of the film
;;   genre is the genre of the film
;;   rating is the rating of the film
;;   running-time is the length of the film (in minutes)
;;   opening date is the date the film released
;;   receipts-collected is the total box office receipts collected (in millions of dollars)

;;make-film: String String String Natural Date Natural -> Film
;;consume a film's title, genre, rating, running time, opening date, and receipts collected and produce a film

;;film-title: Film -> String
;;consume a film and produce its title

;;film-genre: Film -> String
;;consume a film and produce its genre

;;film-rating: Film -> String
;;consume a film and produce its rating

;;film-running-time: Film -> Natural
;;consume a film and produce its length (in minutes)

;;film-opening-date: Film -> Date
;;consume a film and produce its opening date

;;film-receipts-collected: Film -> Natural
;;consume a film and produce its total box office receipts collected (in millions of dollars)

;;film?: any type -> Boolean
;;consume anything and produce true only when it's a film

(define STAR-WARS (make-film "Star Wars: A New Hope" "Sci-Fi" "PG" 105 STAR-WARS-RELEASE 776))
(define ENDGAME (make-film "Avengers: Endgame" "Action" "PG-13" 182 ENDGAME-RELEASE 2798))
(define INCEPTION (make-film "Inception" "Sci-Fi" "PG-13" 148 INCEPTION-RELEASE 160))
(define JOKER (make-film "Joker" "Drama" "R" 122 JOKER-RELEASE 1074))
(define G-RATED-FILM (make-film "For Kids Only" "Comedy" "G" 114 G-RATED-FILM-RELEASE 10))

;PROBLEM 3 -------------------------------------------------------------------------------------------------;

;;suitable-for-children?: Film -> Boolean
;;consumes a Film and produces true if the rating is G, PG, or PG-13, and produces false otherwise

(define (suitable-for-children? film)
  (cond [(string=? (film-rating film) "G") true]
        [(string=? (film-rating film) "PG") true]
        [(string=? (film-rating film) "PG-13") true]
        [else false]))

(check-expect (suitable-for-children? STAR-WARS) true)
(check-expect (suitable-for-children? ENDGAME) true)
(check-expect (suitable-for-children? JOKER) false)
(check-expect (suitable-for-children? G-RATED-FILM) true)

;PROBLEM 4 -------------------------------------------------------------------------------------------------;

;;difference-in-receipts: Film Film -> Natural
;;consumes the total box office receipts collected by two Films and produces the difference (in millions of dollars) between them

(define (difference-in-receipts film-1 film-2)
  (if (> (film-receipts-collected film-1) (film-receipts-collected film-2))
      (- (film-receipts-collected film-1) (film-receipts-collected film-2))
      (- (film-receipts-collected film-2) (film-receipts-collected film-1))))

(check-expect (difference-in-receipts ENDGAME JOKER) 1724)
(check-expect (difference-in-receipts STAR-WARS INCEPTION) 616)
(check-expect (difference-in-receipts INCEPTION ENDGAME) 2638)

;PROBLEM 5 -------------------------------------------------------------------------------------------------;

;;modify-rating: Film String ->Film
;;consumes a Film and a String representing a rating and makes a new Film the same as the original except that its rating has been replaced by the given rating

(define (modify-rating film rating)
  (make-film (film-title film)
             (film-genre film)
             rating
             (film-running-time film)
             (film-opening-date film)
             (film-receipts-collected film)))

(check-expect (modify-rating ENDGAME "R") (make-film (film-title ENDGAME)
                                                     (film-genre ENDGAME)
                                                     "R"
                                                     (film-running-time ENDGAME)
                                                     (film-opening-date ENDGAME)
                                                     (film-receipts-collected ENDGAME)))
(check-expect (modify-rating JOKER "G") (make-film (film-title JOKER)
                                                   (film-genre JOKER)
                                                   "G"
                                                   (film-running-time JOKER)
                                                   (film-opening-date JOKER)
                                                   (film-receipts-collected JOKER)))
(check-expect (modify-rating STAR-WARS "PG-13") (make-film (film-title STAR-WARS)
                                                           (film-genre STAR-WARS)
                                                           "PG-13"
                                                           (film-running-time STAR-WARS)
                                                           (film-opening-date STAR-WARS)
                                                           (film-receipts-collected STAR-WARS)))

;PROBLEM 6 -------------------------------------------------------------------------------------------------;

;;film-release: Film -> Date
;;consumes a Film and produces the Date it was released

(define (film-release film)
  (film-opening-date film))

(check-expect (film-release ENDGAME) ENDGAME-RELEASE)
(check-expect (film-release STAR-WARS) STAR-WARS-RELEASE)
(check-expect (film-release JOKER) JOKER-RELEASE)

;;opens-before?: Film Date -> Boolean
;;consumes a Film and a Date and produces true iff the Film released before the Date

(define (opens-before? film date)
  (if (= (date-year (film-release film)) (date-year date))
      (if (= (date-month (film-release film)) (date-month date))
          (< (date-day (film-release film)) (date-day date))
          (< (date-month (film-release film)) (date-month date)))
      (< (date-year (film-release film)) (date-year date))))

(check-expect (opens-before? STAR-WARS BIRTHDAY) true)
(check-expect (opens-before? ENDGAME MOON-LANDING) false)
(check-expect (opens-before? JOKER TODAY) true)
(check-expect (opens-before? INCEPTION INCEPTION-RELEASE) false)
(check-expect (opens-before? INCEPTION MONTH-BEFORE-INCEPTION) false)