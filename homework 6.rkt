;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname rua-christian-hw6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;; Chrisian Rua
;; cmrua

; PROBLEM 1 -------------------------------------------------------------------------------------------------------------------------------------;

(define-struct user (uname mailbox))
;; a User is a (make-user String Mailbox)
;; interp: represents a User where:
;;   uname is the username of the user
;;   mailbox is the user's mailbox

;; an Email-System is one of:
;;   empty
;;   (cons User empty)
;; interp: represents a list of User

(define-struct message (sender text read?))
;; a Message is a (make-message String String Boolean)
;; interp: represents a Message where:
;;   sender is the username of the sender of the message
;;   text is the body of the message
;;   read? is #true if the message has been read

;; a Mailbox is one of:
;;   empty
;;   (cons Message empty)
;; interp: represents a list of Message

; PROBLEM 2 -------------------------------------------------------------------------------------------------------------------------------------;

(define MAILSYS empty)
(define NEWUSER (make-user "Newuser" empty))

; PROBLEM 3 -------------------------------------------------------------------------------------------------------------------------------------;

;; add-user: String -> Void
;; consumes a username and adds a new user with the given username to the mail system, producing void

(define (add-user uname)
  (set! MAILSYS (cons (make-user uname empty) MAILSYS)))

; PROBLEM 4 -------------------------------------------------------------------------------------------------------------------------------------;

(define FROM-JIMMY (make-message "Jimmy" "Hey there, Ulysses." #false))
(define FROM-RONALD (make-message "Ronald" "You are late!" #true))

(define JIMMY (make-user "Jimmy" (list FROM-RONALD)))
(define RONALD (make-user "Ronald" empty))

(set! MAILSYS (append (list JIMMY RONALD) MAILSYS))

;; find-user: String -> User
;; consumes a username and produces that user's User from MAILSYS

(define (find-user uname)
  (local
    [;; isolate-user: User -> Boolean
     ;; consumes a User and produces #true if the username matches the given recipient
     
     (define (isolate-user user)
       (cond [(empty? user) #false]
             [(user? user) (string=? (user-uname user) uname)]))]
    (first (filter isolate-user MAILSYS))))

(check-expect (find-user "Ronald") RONALD)

;; send-email: String String String -> Void
;; consumes the name of the sender of an email, the name of the recipient of the email, and the text of an email message, storing an unread
;;   message in the recipient's mailbox, producing void

(define (send-email sender recipient text)
  (set-user-mailbox! (find-user recipient) (cons (make-message sender text #false) (user-mailbox (find-user recipient)))))

;(send-email "Ronald" "Jimmy" "test")

; PROBLEM 5 -------------------------------------------------------------------------------------------------------------------------------------;

(define FULL-BOX (list FROM-JIMMY FROM-RONALD))
(define EMPTY-BOX empty)

(define ULYSSES (make-user "Ulysses" FULL-BOX))

(set! MAILSYS (append (list ULYSSES) MAILSYS))

;; status-read: Message -> Message
;; consumes a message and produces the same message but it has been marked as read

(define (status-read message)
  (make-message (message-sender message)
                (message-text message)
                #true))

(check-expect (status-read FROM-JIMMY) (make-message "Jimmy" "Hey there, Ulysses." #true))

;; list-of-unreads: Mailbox Mailbox -> Mailbox
;; consumes a user's mailbox and an empty accumulator list and produces a list of all the messages in the user's mailbox that were unread and sets them to read

(define (list-of-unreads mailbox unreads)
  (cond [(empty? mailbox) unreads]
        [(cons? mailbox) (if (message-read? (first mailbox))
                             (list-of-unreads (rest mailbox) unreads)
                             (list-of-unreads (rest mailbox) (cons (status-read (first mailbox)) unreads)))]))

(check-expect (list-of-unreads FULL-BOX empty) (list (make-message "Jimmy" "Hey there, Ulysses." #true)))
(check-expect (list-of-unreads EMPTY-BOX empty) empty)

;; get-unread-messages: String -> Mailbox
;; consumes a username and produces a list of messages that contains all the unread messages and they've been set to read

(define (get-unread-messages uname)
  (list-of-unreads (user-mailbox (find-user uname)) empty))

;(get-unread-messages "Ulysses")

; PROBLEM 6 -------------------------------------------------------------------------------------------------------------------------------------;

;; biggest-user: Email-System User -> User
;; consumes an email system and a user and produces the user in the email system with the most amount of emails in their mailbox

(define (biggest-user system current-user)
  (cond [(empty? system) current-user]
        [(cons? system) (if (>= (length (user-mailbox (first system))) (length (user-mailbox current-user)))
                            (biggest-user (rest system) (first system))
                            (biggest-user (rest system) current-user))]))

(check-expect (biggest-user MAILSYS (first MAILSYS)) ULYSSES)

;; most-messages: -> User OR Error
;; consumes nothing and produces the user with the most amount of emails in their mailbox OR an error message if there are no users in the system

(define (most-messages)
  (cond [(empty? MAILSYS) (error "There are no users in the email system.")]
        [(cons? MAILSYS) (biggest-user MAILSYS (first MAILSYS))]))

;(most-messages)

; PROBLEM 7 -------------------------------------------------------------------------------------------------------------------------------------;



; PROBLEM 8 -------------------------------------------------------------------------------------------------------------------------------------;

(define ANIMALS (list "cow" "chicken" "pig"))
(define NUMBERS (list "3" "8" "14" "20"))

;; total-string-length-acc: ListOfString Natural -> Natural
;; consumes a list of strings and a natural (accumulation initializer) and produces the sum of the lengths of the strings in the list

(define (total-string-length-acc los length)
  (cond [(empty? los) length]
        [(cons? los) (total-string-length-acc (rest los) (+ (string-length (first los)) length))]))

(check-expect (total-string-length-acc ANIMALS 0) 13)
(check-expect (total-string-length-acc NUMBERS 0) 6)
(check-expect (total-string-length-acc empty 0) 0)

;; total-string-length: ListOfString -> Natural
;; consumes a list of strings and produces the sum of the lengths of the strings in the list

(define (total-string-length los)
  (total-string-length-acc los 0))

(check-expect (total-string-length ANIMALS) 13)
(check-expect (total-string-length NUMBERS) 6)
(check-expect (total-string-length empty) 0)

; PROBLEM 9 -------------------------------------------------------------------------------------------------------------------------------------;

;; squisher: ListOfString String -> String
;; consumes a ListOfString and a string (accumulation initializer) and produces the concatenation of strings in the list in the order they appear in the list

(define (squisher los squished)
  (cond [(empty? los) squished]
        [(cons? los) (squisher (rest los) (string-append squished (first los)))]))

(check-expect (squisher ANIMALS "") "cowchickenpig")
(check-expect (squisher NUMBERS "") "381420")
(check-expect (squisher empty "") "")

;; one-giant-string: ListOfString -> String
;; consumes a ListOfString and produces the concatenation of strings in the list in the order they appear in the list

(define (one-giant-string los)
  (squisher los ""))

(check-expect (one-giant-string ANIMALS) "cowchickenpig")
(check-expect (one-giant-string NUMBERS) "381420")
(check-expect (one-giant-string empty) "")