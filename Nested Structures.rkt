;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Nested Structures|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; CS 305 2014.09.26
;; Homework 4 Nested Structures

;; 1.
;; a)
(define-struct person (name age address phone))
(define-struct address (country state city zipcode street-address))
(define-struct phone (area-code seven-digit))

;; b)
(define phone1 (make-phone 401 3333333)) ;; 401-333-3333
(define phone2 (make-phone 333 4567890)) ;; 333-456-7890
(define address1 (make-address "United States"
                               "Massachusetts"
                               "Boston"
                               02999
                               "1600 Test Street Apt 99"))
(define address2 (make-address "Canada"
                               "Quebec"
                               "Quebec City"
                               99999
                               "10 Moose Syrup Avenue"))
(define person1 (make-person "Canadian Joe"
                             25
                             address2
                             phone2))
(define person2 (make-person "John Q. American"
                             50
                             address1
                             phone1))

;; c)
;; mass-senior?: person -> boolean
;; consumes a person, 
;; and produces true if the person is a senior citizen (age >= 65) 
;; living in Massachusetts, or false otherwise.

(define (mass-senior? person)
  (cond  
    [(and
     (string=? 
      (address-state (person-address person))
      "Massachusetts")
     (< 65 (person-age person))) true]
    [else false]))

;; change-area-code: person number -> person
;; consumes a person and a number which may be 
;; a new area-code for the person’s phone
(define (change-area-code person number)
  (cond
    [(= number (phone-area-code (person-phone person)))
     person]
    [else (make-person
           (person-name person)
           (person-age person)
           (person-address person)
           (make-phone number (phone-seven-digit
                               (person-phone person))))]))

;; 2.
;; a)
(define-struct clock (alarm time))
(define-struct time (hours minutes))
(define-struct alarm (time))

;; b)
(define time1 (make-time 12 0)) ;; 12:00
(define time2 (make-time 20000 333)) ;; 20000:333 #QATESTERPROBZ
(define time3 (make-time 1 30)) ;; 1:30
(define time4 (make-time 8 45)) ;; 8:45
(define time5 (make-time 2 0)) ;; 2:00
(define time6 (make-time 2 0)) ;; 2:00
(define alarm1 (make-alarm time1))
(define alarm2 (make-alarm time2))
(define alarm3 (make-alarm time5))
(define clock1 (make-clock alarm1 time3))
(define clock2 (make-clock alarm2 time4))
(define clock3 (make-clock alarm3 time6))


;; c)
(define (ring-alarm? clock)
  (cond
    [(and 
      (= (time-hours (alarm-time (clock-alarm clock)))
        (time-hours (clock-time clock)))
      (= (time-minutes (alarm-time (clock-alarm clock)))
         (time-minutes (clock-time clock)))) true]
    [else false]))

(check-expect (ring-alarm? clock3) true)

;; 3.
(define (tick-helper time)
  (cond
    [(= (time-minutes time) 59)
     (cond
       [(= (time-hours time) 23)
        (make-time 0 0)]
       [else (make-time (+ 1 (time-hours time)) 0)])]
    [else (make-time (time-hours time) (+ (time-minutes time) 1))]))

(define (tick clock)
  (make-clock (clock-alarm clock) (tick-helper (clock-time clock))))

(define time7 (make-time 23 59))
(define time8 (make-time 0 0))
(define time9 (make-time 1 59))
(define time10 (make-time 2 0))
(define clock4 (make-clock alarm1 time7))
(define clock5 (make-clock alarm1 time9))

(check-expect (tick clock1) (make-clock alarm1 (make-time 1 31)))
(check-expect (tick clock4) (make-clock alarm1 (make-time 0 0)))
(check-expect (tick clock5) (make-clock alarm1 time10))