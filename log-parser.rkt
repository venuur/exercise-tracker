#lang racket

(define test-exercise-log
  "2019-03-26   \nStationary bike\n6.6km / 20 mins\n\n\n2019-03-27\npush ups\n5/6/4/4/2\n")

(define exercise-bike (string->symbol "bike"))
(define exercise-pushup (string->symbol "pushup"))
(define exercise-squat (string->symbol "squat"))
(define exercise-knee-pushup (string->symbol "knee-pushup"))

(struct compound-execise (first second))
(struct log-entry (date exercise quantity))
(struct reps (numbers))
(struct pace (kilometers minutes))

(define (parse-log-entry date-str exercise-str quantity-str)
  (let* ([log-date (string-trim date-str)]
         [exercise (parse-exercise exercise-str)]
         [quantity (parse-quantity exercise quantity-str)])
    (log-entry log-date exercise quantity)))

(define (parse-exercise exercise-str)
  (let ([normalized-str (string-downcase (string-trim exercise-str))])
    (cond
      [(regexp-match? #rx"bike" normalized-str) exercise-bike]
      [(regexp-match? #px"knee\\s*push[-\\s]*up" normalized-str) exercise-knee-pushup]
      [(regexp-match? #px"push[-\\s]*up" normalized-str) exercise-pushup]
      [(regexp-match? #rx"squat" normalized-str) exercise-squat])))

(define (parse-quantity exercise quantity-str)
  (cond
    [(eq? exercise exercise-bike) (parse-pace quantity-str)]
    [else (parse-reps quantity-str)]))

(define (parse-reps reps-str)
  (let ([rep-list (string-split reps-str "/")])
    (reps (map string->number (map string-trim rep-list)))))

(define (parse-pace pace-str)
  (let* ([distance-time (string-split pace-str "/")]
         [distance (first (regexp-match #px"[.\\d]*" (string-trim (first distance-time))))]
         [time (first (regexp-match #px"[.\\d]*" (string-trim (second distance-time))))])
    (pace (string->number distance) (string->number time))))
