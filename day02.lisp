(uiop:define-package :aoc-22/day02
  (:use :cl :aoc-22/utils)
  (:import-from :alexandria
                #:make-keyword)
  (:documentation "--- Day 2: Rock Paper Scissors ---"))

(in-package :aoc-22/day02)

(defun parse-line (line)
  (cons (make-keyword (subseq line 0 1))
        (make-keyword (subseq line 2 3))))

(defun parse-input (lines)
  (mapcar #'parse-line lines))

(defun score-round (round)
  (cond
    ((equal (cons :A :X) round) (+ 1 3))
    ((equal (cons :A :Y) round) (+ 2 6))
    ((equal (cons :A :Z) round) (+ 3 0))
    ((equal (cons :B :X) round) (+ 1 0))
    ((equal (cons :B :Y) round) (+ 2 3))
    ((equal (cons :B :Z) round) (+ 3 6))
    ((equal (cons :C :X) round) (+ 1 6))
    ((equal (cons :C :Y) round) (+ 2 0))
    ((equal (cons :C :Z) round) (+ 3 3))))

(defun score-result (round)
  (cond
    ((equal (cons :A :X) round) (+ 0 3))
    ((equal (cons :A :Y) round) (+ 3 1))
    ((equal (cons :A :Z) round) (+ 6 2))
    ((equal (cons :B :X) round) (+ 0 1))
    ((equal (cons :B :Y) round) (+ 3 2))
    ((equal (cons :B :Z) round) (+ 6 3))
    ((equal (cons :C :X) round) (+ 0 2))
    ((equal (cons :C :Y) round) (+ 3 3))
    ((equal (cons :C :Z) round) (+ 6 1))))

;; => 11841
(defun part1 (&optional (input (parse-input (read-input :format :lines))))
  (reduce #'+ (mapcar #'score-round input)))

;; => 13022
(defun part2 (&optional (input (parse-input (read-input :format :lines))))
  (reduce #'+ (mapcar #'score-result input)))
