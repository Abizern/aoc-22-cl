(uiop:define-package :aoc-22/day03
  (:use :cl :aoc-22/utils)
  (:documentation "--- Day 3: Rucksack Reorganization ---"))

(in-package :aoc-22/day03)

(defun priority (char)
  (let ((code (char-code char)))
    (cond
      ((<= 97 code 122) (- code 96))
      ((<= 65 code 90) (- code 38)))))

(defun common-in-pack (string)
  (let* ((len (length string))
         (mid (/ len 2)))
    (first (intersection (coerce (subseq string 0 mid) 'list)
                         (coerce (subseq string mid) 'list)))))

(defun common-in-group (s1 s2 s3)
  (first (intersection (coerce s3 'list)
                       (intersection (coerce s1 'list)
                                     (coerce s2 'list)))))

;; => 7889
(defun part1 (&optional (input (read-input :format :lines)))
  (loop for pack in input
        sum (priority (common-in-pack pack))))

;; => 2825
(defun part2 (&optional (input (read-input :format :lines)))
  (loop for (a b c) on input by #'cdddr
        sum (priority (common-in-group a b c))))
