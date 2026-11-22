(uiop:define-package :aoc-22/day01
  (:use :cl :aoc-22/utils)
  (:import-from :cl-ppcre
                #:all-matches-as-strings
                #:split)
  (:documentation "--- Day 1: Calorie Counting ---"))

(in-package :aoc-22/day01)

(defun parse-input (line)
  (sort (mapcar (lambda (str)
                  (apply #'+ (mapcar #'parse-integer (all-matches-as-strings "\\d+" str))))
                (split "\\n\\n" line))
        #'>))

;; => 70369
(defun part1 (&optional (input (parse-input (read-input))))
  (first input))

;; => 203002
(defun part2 (&optional (input (parse-input (read-input))))
  (loop for c in input repeat 3 sum c))
