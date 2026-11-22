(uiop:define-package :aoc-22/day04
  (:use :cl :aoc-22/utils)
  (:import-from :cl-ppcre
                #:all-matches-as-strings)
  (:documentation "--- Day 4: Camp Cleanup ---"))

(in-package :aoc-22/day04)

(defun parse-line (line)
  (mapcar #'parse-integer (all-matches-as-strings "\\d+" line)))

(defun parse-input (lines)
  (mapcar #'parse-line lines))

(defun contained-p (lst)
  (destructuring-bind (a b c d) lst
    (or (<= c a b d)
        (<= a c d b))))

(defun overlaps-p (lst)
  (destructuring-bind (a b c d) lst
    (or (<= c a d)
        (<= c b d)
        (<= a c b)
        (<= a d b))))

;; => 530
(defun part1 (&optional (input (parse-input (read-input :format :lines))))
  (loop for l in input
        count (contained-p l)))

;; => 903
(defun part2 (&optional (input (parse-input (read-input :format :lines))))
  (loop for l in input
       count (overlaps-p l)))
