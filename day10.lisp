(uiop:define-package :aoc-22/day10
  (:use :cl :aoc-22/utils)
  (:documentation "--- Day 10: Cathode-Ray Tube ---"))

(in-package :aoc-22/day10)

(defun parse-line (line)
  (unless (zerop (length line))
    (if (string= (subseq line 0 4) "noop")
        (cons :noop nil)
        (cons :addx (parse-integer (subseq line 4))))))

(defun parse-input (lines)
  (mapcar #'parse-line lines))

(defun generate-values (instructions)
  (let ((x 1)
        (values '()))
    (dolist (ins instructions (nreverse values))
      (destructuring-bind (op . val) ins
        (ecase op
          (:noop
           (push x values))
          (:addx
           (push x values)
           (push x values)
           (incf x val)))))))

;; => 14060
(defun part1 (&optional (instructions (parse-input (read-input :format :lines))))
  (let ((vals (generate-values instructions)))
    (loop for n from 19 to 219 by 40
          for v = (nth n vals)
          sum (* (1+ n) v))))

;; => PAPKFKEJ
(defun part2 (&optional (instructions (parse-input (read-input :format :lines))))
  (let ((screen (make-screen 6 40))
        (vals (generate-values instructions)))
    (loop for cycle from 0
          for v in vals
          do (multiple-value-bind (row col) (floor cycle 40)
               (when (<= (abs (- col v)) 1)
                 (setf (pixel screen row col) 1))))
    screen))


(defparameter *ex1* (parse-input '("noop"
                                   "addx 3"
                                   "addx -5")))
