(uiop:define-package :aoc-22/day09
  (:use :cl :aoc-22/utils)
  (:documentation "--- Day 9: Rope Bridge ---"))

(in-package :aoc-22/day09)

(defun parse-line (line)
  (cons (let ((c (char line 0)))
          (cond ((char= c #\U) :N)
                ((char= c #\R) :E)
                ((char= c #\D) :S)
                ((char= c #\L) :W)))
        (parse-integer (subseq line 1))))

(defun parse-input (lines)
  (mapcar #'parse-line lines))

(defun separated-p (p1 p2)
  (unless (= p1 p2)
    (if (or (= (realpart p1) (realpart p2))
            (= (imagpart p1) (imagpart p2)))
        (> (manhattan-2d p1 p2) 1)
        (> (manhattan-2d p1 p2) 2))))

(defun move-head (h dir)
  (move-point-2d h dir))

(defun follow-target (source target)
  (if (separated-p source target)
      (let ((offset (complex (compare (realpart target) (realpart source))
                             (compare (imagpart target) (imagpart source)))))
        (+ source offset))
      source))

(defun move-rope (rope dir)
  (let ((new-head (move-head (first rope) dir)))
    (loop for prev = new-head then next-knot
          for knot in (cdr rope)
          for next-knot = (follow-target knot prev)
          collect next-knot into tail
          finally (return (cons new-head tail)))))

(defun simulate (moves rope-size)
  (let ((rope (make-list rope-size :initial-element #C(0 0)))
        (visited (make-hash-table :test #'eql)))
    (setf (gethash #C(0 0) visited) t)
    (loop for (dir . val) in moves
          do (loop repeat val
                   do (setf rope (move-rope rope dir)
                            (gethash (last1 rope) visited) t)))
    (hash-table-count visited)))

;; => 6030
(defun part1 (&optional (moves (parse-input (read-input :format :lines))))
  (simulate moves 2))

;; => 2545
(defun part2 (&optional (moves (parse-input (read-input :format :lines))))
  (simulate moves 10))

(defparameter *example* '("R 4"
                          "U 4"
                          "L 3"
                          "D 1"
                          "R 4"
                          "D 1"
                          "L 5"
                          "R 2"))
