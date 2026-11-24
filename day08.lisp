(uiop:define-package :aoc-22/day08
  (:use :cl :aoc-22/utils)
  (:documentation "--- Day 8: Treetop Tree House ---"))

(in-package :aoc-22/day08)

(defun tree (grid r c)
  (cell-value grid (cons r c)))

(defun visible-p (grid r c)
  (labels ((shorterp (i j) (> (tree grid r c) (tree grid i j))))
    (or
     (loop for i from (1- r) downto 0 always (shorterp i c))
     (loop for i from (1+ r) below (grid-rows grid) always (shorterp i c))
     (loop for j from (1- c) downto 0 always (shorterp r j))
     (loop for j from (1+ c) below (grid-cols grid) always (shorterp r j)))))

(defun scenery-score (grid r c)
  (labels ((shorterp (i j) (> (tree grid r c) (tree grid i j))))
    (*
     (loop for i from (1- r) downto 0 sum 1 while (shorterp i c))
     (loop for i from (1+ r) below (grid-rows grid) sum 1 while (shorterp i c))
     (loop for j from (1- c) downto 0 sum 1 while (shorterp r j))
     (loop for j from (1+ c) below (grid-cols grid) sum 1 while (shorterp r j)))))

(defun parse-input (lines)
  (lines-to-grid lines #'digit-char-p))

;; => 1705
(defun part1 (&optional (grid (parse-input (read-input :format :lines))))
  (loop for i from 0 below (grid-rows grid)
        sum (loop for j from 0 below (grid-cols grid)
                  count (visible-p grid i j))))

;; => 371200
(defun part2 (&optional (grid (parse-input (read-input :format :lines))))
  (loop for i from 0 below (grid-rows grid)
        maximize (loop for j from 0 below (grid-cols grid)
                       maximize (scenery-score grid i j))))
