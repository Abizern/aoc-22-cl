(uiop:define-package :aoc-22/day12
  (:use :cl :aoc-22/utils)
  (:documentation "--- Day 12: Hill Climbing Algorithm ---"))

(in-package :aoc-22/day12)

(defun matching-fn (char)
  (lambda (grid)
    (cells-matching grid (lambda (c) (char= char c)))))

(defun highest-cell (grid)
  (last1 (funcall (matching-fn #\E) grid)))

(declaim (inline char-value))
(defun char-value (c)
  (case c
    (#\S (char-code #\a))
    (#\E (char-code #\z))
    (otherwise (char-code c))))

(declaim (inline height-difference))
(defun height-difference (grid source target)
  (- (char-value (cell-value grid target))
     (char-value (cell-value grid source))))

(declaim (inline step-back-p))
(defun step-back-p (grid curr next)
  (>= (height-difference grid curr next) -1))

(defun valid-neighbours (grid pos)
  (loop for n in (cell-neighbours grid pos :diagonals nil)
        when (step-back-p grid pos n)
          collect n))

(defun bfs (grid start target-char)
  (let* ((visited (make-hash-table :test #'equal)))
    (setf (gethash start visited) t)
    (loop for steps from 0
          for frontier = (list start) then next
          for next = (loop for pos in frontier
                           nconc (loop for new in (valid-neighbours grid pos)
                                       when (not (gethash new visited))
                                         do (setf (gethash new visited) t)
                                         and collect new))
          when (member target-char (mapcar (lambda (cell)
                                             (cell-value grid cell))
                                           frontier))
            return steps
          when (null frontier)
            return nil)))

;; => 504
(defun part1 (&optional (grid (lines-to-grid (read-input :format :lines))))
  (bfs grid (highest-cell grid) #\S))

;; => 500
(defun part2 (&optional (grid (lines-to-grid (read-input :format :lines))))
  (bfs grid (highest-cell grid) #\a))
