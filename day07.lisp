(uiop:define-package :aoc-22/day07
  (:use :cl :aoc-22/utils)
  (:import-from :alexandria #:ensure-gethash)
  (:import-from :cl-ppcre
                #:all-matches-as-strings
                #:register-groups-bind)
  (:documentation "--- Day 7: No Space Left On Device ---"))

(in-package :aoc-22/day07)

(defstruct dir
  (name "" :type string)
  (parent nil :type (or null dir))
  (children (make-hash-table :test #'equal) :type hash-table)
  (files-size 0 :type integer))

(defun parse-command-cd (line)
  (register-groups-bind (name)
      ("^\\$ cd (\\S+)$" line)
    (cond ((string= name "/")  (cons :root nil))
          ((string= name "..") (cons :up 1))
          (t                   (cons :cd name)))))

(defun parse-size (line)
  (register-groups-bind ((#'parse-integer size))
      ("^(\\d+) " line)
    (cons :size size)))

(defun parse-line (line)
  (or (parse-command-cd line)
      (parse-size line)))

(defun parse-input (lines)
  (remove-if #'null (mapcar #'parse-line lines)))

(defun process-commend (current command)
  (destructuring-bind (cmd . val) command
    (ecase cmd
      (:root (make-dir :name "/"))
      (:up   (dir-parent current))
      (:cd   (or (gethash val (dir-children current))
                 (let ((new-dir (make-dir :name val :parent current)))
                   (setf (gethash val (dir-children current)) new-dir)
                   new-dir)))
      (:size (incf (dir-files-size current) val) current))))

(defun build-dirs (commands)
  (let ((dir-list '())
        (current nil))
    (dolist (cmd commands dir-list)
      (setf current (process-commend current cmd))
      (pushnew current dir-list))))

(defun total-size (d)
  (+ (dir-files-size d)
     (loop for child being the hash-values of (dir-children d)
           sum (total-size child))))

;; => 1443806
(defun part1 (&optional (input (parse-input (read-input :format :lines))))
  (loop for d in (build-dirs input)
        for size = (total-size d)
        when (<= size 100000)
          sum size))

;; => 942298
(defun part2 (&optional (input (parse-input (read-input :format :lines))))
  (let* ((dirs (build-dirs input))
         (root (find "/" dirs :key #'dir-name :test #'string=))
         (needed (- 30000000 (- 70000000 (total-size root)))))
    (loop for d in dirs
          for size = (total-size d)
          when (>= size needed)
            minimize size)))

(defparameter *example* '("$ cd /"
                          "$ ls"
                          "dir a"
                          "14848514 b.txt"
                          "8504156 c.dat"
                          "dir d"
                          "$ cd a"
                          "$ ls"
                          "dir e"
                          "29116 f"
                          "2557 g"
                          "62596 h.lst"
                          "$ cd e"
                          "$ ls"
                          "584 i"
                          "$ cd .."
                          "$ cd .."
                          "$ cd d"
                          "$ ls"
                          "4060174 j"
                          "8033020 d.log"
                          "5626152 d.ext"
                          "7214296 k"))
