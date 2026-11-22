;;;; Advent of Code 2022 solutions in Common Lisp
;;;;
;;;; Each file defines one package named after its path.
;;;;
;;;; (ql:quickload "aoc-22") loads the libraries and utilities
;;;; that are used. Individual days are compiled from the editor
;;;; or loaded using (ql:quickload "aoc-22/day01").

(defsystem "aoc-22"
  :description "System for solutions to Advent of Code 2022 in Common Lisp"
  :author "Abizer Nasir <abizern@mac.com>"
  :version "0.1.0"
  :license "MIT"
  :class :package-inferred-system
  :depends-on ("aoc-22/utils"))

;;; If a test system is added, add its definition here.
