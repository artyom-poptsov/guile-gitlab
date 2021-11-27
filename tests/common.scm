;;; dsv.scm -- Tests for Guile-GitLab common code.

;; Copyright (C) 2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (gitlab common))



(define %test-name "common")
(test-begin %test-name)



(test-equal "make-sieved-list"
  '(a b c d)
  (make-sieved-list 'a '() 'b '() 'c '() 'd))

(test-equal "cons-or-null: value"
  '(key . value)
  (cons-or-null 'key 'value))

(test-equal "cons-or-null: undefined"
  '()
  (cons-or-null 'key 'undefined))

(test-equal "cons-or-null: converter"
  '(answer . "42")
  (cons-or-null 'answer 42 number->string))



(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))

;;; common.scm ends here.
