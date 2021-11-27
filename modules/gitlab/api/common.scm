;;; common.scm -- Common code for Guile-GitLab API.

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


;;; Commentary:

;; This module contains @code{api-get} procedure that allows to fetch GitLab
;; data with pagination.


;;; Code:

(define-module (gitlab api common)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:use-module (gitlab session)
  #:export (api-get))


(define* (api-get session
                  resource
                  #:key
                  (query         '())
                  (limit         #f)
                  (max-page-size 100))
  (let ((get (lambda (page page-size)
               (client-get (gitlab-session-client session)
                           resource
                           #:query (cons (cons 'per_page
                                               (number->string page-size))
                                         (cons (cons 'page (number->string page))
                                               query))))))
    (if limit
        (let ((first-page (get 1 limit)))
          (if (>= (vector-length first-page) limit)
              first-page
              (let loop ((data   (get 2 (- limit max-page-size)))
                         (result (vector->list first-page))
                         (page   2))
                (when (gitlab-session-debug-mode? session)
                  (format (current-error-port) "PAGE: ~a~%" page))
                (cond
                 ((zero? (vector-length data))
                  (list->vector result))
                 ((>= (+ (vector-length data)
                         (length result))
                      limit)
                  (list->vector (append result
                                        (vector->list data))))
                 (else
                  (loop (get (+ page 1)
                             (- limit (* max-page-size page)))
                        (append result (vector->list data))
                        (+ page 1)))))))
        (let loop ((data   (get 1 max-page-size))
                   (result '())
                   (page   1))
          (if (zero? (vector-length data))
              (list->vector result)
              (loop (get (+ page 1) max-page-size)
                    (append result (vector->list data))
                    (+ page 1)))))))

;;; common.scm ends here.

