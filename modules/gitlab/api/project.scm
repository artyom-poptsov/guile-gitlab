;;; project.scm -- Code for GitLab Projects API.

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

;; This module contains procedures to work with GitLab Projects API.


;;; Code:


(define-module (gitlab api project)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:use-module (gitlab session)
  #:use-module (gitlab api common)
  #:export (gitlab-api-projects-get))



(define* (gitlab-api-projects-get session
                                  #:key
                                  (id                #f)
                                  (limit             #f)
                                  (owned?            'undefined))
  (let ((query
         (make-sieved-list
          (cons-or-null 'owned owned?))))
    (if id
        (client-get (gitlab-session-client session)
                    (format #f "/api/v4/projects/~a" id)
                    #:query query)
        (api-get session
                 "/api/v4/projects"
                 #:limit limit
                 #:query query))))

;;; project.scm ends here.
