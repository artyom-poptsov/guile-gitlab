;;; group.scm -- Code for GitLab Users API.

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

;; This module contains procedures to work with GitLab Users API.


;;; Code:


(define-module (gitlab api user)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:use-module (gitlab session)
  #:use-module (gitlab api common)
  #:export (gitlab-api-users-get
            gitlab-api-users-delete))



(define* (gitlab-api-users-get session
                               #:key
                               (id                #f)
                               (limit             #f)
                               (username          'undefined)
                               (active?           'undefined)
                               (blocked?          'undefined)
                               (external?         'undefined)
                               (exclude-internal? 'undefined)
                               (exclude-external? 'undefined)
                               (order-by          'undefined)
                               (sort              'undefined)
                               (two-factor        'undefined)
                               (admins            'undefined)
                               (search            'undefined)
                               (without-projects? 'undefined))
  (let ((max-page-size 100)
        (query
         (make-sieved-list
          (cons-or-null 'username username)
          (cons-or-null 'active active?)
          (cons-or-null 'blocked blocked?)
          (cons-or-null 'external external?)
          (cons-or-null 'exclude_external exclude-external?)
          (cons-or-null 'exclude_internal exclude-internal?)
          (cons-or-null 'order_by order-by)
          (cons-or-null 'sort sort)
          (cons-or-null 'two_factor two-factor)
          (cons-or-null 'admins admins)
          (cons-or-null 'search search)
          (cons-or-null 'without_projects without-projects?))))
    (if id
        (client-get (gitlab-session-client session)
                    (format #f "/api/v4/users/~a" id)
                    #:query query)
        (api-get session
                 "/api/v4/users/"
                 #:limit limit
                 #:query query))))

(define* (gitlab-api-users-delete session
                                  id
                                  #:key
                                  (hard-delete?   'undefined))
  (let ((query
         (make-sieved-list
          (cons-or-null 'hard_delete hard-delete?))))
    (client-delete (gitlab-session-client session)
                   (format #f "/api/v4/users/~a" id)
                   #:query query)))

;;; user.scm ends here.
