;;; session.scm -- GitLab session implementation.

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

;; This module contains implementation of <session> class that represents a
;; GitLab API session.


;;; Code:

(define-module (gitlab session)
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:export (<session>
            gitlab-session-token
            gitlab-session-client
            gitlab-session-debug-mode?))


(define-class-with-docs <session> ()
  "This class describes a GitLab API session."

  ;; GitLab authentication token.
  ;;
  ;; <string>
  (token
   #:init-keyword #:token
   #:getter       gitlab-session-token)

  ;; GitLab <client> instance.
  ;;
  ;; <client>
  (client
   #:setter gitlab-session-client-set!
   #:getter gitlab-session-client)

  ;; <boolean>
  (debug-mode?
   #:init-value   #f
   #:init-keyword #:debug-mode?
   #:getter       gitlab-session-debug-mode?))

(define-method (initialize (session <session>) initargs)
  (next-method)
  (let ((token       (constructor-argument #:token initargs))
        (endpoint    (constructor-argument #:endpoint initargs))
        (debug-mode? (constructor-argument #:debug-mode? initargs)))
    (gitlab-session-client-set! session (make <client>
                                          #:debug? debug-mode?
                                          #:token  token
                                          #:server (string->uri endpoint)))))

;;; session.scm ends here.

