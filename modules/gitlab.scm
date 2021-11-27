;;; gitlab.scm -- Guile GitLab API.

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

;; Guile-GitLab provides procedures for interaction with GitLab REST API.


;;; Code:

(define-module (gitlab)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (gitlab common)
  #:use-module (gitlab client)
  #:use-module (gitlab session)
  #:use-module (gitlab api user)
  #:use-module (gitlab api group)
  #:use-module (gitlab api project)
  #:use-module (gitlab api version)
  #:re-export (<session>
               gitlab-session-token
               gitlab-session-client
               gitlab-session-debug-mode?
               gitlab-api-users-get
               gitlab-api-users-delete
               gitlab-api-groups-get
               gitlab-api-projects-get
               gitlab-api-version-get))

;;; gitlab.scm ends here.
