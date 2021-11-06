#!/usr/bin/guile-2.2 \
-L ../modules -e main -s
!#

(use-modules (ice-9 getopt-long)
             (ice-9 format)
             (ice-9 textual-ports)
             (ice-9 pretty-print)
             (srfi  srfi-1)
             (srfi  srfi-26)
             (oop goops)
             (web uri)
             (gitlab))

(define (print-help program-name)
  (format #t "\
Usage: ~a <command> [arguments]

Commands:
  user        Operations on GitLab users.
  help        Print this message and exit.

Global options:
  --token, -t <token>
             Set GitLab authentication token.

User options:
  --id <id>
  --name <name>
  --username <username>
  --email <email>
  --state <state>
"
          program-name))



(define (print result)
  (for-each (lambda (rec)
              (let ((key   (car rec))
                    (value (cdr rec)))
                (format #t "~20a: ~a~%"
                        key
                        (cond
                         ((equal? value #f)
                          "false")
                         ((equal? value #t)
                          "true")
                         (else
                          value)))))
            result))



(define %user-option-spec
  '((help   (single-char #\h) (value #f))
    (server (single-char #\s) (value #t))
    (token  (single-char #\t) (value #t))
    (id                       (value #t))
    (active?                  (value #t))
    (order-by                 (value #t))
    (sort                     (value #t))
    (name                     (value #t))
    (username                 (value #t))
    (email                    (value #t))
    (state                    (value #t))))

(define (string->boolean str)
  (cond
   ((string=? str "true")
    #t)
   ((string=? str "false")
    #f)
   (else
    (error "Wrong boolean value (expecting 'true' or 'false')" str))))

(define (handle-user-command program-name args)
  (let* ((options (getopt-long args %user-option-spec))
         (help-needed? (option-ref options 'help      #f))
         ;; Required parameters.
         (server       (option-ref options 'server    #f))
         (token        (option-ref options 'token     #f))
         ;; Optional parameters.
         (id           (option-ref options 'id        #f))
         (active?      (option-ref options 'active?   'undefined))
         (external?    (option-ref options 'external? 'undefined))
         (order-by     (option-ref options 'order-by  'undefined))
         (sort         (option-ref options 'sort      'undefined))
         (name         (option-ref options 'name      'undefined))
         (username     (option-ref options 'username  'undefined))
         (email        (option-ref options 'email     'undefined))
         (state        (option-ref options 'state     'undefined)))

    (when (or help-needed? (< (length args) 2))
      (print-help program-name)
      (exit 0))

    (unless token
      (error "GitLab token is not provided."))

    (unless server
      (error "GitLab server URL is not provided."))

    (let* ((gitlab (make <gitlab>
                     #:endpoint server
                     #:token    token))
           (result (gitlab-request-users gitlab
                                         #:id        id
                                         #:username  username
                                         #:active?   (if (equal? active? 'undefined)
                                                         'undefined
                                                         (string->boolean active?))
                                         #:external? (if (equal? external? 'undefined)
                                                         'undefined
                                                         (string->boolean external?))
                                         #:order-by  order-by
                                         #:sort      sort)))
      (if id
          (print result)
          (pretty-print result))
      (newline))))



(define (main args)
  (let ((program-name (car args)))

    (when (< (length args) 2)
      (print-help program-name)
      (exit 0))

    (let ((command (cadr args)))
      (cond
        ((string=? command "user")
         (handle-user-command program-name (cdr args)))
        (else
         (print-help program-name)
         (exit 0))))))

;;; gitlab-ctl.scm ends here.

