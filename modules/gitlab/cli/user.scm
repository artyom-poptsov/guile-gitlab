(define-module (gitlab cli user)
  #:use-module (oop goops)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (web uri)
  #:use-module (gitlab)
  #:use-module (gitlab cli common)
  #:export (gitlab-cli-user))



(define (print-user-help program-name)
  (format #t "\
Usage: ~a user <sub-command> [arguments]

Sub-commands:
  list, ls    List users in various ways.
  remove, rm  Delete the specified user.
"
          program-name))

(define %user-option-spec
  '((help   (single-char #\h) (value #f))
    (server (single-char #\s) (value #t))
    (token  (single-char #\t) (value #t))
    (limit  (single-char #\l) (value #t))
    (print  (single-char #\p) (value #t))
    (format (single-char #\f) (value #t))
    (id                       (value #t))
    (active?                  (value #t))
    (order-by                 (value #t))
    (sort                     (value #t))
    (name                     (value #t))
    (email-like               (value #t))
    (email-not-like           (value #t))
    (username                 (value #t))
    (search                   (value #t))
    (state                    (value #t))))

(define (string->boolean str)
  (cond
   ((equal? str 'undefined)
    'undefined)
   ((string=? str "true")
    #t)
   ((string=? str "false")
    #f)
   (else
    (error "Wrong boolean value (expecting 'true' or 'false')" str))))



(define (print-user-help/list program-name)
  (format #t "\
Usage: ~a user list [arguments]
       ~a user ls   [arguments]

Options:
  --token <token>
  --server <server>
  --limit <limit>
              Limit the number of users that will be requested
              from the server.  By default, the command will fetch
              all the users that satisfy the requirements set by
              other options.
  --print, -p <list-of-parameters>
              Print only the specified parameters from this
              comma-separated list.
              Value example: name,email
  --id <id>
              ID of a user that should be requested.
  --name <name>
  --email-like <regexp>
              Print only users whose emails are matched by the
              regexp.  Note that currently this option does not work when
              '--id' is used.
  --email-not-like <regexp>
              Print only users whose emails are NOT matched by the
              regexp.  Note that currently this option does not work when
              '--id' is used.
  --username <username>
  --search <query>
              Search for users by name, username, primary email,
              or secondary email, by using this option.
  --state <state>
"
          program-name
          program-name))

(define (gitlab-cli-user-list program-name args)
  (let* ((options (getopt-long (cons program-name args) %user-option-spec))
         (help-needed? (option-ref options 'help      #f))
         ;; Required parameters.
         (server       (option-ref options 'server    #f))
         (token        (option-ref options 'token     #f))
         ;; Optional parameters.
         (limit        (option-ref options 'limit     #f))
         (fields       (option-ref options 'print     #f))
         (print-format (string->symbol (option-ref options 'format    "scheme")))
         (id           (option-ref options 'id        #f))
         (active?      (option-ref options 'active?   'undefined))
         (external?    (option-ref options 'external? 'undefined))
         (order-by     (option-ref options 'order-by  'undefined))
         (sort         (option-ref options 'sort      'undefined))
         (name         (option-ref options 'name      'undefined))
         (email-like   (option-ref options 'email-like #f))
         (email-not-like (option-ref options 'email-not-like #f))
         (username     (option-ref options 'username  'undefined))
         (search       (option-ref options 'search    'undefined))
         (state        (option-ref options 'state     'undefined)))

    (when (or help-needed? (< (length args) 2))
      (print-user-help/list program-name)
      (exit 0))

    (unless server
      (error "'--server' option must be specified" args))

    (unless token
      (error "'--token' option must be specified" args))

    (let* ((session (make <session>
                      #:endpoint server
                      #:token    token))
           (result (gitlab-request-users session
                                         #:limit     (and limit
                                                          (string->number limit))
                                         #:id        id
                                         #:username  username
                                         #:active?   (if (equal? active? 'undefined)
                                                         'undefined
                                                         (string->boolean active?))
                                         #:external? (if (equal? external? 'undefined)
                                                         'undefined
                                                         (string->boolean external?))
                                         #:order-by  order-by
                                         #:search    search
                                         #:sort      sort)))

      (when (and email-like email-not-like)
        (error "Only one option must be specified: --email-like, --email-not-like"))

      (cond
       (id
        (print result fields #:format print-format))
       (email-like
        (let* ((lst (vector->list result))
               (rx  (make-regexp email-like))
               (filtered-lst (filter (lambda (user)
                                       (regexp-exec rx (assoc-ref user "email")))
                                     lst)))
          (print-many filtered-lst fields #:format print-format)))
       (email-not-like
        (let* ((lst (vector->list result))
               (rx  (make-regexp email-not-like))
               (filtered-lst (filter (lambda (user)
                                       (not (regexp-exec rx (assoc-ref user "email"))))
                                     lst)))
          (print-many filtered-lst fields #:format print-format)))
       (else
        (print-many (vector->list result) fields #:format print-format))))))



(define %user-remove-option-spec
  '((help   (single-char #\h) (value #f))
    (server (single-char #\s) (value #t))
    (token  (single-char #\t) (value #t))
    (id                       (value #t))
    (force  (single-char #\f) (value #f))
    (hard-delete              (value #f))))

(define (print-user-delete-help program-name)
  (format #t "\
Usage: ~a user remove [arguments]
Usage: ~a user rm     [arguments]

Required options:
  --server, -s <server-url>
  --token, -t <token>
  --id <id>

Other options:
  --force, -f
              Don't ask questions, just do it.
  --hard-delete?
              Delete all user's contributions.
  --help, -h
"
          program-name
          program-name))

(define (gitlab-cli-user-remove program-name args)
  (let* ((options (getopt-long (cons program-name args) %user-remove-option-spec))
         (help-needed? (option-ref options 'help      #f))
         ;; Required parameters.
         (server       (option-ref options 'server    #f))
         (token        (option-ref options 'token     #f))
         (id           (option-ref options 'id        #f))
         (force?       (option-ref options 'force     #f))
         (hard-delete? (option-ref options 'hard-delete #f)))

    (when help-needed?
      (print-user-delete-help program-name)
      (exit 0))

    (unless server
      (error "'--server' option must be specified" args))

    (unless token
      (error "'--token' option must be specified" args))

    (unless id
      (error "'--id' option must be specified" args))

    (unless force?
      (format #t
              "User with ID ~a is going to be deleted.  Proceed? (y/n) "
              id)
      (let ((response (read-line)))
        (unless (or (string=? response "y")
                    (string=? response "Y"))
          (format #t "Canceling the operation~%")
          (exit 0))))

    (let ((session (make <session>
                     #:endpoint server
                     #:token    token)))
      (gitlab-delete-user session
                          id
                          #:hard-delete? (if (equal? hard-delete? #f)
                                             'undefined
                                             "true")))))



(define %commands
  `((("list" "ls")        ,gitlab-cli-user-list)
    (("remove" "rm")      ,gitlab-cli-user-remove)))

(define (gitlab-cli-user program-name args)
  (when (zero? (length args))
    (print-user-help program-name)
    (exit 0))

  (let* ((sub-command (car args))
         (handler     (command-match sub-command %commands)))
    (if handler
        (handler program-name (cdr args))
        (begin
          (print-user-help program-name)
          (exit 0)))))

;;; user.scm ends here.

