(define-module (gitlab cli version)
  #:use-module (oop goops)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (web uri)
  #:use-module (gitlab)
  #:use-module (gitlab cli common)
  #:export (gitlab-cli-version))



(define (print-help program-name)
  (format #t "\
Usage: ~a version

This command prints the current gitlab version to standard output stream.
"
          program-name))



(define %option-spec
  '((server (single-char #\s) (value #t))
    (token  (single-char #\t) (value #t))
    (help   (single-char #\h) (value #f))))



(define (gitlab-cli-version program-name args)
  (let* ((options (getopt-long (cons program-name args) %option-spec))
         ;; Required parameters.
         (server       (option-ref options 'server    #f))
         (token        (option-ref options 'token     #f))
         ;; Optional parameters.
         (help-needed? (option-ref options 'help      #f)))

    (when (or help-needed? (< (length args) 1))
      (print-help program-name)
      (exit 0))

    (unless server
      (error "'--server' option must be specified" args))

    (unless token
      (error "'--token' option must be specified" args))

    (let* ((session (make <session>
                      #:endpoint server
                      #:token    token))
           (result (gitlab-api-version-get session)))
      (pretty-print result)
      (newline))))

;;; version.scm ends here.

