(define-module (gitlab cli common)
  #:use-module (ice-9 pretty-print)
  #:export (print
            print-many
            string->boolean
            string-any=?
            command-match))


(define (print result fields)
  (let ((fields (and fields (string-split fields #\,))))
    (for-each (lambda (rec)
                (let ((key   (car rec))
                      (value (cdr rec)))
                  (if (or (not fields)
                          (member key fields))
                      (format #t "~20a: ~a~%"
                              key
                              (cond
                               ((equal? value #f)
                                "false")
                               ((equal? value #t)
                                "true")
                               (else
                                value))))))
              result)))

(define (print-many data fields)
  (if fields
      (let ((fields (string-split fields #\,)))
        (pretty-print (map (lambda (user)
                             (filter (lambda (rec) (member (car rec) fields))
                                     user))
                           data)))
      (pretty-print data)))



(define (string->boolean str)
  (cond
   ((string=? str "true")
    #t)
   ((string=? str "false")
    #f)
   (else
    (error "Wrong boolean value (expecting 'true' or 'false')" str))))



(define (string-any=? str string-list)
  (cond
   ((null? string-list)
    #f)
   ((string=? str (car string-list))
    #t)
   (else
    (string-any=? str (cdr string-list)))))

(define (command-match command command-list)
  (if (string-any=? command (caar command-list))
      (cadar command-list)
      (command-match command (cdr command-list))))

;;; common.scm ends here.
