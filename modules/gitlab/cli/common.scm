(define-module (gitlab cli common)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:export (print
            print-many
            string->boolean
            string-any=?
            command-match))


(define (%print-human-readable result fields)
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

(define* (%print-csv result fields #:key (print-header? #t))
  (let ((fields (and fields (string-split fields #\,))))
    (let loop ((res     result)
               (headers '())
               (values  '()))
      (if (null? res)
          (begin
            (when print-header?
              (display (string-join headers ","))
              (newline))
            (display (string-join values ","))
            (newline))
          (let* ((rec   (car res))
                 (key   (car rec))
                 (value (cdr rec)))
            (if (or (not fields) (member key fields))
                (loop (cdr res)
                      (cons key headers)
                      (cons (cond
                             ((equal? value #f)
                              "false")
                             ((equal? value #t)
                              "true")
                             (else
                              (object->string value)))
                            values))
                (loop (cdr res)
                      headers
                      values)))))))

(define (%print-scheme result fields)
  (display result)
  (newline)
  (if fields
      (let ((fields (string-split fields #\,)))
        (pretty-print (filter (lambda (rec) (member (car rec) fields))
                              result)))
      (pretty-print result)))

(define* (print result fields #:key (format 'scheme))
  (case format
    ((scheme)
     (%print-scheme result fields))
    ((csv)
     (%print-csv result fields))
    ((human-readable)
     (%print-human-readable result fields))
    (else
     (error "Unknown format" format))))



(define (%print-many/csv result fields)
  (unless (null? result)
    (%print-csv (car result) fields #:print-header? #t)
    (for-each (lambda (data)
                (%print-csv data fields #:print-header? #f))
              (cdr result))))

(define* (print-many result fields #:key (format 'scheme))
  (case format
    ((scheme)
     (if fields
         (let ((fields (string-split fields #\,)))
           (pretty-print (map (lambda (user)
                                (filter (lambda (rec) (member (car rec) fields))
                                        user))
                              result)))
         (pretty-print result)))
    ((csv)
     (%print-many/csv result fields))
    ((human-readable)
     (for-each (lambda (data)
                 (%print-human-readable data fields)
                 (newline))
               result))))

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
