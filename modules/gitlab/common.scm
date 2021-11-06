(define-module (gitlab common)
  #:export (constructor-argument
            cons-or-null
            make-sieved-list))

(define-syntax cons-or-null
  (syntax-rules ()
    ((_ key value)
     (if (not (equal? value 'undefined))
         (cons key value)
         '()))
    ((_ key value converter)
     (if (not (equal? value 'undefined))
         (cons key (converter value))
         '()))))

(define (make-sieved-list . elements)
  (delete '() elements))

(define (constructor-argument keyword initargs)
  (and (memq keyword initargs)
       (cadr (memq keyword initargs))))
