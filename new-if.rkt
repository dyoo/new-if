#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse))

;; Let's define a new version of if that works like Joe and Shriram's design.
;; 
;; (new-if test1 body1 ... elseif test2 body2 ... else body3)
;;
;;

;; We reuse the else from racket/base.  We introduce another keyword elseif.
(define-syntax (elseif stx)
  (raise-syntax-error #f "not allowed as an expression" stx))


;; We need a few utilities at compile-time to help break up the sequence of syntaxes following
;; a new-if form.
(begin-for-syntax
  (struct test-body (test body)) ;; a test syntax, followed by a list of body syntaxes
  (struct else-body (body))      ;; an unconditional list of body syntaxes.
  
  ;; decompose-bodies: (listof syntax) -> (listof (U test-body else-body))
  ;; A little grim...
  (define (decompose-bodies stxs original-stx)
    ;; up-to-next-keyword: (listof syntax) -> (values (listof syntax) (listof syntax))
    ;; Walks the syntax and splits it: the syntaxes before a keyword, and the remainder.
    (define (up-to-next-keyword stxs)
      (let loop ([stxs stxs]
                 [acc '()])
        (cond [(empty? stxs)
               (values (reverse acc) '())]
              [(and (identifier? (first stxs))
                    (or (free-identifier=? (first stxs) #'elseif)
                        (free-identifier=? (first stxs) #'else)))
               (values (reverse acc) stxs)]
              [else
               (loop (rest stxs) (cons (first stxs) acc))])))
    
    (let loop ([stxs stxs])
      (cond
        [(empty? stxs)
         '()]
        [(free-identifier=? (first stxs) #'elseif)
         (define-values (terms rest-of-body) (up-to-next-keyword (rest stxs)))
         (cond
           [(empty? terms)
            (raise-syntax-error #f 
                                (format "Expected non-empty test and body sequence following ~a"
                                        (syntax-e (first stxs)))
                                (first stxs))]
           [else
            (cons (test-body (first terms) (rest terms))
                  (loop rest-of-body))])]
        [(free-identifier=? (first stxs) #'else)
         (define-values (terms rest-of-body) (up-to-next-keyword (rest stxs)))
         (cond
           [(empty? rest-of-body)
            (list (test-body (first terms) (rest terms)))]
           [else
            (raise-syntax-error #f (format "Unexpected keyword ~a after ~a"
                                           (syntax-e (first rest-of-body))
                                           (syntax-e (first stxs)))
                                (first rest-of-body))])]
        [else
         ;; This should be impossible, but let's be exhaustive.
         (raise-syntax-error #f "Internal error" original-stx)]))))


(define-syntax (new-if stx)
  (syntax-parse stx
    [(head elts ...)
     (with-syntax ([mock-elseif (datum->syntax #'here 'elseif #'head)])
       (define clause-bodies (decompose-bodies (syntax->list #'(mock-elseif elts ...)) stx))
       (with-syntax ([(clauses ...)
                      (for/list ([clause-body clause-bodies])
                        (cond [(test-body? clause-body)
                               #`[#,(test-body-test clause-body)
                                  #,@(test-body-body clause-body)]]
                              [else
                               #`[else #,@(else-body-body clause-body)]]))])
         #'(cond clauses ...)))]))



(module+ test
         (define n (read))
         (new-if (= n 42)
                 (printf "you got the right answer\n")
                 (printf "hurrah")
          elseif (= n 16)
                 (printf "nope\n")
                 (printf "no go\n")
          else
                 (printf "hmm\n")))
