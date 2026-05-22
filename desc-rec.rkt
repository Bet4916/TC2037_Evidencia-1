#lang racket


(define (match-token expected-label tokens errors)
  (cond
    [(empty? tokens) (list tokens (add-error errors expected-label tokens))]
    [(equal? expected-label (caar tokens)) (list (rest tokens) errors)]
    [else (list (rest tokens) (add-error errors expected-label tokens))]
    )
  )

(define (add-error errors expected-label tokens)
  (let ([token-recibido (if (empty? tokens) 
                            "Fin de archivo (EOF)" 
                            (caar tokens))])
    (append errors
            (list (format "Error: esperaba ~a, se recibio ~a" expected-label token-recibido)))))

(define (main-recdes tokens errors)
  (let* ([t1 (states-decl tokens errors)]
         [t2 (alphabet-decl t1 errors)]
         [t3 (start-decl t2 errors)]
         [t4 (final-decl t3 errors)]
         [t5 (transition-block t4 errors)]
         [t-fin (check-decl t5 errors)]    
         )
    (if (empty? t-fin)
        (displayln "La cadena es válida y aceptada por la grámatica") (displayln "Error: Hay tokens que sobran")
        )
    )
  )

(define (states-ids tokens errors)
  (match-let*(
              [(list tok1 err1) (match-token "state-id" tokens errors)]
              )
    (ids-tail tok1 err1)
      )
  )
(define (ids-tail tokens errors)
  (cond
    [(empty? tokens) (list tokens errors)]
    [(equal? "separator-coma" (caar tokens)) (match-let*(
                                                         [(list tok-coma err-coma) (match-token "separator-coma" tokens errors)]
                                                         [(list tok-id err-id) (match-token "state-id" tok-coma err-coma)]
                                                         )
                                               (ids-tail tok-id err-id)
                                               )]
    [else (list tokens errors)]
    )
  )
(define (states-decl tokens errors)
  (match-let* (
               [(list tok1 err1) (match-token "pr-states" tokens errors)]
               [(list tok2 err2) (match-token "assignation-dots" tok1 err1)]
               [(list tok3 err3) (match-token "open-braces" tok2 err2)]
               [(list tok4 err4) (states-ids tok3 err3)]
               [(list tok5 err5) (match-token "close-braces" tok4 err4)]
               [(list tok6 err6) (match-token "terminator-semicol" tok5 err5)]
               
               )
    (list tok6 err6))
  )


