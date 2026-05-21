#lang racket
(provide tokenize styler print-tokens)

(define allRegex
  '(
    ("space" #rx"^[ \t\n\r]+")
    ("pr-states" #rx"^states")
    ("pr-alphabet" #rx"^alphabet")
    ("pr-startState" #rx"^start_state")
    ("pr-finalState" #rx"^final_state")
    ("pr-transitions" #rx"^transitions")
    ("pr-check" #rx"^check")
    ("string" #rx"^[‘'’][^‘'’]*[’'’]")
    
    ("state-id" #rx"^q[0-9]+")
    ("separator-coma" #rx"^,")
    ("assignation-dots" #rx"^:")
    ("terminator-semicol" #rx"^;")
    ("comment" #rx"^//[^\n\r]*")
    ("operand-transition" #rx"^->")
    ("open-braces" #rx"^{")
    ("close-braces" #rx"^}")
   
   ))

(define (getMatch label-regex str)
  (let ([label (first label-regex)]
        [regex (second label-regex)])
    (let ([match (regexp-match-positions regex str)])
      (if match
          (let ([lenMatch (cdr (first match))])
            (list label lenMatch (substring str 0 lenMatch)))
          (list "none" 0 "")))))
(define (print-tokens token-list)
  (for ([t token-list])
    (printf "~a : ~a\n" (first t) (second t))))
(define (getMaximo allMatches)
  (let* ([longs (map second allMatches)]
         [maxLen (apply max longs)])
    (if (= maxLen 0)
        (list "error" 1 " ") 
        (first (filter (lambda (row) (= maxLen (second row))) allMatches)))))

(define (tokenize str)
  (if (zero? (string-length str))
      '()
      (let* ([allMatches (map (lambda (r) (getMatch r str)) allRegex)]
             [bestMatch  (getMaximo allMatches)]
             [label      (first bestMatch)]
             [len        (second bestMatch)]
             [lexema     (third bestMatch)]
             [restStr    (substring str len)])
        (if (equal? label "space")
            (tokenize restStr)
            (cons (list label lexema) (tokenize restStr))))))

(define (styler token)
  (let ([label (first token)]
        [lexema (second token)])
    (cond
      [(equal? label "pr-states") (format "<span class='keyword'>~a</span>"  lexema)]
      [(equal? label "pr-alphabet") (format "<span class='keyword'>~a</span>"  lexema)]
      [(equal? label "pr-startState") (format "<span class='keyword'>~a</span>"  lexema)]
      [(equal? label "pr-finalState") (format "<span class='keyword'>~a</span>"  lexema)]
      [(equal? label "pr-transitions") (format "<span class='keyword'>~a</span>"  lexema)]
      [(equal? label "pr-check") (format "<span class='keyword'>~a</span>"  lexema)]
      [(equal? label "state-id")  (format "<span class='stateId'>~a</span>"  lexema)]
      [(equal? label "assignation-dots")    (format "<span class='assignation'>~a</span>"    lexema)]
      [(equal? label "separator-coma")     (format "<span class='comma'>~a</span>"     lexema)]
      [(equal? label "terminator-semicol")(format "<span class='semicolon'>~a</span><br>" lexema)]
      [(equal? label "comment") (format "<span class='comment'>~a</span>"  lexema)]
      [(equal? label "operand-transition") (format "<span class='transition'>~a</span>"  lexema)]
      [(equal? label "open-braces") (format "<span class='braces'>~a</span>"  lexema)]
      [(equal? label "close-braces") (format "<span class='braces'>~a</span>"  lexema)]
      [(equal? label "string") (format "<span class='string'>~a</span>"  lexema)]
      [else (format "<span>~a</span>" lexema)])))