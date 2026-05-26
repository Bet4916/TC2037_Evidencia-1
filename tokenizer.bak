#lang racket

; tokenizer
; Autores: Ivan Burrola, Alberto Lopez, Axel Lugo, Sebastian Viche

(provide tokenize styler print-tokens
         extrae-errores-lex tokens-sin-errores tokens-sin-comentarios)

; -----------------------------------------------------------------------
; allRegex: lista de pares (label regex)
; Tabla de reglas del lexico. El orden no define prioridad (la elige
; getMaximo por longitud), pero las palabras reservadas van antes que
; state-id para evitar ambiguedades de prefijo.
(define allRegex
  '(
    ("space"              #rx"^[ \t\n\r]+")
    ("pr-states"          #rx"^states")
    ("pr-alphabet"        #rx"^alphabet")
    ("pr-startState"      #rx"^start_state")
    ("pr-finalState"      #rx"^final_state")
    ("pr-transitions"     #rx"^transitions")
    ("pr-check"           #rx"^check")
    ("string"             #rx"^'[^']*'")
    ("state-id"           #rx"^q[0-9]+")
    ("separator-coma"     #rx"^,")
    ("assignation-dots"   #rx"^:")
    ("terminator-semicol" #rx"^;")
    ("comment"            #rx"^//[^\n\r]*")
    ("operand-transition" #rx"^->")
    ("open-braces"        #rx"^[{]")
    ("close-braces"       #rx"^[}]")
  ))

; getMatch
; Intenta hacer match de UNA regex al inicio del string.
; Devuelve (label 0 "")
(define (getMatch label-regex str)
  (let ([label (first  label-regex)]
        [regex (second label-regex)])
    (let ([match (regexp-match-positions regex str)])
      (if match
          (let ([lenMatch (cdr (first match))])
            (list label lenMatch (substring str 0 lenMatch)))
          (list "none" 0 "")))))

; print-tokens: lista-de-tokens
; Imprime cada token en formato "label : lexema"
(define (print-tokens token-list)
  (for ([t token-list])
    (printf "~a : ~a\n" (first t) (second t))))

; getMaximo: lista-de-matches busca el bestMatch
(define (getMaximo allMatches)
  (let* ([longs  (map second allMatches)]
         [maxLen (apply max longs)])
    (if (= maxLen 0)
        (list "error" 1 "?")
        (first (filter (lambda (row) (= maxLen (second row))) allMatches)))))

; tokenize: string lo vuelve una lista-de-tokens
(define (tokenize str)
  (if (zero? (string-length str))
      '()
      (let* ([allMatches (map (lambda (r) (getMatch r str)) allRegex)]
             [bestMatch  (getMaximo allMatches)]
             [label      (first  bestMatch)]
             [len        (second bestMatch)]
             [restStr    (substring str len)])
        (cond
          ; Espacioos o saltos son dummies
          [(equal? label "space")
           (tokenize restStr)]
          ; Caracter no reconocido es un error
          [(equal? label "error")
           (cons (list "error" (substring str 0 1))
                 (tokenize (substring str 1)))]
          ; Token valido lo emite y continua con el resto del string
          [else
           (cons (list label (third bestMatch)) (tokenize restStr))]))))

; Funciones de filtrado

; extrae-errores-lex: lista-tokens 
(define (extrae-errores-lex tokens)
  (map (lambda (tok)
         (format "Error lexico: simbolo '~a' no reconocido" (second tok)))
       (filter (lambda (tok) (equal? (first tok) "error")) tokens)))

; tokens-sin-errores: lista-tokens -> lista-tokens
; Devuelve solo los tokens validos (excluye tokens "error").
(define (tokens-sin-errores tokens)
  (filter (lambda (tok) (not (equal? (first tok) "error"))) tokens))

; tokens-sin-comentarios: lista-tokens -> lista-tokens
; Excluye los tokens de comentario. El parser los ignora; solo sirven
; para el resaltado de color en el HTML.
(define (tokens-sin-comentarios tokens)
  (filter (lambda (tok) (not (equal? (first tok) "comment"))) tokens))

; -----------------------------------------------------------------------
; styler: token -> string-HTML
; Convierte un token en su representacion HTML con la clase CSS correcta.
; Sigue el patron del esqueleto: una clausula cond por tipo de token.
; Los tokens de ';' y comentarios agregan <br> para mantener legibilidad.
(define (styler token)
  (let ([label  (first  token)]
        [lexema (second token)])
    (cond
      [(equal? label "pr-states")
       (format "<span class='keyword'>~a</span> " lexema)]
      [(equal? label "pr-alphabet")
       (format "<span class='keyword'>~a</span> " lexema)]
      [(equal? label "pr-startState")
       (format "<span class='keyword'>~a</span> " lexema)]
      [(equal? label "pr-finalState")
       (format "<span class='keyword'>~a</span> " lexema)]
      [(equal? label "pr-transitions")
       (format "<span class='keyword'>~a</span> " lexema)]
      [(equal? label "pr-check")
       (format "<span class='keyword'>~a</span> " lexema)]
      [(equal? label "state-id")
       (format "<span class='stateId'>~a</span> " lexema)]
      [(equal? label "assignation-dots")
       (format "<span class='assignation'>~a</span> " lexema)]
      [(equal? label "separator-coma")
       (format "<span class='comma'>~a</span> " lexema)]
      [(equal? label "terminator-semicol")
       (format "<span class='semicolon'>~a</span><br>" lexema)]
      [(equal? label "comment")
       (format "<span class='comment'>~a</span><br>" lexema)]
      [(equal? label "operand-transition")
       (format "<span class='transition'>~a</span> " lexema)]
      [(equal? label "open-braces")
       (format "<span class='braces'>~a</span> " lexema)]
      [(equal? label "close-braces")
       (format "<span class='braces'>~a</span> " lexema)]
      [(equal? label "string")
       (format "<span class='string'>~a</span> " lexema)]
      [(equal? label "error")
       (format "<span class='lex-error'>~a</span> " lexema)]
      [else
       (format "<span>~a</span> " lexema)])))
