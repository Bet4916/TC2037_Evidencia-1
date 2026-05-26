#lang racket

; desc-rec.rkt
; Parser por descenso recursivo para el lenguaje de descripcion de DFAs.
; Sigue el patron del esqueleto de la maestra (descenso-rec-completo).
;
; Cada funcion de parseado recibe (toks auto errors) y devuelve
; (list toks auto errors), donde:
;   toks   - tokens restantes por consumir
;   auto   - hash del automata en construccion (crece con cada seccion)
;   errors - lista de mensajes de error acumulados
;
; Las funciones base match-token y add-error se mantienen identicas
; al esqueleto original de desc-rec.rkt.
;
; Autores: Ivan Burrola, Alberto Lopez, Axel Lugo, Sebastian Viche
; Materia: Implementacion de metodos computacionales

(provide parse-automata)

; -----------------------------------------------------------------------
; Funciones base del esqueleto original

; add-error: errors string tokens -> errors
; Agrega un mensaje de error a la lista.
(define (add-error errors expected-label tokens)
  (let ([token-recibido (if (empty? tokens)
                            "EOF"
                            (caar tokens))])
    (append errors
            (list (format "Error sintactico: esperaba ~a, se recibio ~a"
                          expected-label token-recibido)))))

; match-token: string tokens errors -> (list tokens errors)
; Intenta consumir el token esperado del frente del stream.
; Si coincide avanza; si no, registra error y avanza de todas formas
; para permitir recuperacion y reportar mas errores.
(define (match-token expected-label tokens errors)
  (cond
    [(empty? tokens)
     (list tokens (add-error errors expected-label tokens))]
    [(equal? expected-label (caar tokens))
     (list (rest tokens) errors)]
    [else
     (list (rest tokens) (add-error errors expected-label tokens))]))

; -----------------------------------------------------------------------
; Funciones auxiliares

; current-label: tokens -> string
(define (current-label toks)
  (if (empty? toks) "EOF" (caar toks)))

; current-lexema: tokens -> string
(define (current-lexema toks)
  (if (empty? toks) "" (second (car toks))))

; strip-quotes: string -> string
; Extrae el contenido entre comillas simples. Si no hay comillas, devuelve s.
(define (strip-quotes s)
  (let ([result (regexp-match #rx"^'(.*)'$" s)])
    (if result (second result) s)))

; -----------------------------------------------------------------------
; parse-list: string tokens errors -> (list tokens lista-lexemas errors)
; Parsea una lista de items del mismo tipo: item (',' item)*
; Devuelve la lista de lexemas encontrados (con comillas si es string).

(define (parse-list-tail item-label toks items errors)
  (cond
    [(empty? toks) (list toks items errors)]
    ; Si hay coma y le sigue el tipo esperado, continua la lista
    [(equal? (current-label toks) "separator-coma")
     (let ([toks1 (rest toks)])
       (cond
         [(equal? (current-label toks1) item-label)
          (parse-list-tail item-label
                           (rest toks1)
                           (append items (list (current-lexema toks1)))
                           errors)]
         [else
          (list toks1 items (add-error errors item-label toks1))]))]
    [else (list toks items errors)]))

(define (parse-list item-label toks errors)
  (if (equal? (current-label toks) item-label)
      (parse-list-tail item-label
                       (rest toks)
                       (list (current-lexema toks))
                       errors)
      (list toks '() (add-error errors item-label toks))))

; -----------------------------------------------------------------------
; parse-states: tokens auto errors -> (list tokens auto errors)
; Parsea: 'states' ':' '{' state-id (',' state-id)* '}' ';'
; Agrega cada estado al hash auto con un hash de transiciones vacio.

(define (parse-states toks auto errors)
  (match-let* ([(list t1 e1)      (match-token "pr-states"          toks errors)]
               [(list t2 e2)      (match-token "assignation-dots"   t1   e1)]
               [(list t3 e3)      (match-token "open-braces"        t2   e2)]
               [(list t4 ids e4)  (parse-list  "state-id"           t3   e3)]
               [(list t5 e5)      (match-token "close-braces"       t4   e4)]
               [(list t6 e6)      (match-token "terminator-semicol" t5   e5)])
    (define new-auto
      (foldl (lambda (id a) (hash-set a id (hash))) auto ids))
    (list t6 new-auto e6)))

; -----------------------------------------------------------------------
; parse-alphabet: tokens auto errors -> (list tokens auto errors)
; Parsea: 'alphabet' ':' '{' string (',' string)* '}' ';'
; Guarda el alfabeto como lista de simbolos sin comillas bajo "alfabeto".

(define (parse-alphabet toks auto errors)
  (match-let* ([(list t1 e1)       (match-token "pr-alphabet"        toks errors)]
               [(list t2 e2)       (match-token "assignation-dots"   t1   e1)]
               [(list t3 e3)       (match-token "open-braces"        t2   e2)]
               [(list t4 syms e4)  (parse-list  "string"             t3   e3)]
               [(list t5 e5)       (match-token "close-braces"       t4   e4)]
               [(list t6 e6)       (match-token "terminator-semicol" t5   e5)])
    (define new-auto (hash-set auto "alfabeto" (map strip-quotes syms)))
    (list t6 new-auto e6)))

; -----------------------------------------------------------------------
; parse-start: tokens auto errors -> (list tokens auto errors)
; Parsea: 'start_state' ':' '{' state-id '}' ';'
; Guarda el estado inicial bajo la key "inicial".

; Validacion semantica: el estado inicial debe haber sido declarado en states
(define (parse-start toks auto errors)
  (match-let* ([(list t1 e1) (match-token "pr-startState"      toks errors)]
               [(list t2 e2) (match-token "assignation-dots"   t1   e1)]
               [(list t3 e3) (match-token "open-braces"        t2   e2)]
               [estado       (current-lexema t3)]
               [(list t4 e4) (match-token "state-id"           t3   e3)]
               [(list t5 e5) (match-token "close-braces"       t4   e4)]
               [(list t6 e6) (match-token "terminator-semicol" t5   e5)])
    (define e7
      (if (hash-has-key? auto estado)
          e6
          (append e6
                  (list (format "Error semantico: el estado inicial ~a no fue declarado en states"
                                estado)))))
    (define new-auto (hash-set auto "inicial" estado))
    (list t6 new-auto e7)))

; -----------------------------------------------------------------------
; parse-final: tokens auto errors -> (list tokens auto errors)
; Parsea: 'final_state' ':' '{' state-id (',' state-id)* '}' ';'
; Guarda la lista de estados finales bajo la key "finales".

; Validacion semantica: cada estado final debe haber sido declarado en states
(define (parse-final toks auto errors)
  (match-let* ([(list t1 e1)      (match-token "pr-finalState"      toks errors)]
               [(list t2 e2)      (match-token "assignation-dots"   t1   e1)]
               [(list t3 e3)      (match-token "open-braces"        t2   e2)]
               [(list t4 ids e4)  (parse-list  "state-id"           t3   e3)]
               [(list t5 e5)      (match-token "close-braces"       t4   e4)]
               [(list t6 e6)      (match-token "terminator-semicol" t5   e5)])
    (define e7
      (foldl (lambda (id errs)
               (if (hash-has-key? auto id)
                   errs
                   (append errs
                           (list (format "Error semantico: el estado final ~a no fue declarado en states"
                                         id)))))
             e6 ids))
    (define new-auto (hash-set auto "finales" ids))
    (list t6 new-auto e7)))

; -----------------------------------------------------------------------
; parse-transition: tokens auto errors -> (list tokens auto errors)
; Parsea una transicion individual:
;   state-id '->' '{' string (',' string)* '}' '->' state-id
; Para cada simbolo en la lista, registra la transicion origen->destino.

(define (parse-transition toks auto errors)
  (let ([origen (current-lexema toks)])
    (match-let* ([(list t1 e1)       (match-token "state-id"           toks errors)]
                 [(list t2 e2)       (match-token "operand-transition"  t1   e1)]
                 [(list t3 e3)       (match-token "open-braces"         t2   e2)]
                 [(list t4 syms e4)  (parse-list  "string"              t3   e3)]
                 [(list t5 e5)       (match-token "close-braces"        t4   e4)]
                 [(list t6 e6)       (match-token "operand-transition"   t5   e5)]
                 [destino            (current-lexema t6)]
                 [(list t7 e7)       (match-token "state-id"            t6   e6)])
      ; Para cada simbolo, actualiza el sub-hash de transiciones del estado origen
      (define new-auto
        (foldl (lambda (sym a)
                 (define s          (strip-quotes sym))
                 (define trans-orig (hash-ref a origen (hash)))
                 (hash-set a origen (hash-set trans-orig s destino)))
               auto syms))
      (list t7 new-auto e7))))

; parse-transitions-tail: tokens auto errors -> (list tokens auto errors)
; Parsea cero o mas transiciones adicionales: (',' Transition)*
; Se detiene cuando no hay coma seguida de state-id.
(define (parse-transitions-tail toks auto errors)
  (cond
    [(empty? toks) (list toks auto errors)]
    ; Coma seguida de state-id indica otra transicion
    [(and (equal? (current-label toks) "separator-coma")
          (not (empty? (rest toks)))
          (equal? (current-label (rest toks)) "state-id"))
     (match-let* ([(list t1 a1 e1) (parse-transition (rest toks) auto errors)])
       (parse-transitions-tail t1 a1 e1))]
    [else (list toks auto errors)]))

; parse-transitions: tokens auto errors -> (list tokens auto errors)
; Parsea: 'transitions' ':' Transition (',' Transition)* ';'

(define (parse-transitions toks auto errors)
  (match-let* ([(list t1 e1)        (match-token "pr-transitions"     toks errors)]
               [(list t2 e2)        (match-token "assignation-dots"   t1   e1)]
               [(list t3 a3 e3)     (parse-transition                 t2   auto e2)]
               [(list t4 a4 e4)     (parse-transitions-tail           t3   a3   e3)]
               [(list t5 e5)        (match-token "terminator-semicol" t4   e4)])
    (list t5 a4 e5)))

; -----------------------------------------------------------------------
; parse-check: tokens auto errors -> (list tokens auto errors)
; Parsea (opcionalmente): 'check' ':' '{' string (',' string)* '}' ';'
; Si no hay seccion check, devuelve el estado sin cambios ni error.

(define (parse-check toks auto errors)
  (if (or (empty? toks)
          (not (equal? (current-label toks) "pr-check")))
      (list toks auto errors)
      (match-let* ([(list t1 e1)       (match-token "pr-check"           toks errors)]
                   [(list t2 e2)       (match-token "assignation-dots"   t1   e1)]
                   [(list t3 e3)       (match-token "open-braces"        t2   e2)]
                   [(list t4 strs e4)  (parse-list  "string"             t3   e3)]
                   [(list t5 e5)       (match-token "close-braces"       t4   e4)]
                   [(list t6 e6)       (match-token "terminator-semicol" t5   e5)])
        (define new-auto (hash-set auto "check" strs))
        (list t6 new-auto e6))))

; -----------------------------------------------------------------------
; parse-automata: lista-tokens -> (list auto errors)
; Punto de entrada del parser. Recibe el stream de tokens ya filtrado
; (sin errores lexicos, sin comentarios) y construye el hash del automata.
; El hash resultante tiene la estructura:
;   "q0"      -> (hash "a" "q1" ...)   ; transiciones por estado
;   "inicial" -> "q0"
;   "finales" -> (list "q2")
;   "alfabeto"-> (list "a" "b" ...)
;   "check"   -> (list "'ab'" "'b'" ...)

(define (parse-automata tokens)
  (match-let* ([(list t1 a1 e1) (parse-states      tokens (hash) '())]
               [(list t2 a2 e2) (parse-alphabet    t1     a1     e1)]
               [(list t3 a3 e3) (parse-start       t2     a2     e2)]
               [(list t4 a4 e4) (parse-final       t3     a3     e3)]
               [(list t5 a5 e5) (parse-transitions t4     a4     e4)]
               [(list t6 a6 e6) (parse-check       t5     a5     e5)])
    ; Tokens sobrantes al final indican algo inesperado
    (define errores-finales
      (if (and (not (empty? t6))
               (not (equal? (current-label t6) "EOF")))
          (add-error e6 "EOF" t6)
          e6))
    (list a6 errores-finales)))
