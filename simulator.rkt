#lang racket

; simulator.rkt
; Autores: Ivan Burrola, Alberto Lopez, Axel Lugo, Sebastian Viche

(provide simular-cadena simular-check)

; strip-quotes: string -> string
; Extrae el contenido entre comillas simples. Si no hay comillas, devuelve s
(define (strip-quotes s)
  (let ([result (regexp-match #rx"^'(.*)'$" s)])
    (if result (second result) s)))


; Un paso recursivo de la simulacion DFA.
; Procesa un caracter por llamada, siguiendo las transiciones del hash
; Parametros:
;   auto         - hash del automata
;   cadena       - la cadena completa a validar
;   idx          - indice del caracter actual
;   estado-actual - estado en que se encuentra el automata
;   finales      - lista de estados de aceptacion
(define (simular-paso auto cadena idx estado-actual finales)
  (cond
    ; Caso base: se proceso toda la cadena
    [(= idx (string-length cadena))
     (if (member estado-actual finales) "aceptada" "rechazada")]
    ; Caso recursivo: obtener siguiente caracter y seguir transicion
    [else
     (define c            (string (string-ref cadena idx)))
     (define trans-estado (hash-ref auto estado-actual #f))
     (cond
       ; El estado actual no tiene transiciones registradas
       [(not trans-estado) "rechazada"]
       ; Existe una transicion para este simbolo
       [(hash-has-key? trans-estado c)
        (simular-paso auto cadena (+ idx 1)
                      (hash-ref trans-estado c)
                      finales)]
       ; No hay transicion definida para este simbolo: rechazada
       [else "rechazada"])]))

; Simula el DFA sobre una cadena. Devuelve "aceptada" o "rechazada"
; Inicia desde el estado inicial definido en el hash.
(define (simular-cadena auto cadena)
  (define inicial (hash-ref auto "inicial" #f))
  (define finales (hash-ref auto "finales" '()))
  (if (not inicial)
      "rechazada"
      (simular-paso auto cadena 0 inicial finales)))

; simular-check: hash -> lista de (cadena-original resultado)
; Simula el DFA sobre todas las cadenas del campo "check"
; Cada cadena del check se guarda con comillas; se stripean para simular
; Devuelve lista de pares (string-original string-resultado)
(define (simular-check auto)
  (define cadenas (hash-ref auto "check" '()))
  (map (lambda (s)
         (define contenido (strip-quotes s))
         (list s (simular-cadena auto contenido)))
       cadenas))
