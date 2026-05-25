#lang racket

; draw-graph-modulo.rkt
; Genera el diagrama de estados del automata a partir del hash del automata

(require racket/system)
(require racket/runtime-path)
(require net/base64)

(provide genera-img)
(define-runtime-path here ".")
(current-directory here)


; estado->dot-nodo : string lista-de-strings -> string
; Genera la declaracion de un nodo (estado)
(define (estado->dot-nodo estado finales)
  (define forma
    (if (member estado finales)
        "doublecircle"
        "circle"))
  (string-append
   "  " estado " [shape=" forma "];\n"))


; transicion->dot-arista : string string lista-de-simbolos -> string
; Genera una linea para la arista entre dos estados,

(define (transicion->dot-arista origen destino simbolos)
  (define etiqueta (string-join simbolos ", "))
  (string-append
   "  " origen " -> " destino
   " [label=\"" etiqueta "\"];\n"))


; ¡
; agrupar-por-destino : hash-de-transiciones -> lista de (destino . simbolos)
; agrupa los simbolos que van al mismo destino segun el HASH
(define (agrupar-por-destino trans-hash)
  (define pares (hash->list trans-hash))  ; lista de (simbolo . destino)
  ; Plegado funcional: acumula un hash de (destino -> lista-simbolos)
  (define agrupado
    (foldl (lambda (par acc)
             (define simbolo (car par))
             (define destino (cdr par))
             (define previos (hash-ref acc destino '()))
             (hash-set acc destino (append previos (list simbolo))))
           (hash)
           pares))
  (hash->list agrupado))

; estado->dot-aristas : string hash -> string
(define (estado->dot-aristas estado trans-hash)
  (define grupos (agrupar-por-destino trans-hash))
  (apply string-append
         (map (lambda (par)
                (define destino  (car par))
                (define simbolos (cdr par))
                (transicion->dot-arista estado destino simbolos))
              grupos)))

; Extrae solo las keys que son estados (excluye "inicial" y "finales")
(define (auto-hash->estados auto)
  (filter (lambda (k)
            (and (not (equal? k "inicial"))
                 (not (equal? k "finales"))))
          (hash-keys auto)))


; Genera el contenido completo del archivo .dot para Graphviz
; Estructura esperada del hash:
;   (hash "q0" (hash "a" "q1")
;         "q1" (hash "b" "q2")
;         "inicial"  "q0"
;         "finales"  (list "q2"))
(define (genera-dot auto)
  (define estados (auto-hash->estados auto))
  (define inicial (hash-ref auto "inicial" "q0"))
  (define finales (hash-ref auto "finales" '()))

  ; Nodos de los estados
  (define lineas-nodos
    (apply string-append
           (map (lambda (e) (estado->dot-nodo e finales))
                estados)))

  ; Aristas de las transiciones
  (define lineas-aristas
    (apply string-append
           (map (lambda (e)
                  (define trans (hash-ref auto e (hash)))
                  (estado->dot-aristas e trans))
                estados)))

  ; Nodo invisible de inicio (flecha de Start)
  (define linea-start
    (string-append
     "  __start [shape=plaintext label=\"\"];\n"
     "  __start -> " inicial ";\n"))

  (string-append
   "digraph DFA {\n"
   "  rankdir=LR;\n"
   "  node [fontname=\"Courier\" fontsize=12];\n"
   "  edge [fontname=\"Courier\" fontsize=11];\n"
   linea-start
   lineas-nodos
   lineas-aristas
   "}\n"))

; genera-img : hash -> string

(define (genera-img auto)
  (with-handlers
    ; Si algo falla (graphviz no instalado, etc.) devuelve "" sin crashear
    ([exn:fail?
      (lambda (e)
        (displayln (format "Aviso: no se pudo generar imagen del automata: ~a"
                           (exn-message e)))
        "")])
    (parameterize ([current-directory here])
      (define dot-text (genera-dot auto))

      ; Escribe el archivo .dot
      (call-with-output-file "dfa.dot"
        #:exists 'replace
        (lambda (out) (display dot-text out)))

      ; Llama a Graphviz para generar PNG
      (define exito (system "dot -Tpng dfa.dot -o dfa.png"))

      (if exito
          ; Lee y codifica en base64
          (bytes->string/latin-1
           (base64-encode (file->bytes "dfa.png")))
          (begin
            (displayln "Aviso: el comando 'dot' fallo. Verifica que Graphviz este instalado.")
            "")))))
