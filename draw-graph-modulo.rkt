#lang racket

; draw-graph-modulo.rkt
; Dependencia externa: Graphviz (comando 'dot') debe estar instalado.
; Autores: Ivan Burrola, Alberto Lopez, Axel Lugo, Sebastian Viche


(require racket/system)
(require racket/runtime-path)
(require net/base64)

(provide genera-img)

; Establece el directorio de trabajo como el directorio del modulo
(define-runtime-path here ".")
(current-directory here)



; Genera la declaracion DOT de un nodo (estado).
; Si el estado es final, se dibuja con doublecircle; si no, con circle.
;
; Parametros:
;   estado  - nombre del estado, ej. "q0"
;   finales - lista de estados finales, ej. '("q2" "q3")
(define (estado->dot-nodo estado finales)
  (define forma
    (if (member estado finales)
        "doublecircle"
        "circle"))
  (string-append
   "  " estado " [shape=" forma "];\n"))



; Genera una linea DOT para la arista entre dos estados,
; agrupando todos los simbolos que van del mismo origen al mismo destino.
; Parametros:
;   origen   - estado de origen, ej. "q0"
;   destino  - estado de destino, ej. "q1"
;   simbolos - lista de strings con los simbolos, ej. '("a" "b")
(define (transicion->dot-arista origen destino simbolos)
  (define etiqueta (string-join simbolos ", "))
  (string-append
   "  " origen " -> " destino
   " [label=\"" etiqueta "\"];\n"))



; agrupar-por-destino : hash-de-transiciones -> lista de (destino . simbolos)
; Dado el hash de transiciones de UN estado (ej. (hash "a" "q1" "b" "q1")),
; agrupa los simbolos que van al mismo destino.
; Devuelve lista de pares (destino lista-de-simbolos)
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
  (hash->list agrupado))  ; devuelve lista de (destino . lista-simbolos)


; Genera todas las aristas DOT que salen de un estado dado.
;
; Parametros:
;   estado     - nombre del estado origen
;   trans-hash - hash de transiciones de ese estado (simbolo -> destino)
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
                 (not (equal? k "finales"))
                 (not (equal? k "alfabeto"))
                 (not (equal? k "check"))))
          (hash-keys auto)))



; Genera el contenido completo del archivo .dot para Graphviz
; a partir del hash del automata construido por el parser.
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


; Genera la imagen PNG del automata y la devuelve codificada en base64.
; Si Graphviz no esta instalado, devuelve string vacio y muestra aviso.
;
; Parametros:
;   auto - hash del automata (mismo formato que genera-dot espera)
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
