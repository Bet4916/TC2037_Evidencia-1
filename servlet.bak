#lang racket

; servlet.rkt
; Servidor web que expone el interprete del lenguaje DFA como una aplicacion HTTP.
;
; Sigue el patron del esqueleto servlet-imagen.rkt de la maestra:
;   GET  /  -> sirve index.html
;   POST /  -> recibe JSON {input, test}, retorna JSON {resultado, imagen}
;
; Pipeline de procesamiento:
;   tokenize  ->  validar lex  ->  parse-automata  ->  simular  ->  genera-img
;
; Si hay errores lexicos:    HTML con codigo resaltado + errores, sin continuar.
; Si hay errores sintacticos: HTML con codigo resaltado + errores, sin continuar.
; Si todo OK:                HTML con codigo + simulaciones + imagen base64.
;
; Autores: Ivan Burrola, Alberto Lopez, Axel Lugo, Sebastian Viche
; Materia: Implementacion de metodos computacionales

(require web-server/servlet
         web-server/servlet-env
         json
         racket/runtime-path)

(require "tokenizer.rkt"
         "desc-rec.rkt"
         "simulator.rkt"
         "html-generator.rkt"
         "draw-graph-modulo.rkt")

; Directorio del archivo servlet.rkt (para encontrar index.html junto a el)
(define-runtime-path here ".")


; -----------------------------------------------------------------------
; process-input: string string -> (values string string)
;
; Ejecuta el pipeline completo sobre el texto del editor.
;
; Parametros:
;   input    - contenido del textarea del editor (codigo DFA)
;   test-str - cadenas extra del campo de prueba en la UI (una por linea)
;
; Devuelve:
;   resultado-html - fragmento HTML para inyectar en el div de salida
;   imagen-b64     - imagen PNG del automata en base64, o "" si falla/hay errores
(define (process-input input test-str)

  ; 1. Tokenizar
  (define tokens      (tokenize input))
  (define errores-lex (extrae-errores-lex tokens))
  (define tok-clean   (tokens-sin-comentarios (tokens-sin-errores tokens)))

  (cond

    ; ---- Hay errores lexicos: mostrar codigo + errores, parar ----
    [(not (null? errores-lex))
     (values (make-result-content tokens errores-lex '() '())
             "")]

    ; ---- Sin errores lexicos: intentar parsear ----
    [else
     (define parse-result  (parse-automata tok-clean))
     (define auto          (first  parse-result))
     (define errores-sin   (second parse-result))

     (cond

       ; ---- Hay errores sintacticos: mostrar codigo + errores, parar ----
       [(not (null? errores-sin))
        (values (make-result-content tokens '() errores-sin '())
                "")]

       ; ---- Todo bien: simular y graficar ----
       [else
        ; Cadenas del bloque check: en el codigo fuente
        (define resultados-check (simular-check auto))

        ; Cadenas del campo "test" del HTML (una por linea, sin comillas)
        (define test-cadenas
          (if (string=? (string-trim test-str) "")
              '()
              (filter (lambda (s) (not (string=? (string-trim s) "")))
                      (regexp-split #rx"\r?\n" test-str))))

        (define resultados-test
          (map (lambda (c) (list c (simular-cadena auto (string-trim c))))
               test-cadenas))

        ; Combinar ambas listas de resultados
        (define resultados-todos (append resultados-check resultados-test))

        ; Generar imagen del automata (puede devolver "" si Graphviz no esta)
        (define imagen (genera-img auto))

        (values (make-result-content tokens '() '() resultados-todos)
                imagen)])]))


; -----------------------------------------------------------------------
; start: request -> response
; Dispatcher principal del servlet.
(define (start req)
  (define method (request-method req))

  (cond

    ; ---- GET: servir index.html ----
    [(equal? method #"GET")
     (define path (build-path here "index.html"))
     (if (file-exists? path)
         (response/full
          200 #"OK"
          (current-seconds)
          #"text/html; charset=utf-8"
          '()
          (list (file->bytes path)))
         (response/full
          404 #"Not Found"
          (current-seconds)
          #"text/plain"
          '()
          (list #"index.html no encontrado. Asegurate de que esta en la misma carpeta que servlet.rkt")))]

    ; ---- POST: procesar codigo DFA, devolver JSON ----
    [(equal? method #"POST")
     (with-handlers
       ([exn:fail?
         (lambda (e)
           (response/full
            500 #"Internal Server Error"
            (current-seconds)
            #"application/json"
            '()
            (list (jsexpr->bytes
                   (hash 'resultado
                         (string-append
                          "<div class='error-msg'>Error interno del servidor: "
                          (exn-message e)
                          "</div>")
                         'imagen "")))))])
       (define raw-body (request-post-data/raw req))
       (define body-str (if raw-body (bytes->string/utf-8 raw-body) "{}"))
       (define data     (string->jsexpr body-str))

       ; Extraer campos del JSON; usar "" como default si no vienen
       (define input    (let ([v (hash-ref data 'input #f)])
                          (if (string? v) v "")))
       (define test-str (let ([v (hash-ref data 'test #f)])
                          (if (string? v) v "")))

       (define-values (resultado imagen) (process-input input test-str))

       (response/full
        200 #"OK"
        (current-seconds)
        #"application/json; charset=utf-8"
        '()
        (list (jsexpr->bytes (hash 'resultado resultado 'imagen imagen)))))]

    ; ---- Otro metodo: 405 ----
    [else
     (response/full
      405 #"Method Not Allowed"
      (current-seconds)
      #"text/plain"
      '()
      (list #"Metodo no permitido"))]))


; -----------------------------------------------------------------------
; Arrancar el servidor
; #:servlet-regexp #rx"" hace que el servlet maneje TODAS las rutas
(serve/servlet
 start
 #:launch-browser? #t
 #:servlet-path    "/"
 #:servlet-regexp  #rx""
 #:port            8000)
