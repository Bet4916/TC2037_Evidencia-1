#lang racket

; html-generator.rkt
; Autores: Ivan Burrola, Alberto Lopez, Axel Lugo, Sebastian Viche
; Materia: Implementacion de metodos computacionales

(require "tokenizer.rkt")

(provide tokens->html-body
         make-html-page)

; css-estilos : string
; Estilos CSS para cada tipo de lexema del lenguaje.
(define css-estilos
  "
  body {
    background-color: #1e1e2e;
    color: #cdd6f4;
    font-family: 'Courier New', Courier, monospace;
    font-size: 15px;
    padding: 32px;
    line-height: 1.8;
  }

  h2 {
    color: #89b4fa;
    border-bottom: 1px solid #313244;
    padding-bottom: 8px;
    margin-top: 32px;
  }

  /* Bloque de codigo resaltado */
  .code-block {
    background-color: #181825;
    border: 1px solid #313244;
    border-radius: 8px;
    padding: 20px 28px;
    margin: 16px 0;
    white-space: pre-wrap;
    line-height: 2;
  }

  /* Palabras reservadas: states, alphabet, start_state, final_state, transitions, check */
  .keyword {
    color: #cba6f7;
    font-style: italic;
    font-weight: bold;
  }

  /* Identificadores de estado: q0, q1, q2 ... */
  .stateId {
    color: #a6e3a1;
    font-weight: bold;
  }

  /* Operador de transicion: -> */
  .transition {
    color: #89dceb;
  }

  /* Cadenas / simbolos del alfabeto: 'a', '0' ... */
  .string {
    color: #a6e3a1;
  }

  /* Asignacion: : */
  .assignation {
    color: #f38ba8;
  }

  /* Separador: , */
  .comma {
    color: #cdd6f4;
  }

  /* Terminador: ; */
  .semicolon {
    color: #f38ba8;
  }

  /* Llaves: { } */
  .braces {
    color: #fab387;
  }

  /* Comentarios: // ... */
  .comment {
    color: #6c7086;
    font-style: italic;
  }

  /* Bloques de resultado de simulacion */
  .resultado-ok {
    color: #a6e3a1;
    font-weight: bold;
  }

  .resultado-fail {
    color: #f38ba8;
    font-weight: bold;
  }

  /* Errores lexicos o sintacticos */
  .error-msg {
    color: #f38ba8;
    background-color: #3b1a1a;
    border-left: 4px solid #f38ba8;
    padding: 6px 12px;
    margin: 4px 0;
    border-radius: 4px;
  }

  .sim-block {
    background-color: #181825;
    border: 1px solid #313244;
    border-radius: 8px;
    padding: 16px 24px;
    margin: 16px 0;
  }
  ")

; tokens->html-body : lista-de-tokens -> string
(define (tokens->html-body token-stream)
  (apply string-append (map styler token-stream)))


; errores->html : lista-de-strings -> string
(define (errores->html errores)
  (apply string-append
         (map (lambda (e)
                (string-append "<div class='error-msg'>" e "</div>\n"))
              errores)))


; resultados->html : lista de (cadena . resultado) -> string
(define (resultados->html resultados)
  (apply string-append
         (map (lambda (par)
                (let* ([cadena   (first par)]
                       [resultado (second par)]
                       [clase    (if (equal? resultado "aceptada")
                                    "resultado-ok"
                                    "resultado-fail")]
                       [icono    (if (equal? resultado "aceptada") "✓" "✗")])
                  (string-append
                   "<div><span class='string'>" cadena "</span>"
                   "  &rarr;  "
                   "<span class='" clase "'>" icono " " resultado "</span></div>\n")))
              resultados)))


; make-html-page : string lista lista lista -> string
; Genera la pagina HTML completa de salida.

(define (make-html-page token-stream errores-lex errores-sin sim-resultados imagen-b64)

  (define cuerpo-codigo  (tokens->html-body token-stream))
  (define cuerpo-errlex  (errores->html errores-lex))
  (define cuerpo-errsin  (errores->html errores-sin))
  (define cuerpo-sim     (resultados->html sim-resultados))

  ; Seccion de errores
  (define seccion-errores-lex
    (if (null? errores-lex)
        ""
        (string-append
         "<h2>Errores Lexicos</h2>\n"
         "<div class='sim-block'>\n"
         cuerpo-errlex
         "</div>\n")))

  (define seccion-errores-sin
    (if (null? errores-sin)
        ""
        (string-append
         "<h2>Errores Sintacticos</h2>\n"
         "<div class='sim-block'>\n"
         cuerpo-errsin
         "</div>\n")))

  ; Seccion de simulacion 
  (define seccion-sim
    (if (null? sim-resultados)
        ""
        (string-append
         "<h2>Resultados de Simulacion (check)</h2>\n"
         "<div class='sim-block'>\n"
         cuerpo-sim
         "</div>\n")))

  ; Seccion de imagen del automata
  (define seccion-imagen
    (if (string=? imagen-b64 "")
        ""
        (string-append
         "<h2>Diagrama del Automata</h2>\n"
         "<div class='sim-block'>\n"
         "<img src='data:image/png;base64," imagen-b64 "' "
         "style='max-width:100%; border-radius:6px;'>\n"
         "</div>\n")))

  ; HTML completo
  (string-append
   "<!DOCTYPE html>\n"
   "<html lang='es'>\n"
   "<head>\n"
   "  <meta charset='UTF-8'>\n"
   "  <title>DFA Language — Resultado</title>\n"
   "  <style>\n"
   css-estilos
   "  </style>\n"
   "</head>\n"
   "<body>\n"
   "<h2>Codigo Fuente (resaltado)</h2>\n"
   "<div class='code-block'>\n"
   cuerpo-codigo
   "\n</div>\n"
   seccion-errores-lex
   seccion-errores-sin
   seccion-sim
   seccion-imagen
   "</body>\n"
   "</html>\n"))
