#lang racket

; html-generator.rkt
; Genera la representacion HTML del resultado: codigo resaltado,
; errores lexicos/sintacticos y resultados de simulacion.
;
; Patron central del esqueleto (maestra):
;   (apply string-append (map styler token-stream))
;
; Provee dos variantes de salida:
;   make-result-content -> contenido para el div del servlet (sin CSS)
;   make-html-page      -> pagina HTML autonoma con CSS incluido
;
; Autores: Ivan Burrola, Alberto Lopez, Axel Lugo, Sebastian Viche
; Materia: Implementacion de metodos computacionales

(require "tokenizer.rkt")

(provide tokens->html-body
         make-result-content
         make-html-page
         css-estilos)

; -----------------------------------------------------------------------
; css-estilos: string
; Estilos CSS para todos los tipos de lexema.
; Las clases deben coincidir exactamente con las que genera styler.
(define css-estilos
  "
  .code-block {
    background: #12121e; border: 1px solid #313244; border-radius: 6px;
    padding: 16px 20px; margin: 10px 0; line-height: 2.2;
    overflow-x: auto;
  }
  .sim-block {
    background: #12121e; border: 1px solid #313244; border-radius: 6px;
    padding: 14px 20px; margin: 10px 0;
  }
  h2 {
    color: #89b4fa; font-size: 0.85rem; text-transform: uppercase;
    letter-spacing: 0.08em; border-bottom: 1px solid #313244;
    padding-bottom: 6px; margin: 20px 0 10px 0;
  }
  h2:first-child { margin-top: 0; }
  .keyword     { color: #cba6f7; font-style: italic; font-weight: bold; }
  .stateId     { color: #a6e3a1; font-weight: bold; }
  .transition  { color: #89dceb; }
  .string      { color: #a6e3a1; }
  .assignation { color: #f38ba8; }
  .comma       { color: #cdd6f4; }
  .semicolon   { color: #f38ba8; }
  .braces      { color: #fab387; }
  .comment     { color: #6c7086; font-style: italic; }
  .lex-error   { color: #f38ba8; background: #3b1a1a;
                 border-radius: 2px; padding: 0 3px; }
  .resultado-ok   { color: #a6e3a1; font-weight: bold; }
  .resultado-fail { color: #f38ba8; font-weight: bold; }
  .error-msg {
    color: #f38ba8; background: #3b1a1a; border-left: 4px solid #f38ba8;
    padding: 6px 12px; margin: 4px 0; border-radius: 4px; font-size: 13px;
  }
  ")

; -----------------------------------------------------------------------
; tokens->html-body: lista-tokens -> string
; Patron del esqueleto: map styler sobre el stream, luego apply string-append.
; Es la operacion central de resaltado de sintaxis.
(define (tokens->html-body token-stream)
  (apply string-append (map styler token-stream)))

; -----------------------------------------------------------------------
; errores->html: lista-strings -> string
; Convierte cada mensaje de error en un div con estilo.
(define (errores->html errores)
  (apply string-append
         (map (lambda (e)
                (string-append "<div class='error-msg'>" e "</div>\n"))
              errores)))

; -----------------------------------------------------------------------
; resultados->html: lista de (cadena resultado) -> string
; Genera una linea por cada cadena simulada con su resultado y simbolo.
(define (resultados->html resultados)
  (apply string-append
         (map (lambda (par)
                (let* ([cadena    (first  par)]
                       [resultado (second par)]
                       [clase     (if (equal? resultado "aceptada")
                                     "resultado-ok" "resultado-fail")]
                       [icono     (if (equal? resultado "aceptada") "&#10003;" "&#10007;")])
                  (format
                   "<div><span class='string'>~a</span> &rarr; <span class='~a'>~a ~a</span></div>\n"
                   cadena clase icono resultado)))
              resultados)))

; -----------------------------------------------------------------------
; make-result-content: tokens errlex errsin resultados -> string
; Genera el contenido HTML para el div de salida del servlet.
; No incluye <html>, <head> ni CSS (eso va en index.html).
(define (make-result-content token-stream errores-lex errores-sin resultados)
  (string-append
   "<h2>Codigo resaltado</h2>"
   "<div class='code-block'>"  (tokens->html-body token-stream) "</div>"
   (if (null? errores-lex) ""
       (string-append
        "<h2>&#9888; Errores Lexicos</h2>"
        "<div class='sim-block'>" (errores->html errores-lex) "</div>"))
   (if (null? errores-sin) ""
       (string-append
        "<h2>&#9888; Errores Sintacticos</h2>"
        "<div class='sim-block'>" (errores->html errores-sin) "</div>"))
   (if (null? resultados) ""
       (string-append
        "<h2>&#9654; Resultados de Simulacion</h2>"
        "<div class='sim-block'>" (resultados->html resultados) "</div>"))))

; -----------------------------------------------------------------------
; make-html-page: tokens errlex errsin resultados imagen-b64 -> string
; Genera una pagina HTML completa y autonoma (con CSS incluido).
; Util para generar un archivo .html de salida independiente.
(define (make-html-page token-stream errores-lex errores-sin resultados imagen-b64)
  (string-append
   "<!DOCTYPE html>\n<html lang='es'>\n<head>\n"
   "<meta charset='UTF-8'>\n"
   "<title>DFA Language — Resultado</title>\n"
   "<style>\n"
   "body { background:#1e1e2e; color:#cdd6f4;"
   " font-family:'Courier New',monospace; padding:32px; line-height:1.8; }\n"
   "h1 { color:#89b4fa; margin-bottom:20px; }\n"
   css-estilos
   "</style>\n</head>\n<body>\n"
   "<h1>// DFA Language Interpreter</h1>"
   (make-result-content token-stream errores-lex errores-sin resultados)
   (if (string=? imagen-b64 "") ""
       (string-append
        "<h2>Diagrama del Automata</h2>"
        "<div class='sim-block'>"
        "<img src='data:image/png;base64," imagen-b64
        "' style='max-width:100%;border-radius:6px;'>"
        "</div>"))
   "\n</body>\n</html>\n"))
