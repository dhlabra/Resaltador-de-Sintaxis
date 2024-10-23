#lang racket

(require racket/string)

(define (highlight-python-code code)
  (define keywords '("def" "print" "return" "if" "else" "while" "for" "in" "import"))
  
  (define (wrap-with-span class content)
    (string-append "<span class=\"" class "\">" content "</span>"))

  (define (highlight-word word)
    (cond
      [(member word keywords) (wrap-with-span "keyword" word)]
      [(regexp-match? #px"^[0-9]+$" word) (wrap-with-span "number" word)]
      [(regexp-match? #px"^\".*\"$" word) (wrap-with-span "string" word)]
      [(regexp-match? #px"[\\+\\-\\*/=]" word) (wrap-with-span "operator" word)]
      [(regexp-match? #px"\\b[a-zA-Z_][a-zA-Z0-9_]*\\b" word) (wrap-with-span "variables" word)]
      [else (wrap-with-span "identifier" word)]))

  (define (tokenize code)
    (define token-re #px"\"(?:[^\"]|\\\\\")*\"|\\w+|\\S")
    (regexp-match* token-re code))

  ;; Procesa cada linea de manera individual
  (define highlighted-code
    (string-join
     (map (lambda (line)
            (string-join (map highlight-word (tokenize line)) " "))
          (string-split code "\n"))
     "\n"))

  highlighted-code)

;; Para leer el archivo
(define (read-file-to-string filepath)
  (with-input-from-file filepath
    (lambda ()
      (define content (port->string (current-input-port)))
      content)))

;; Lee el codigo python desde el archivo texto
(define python-file-path "codigoPython.txt")
(define python-code (read-file-to-string python-file-path))

(define highlighted-html (highlight-python-code python-code))

(define html-template
  (string-append
   "<!DOCTYPE html>\n"
   "<html lang=\"en\">\n"
   "<head>\n"
   "    <meta charset=\"UTF-8\">\n"
   "    <title>Highlighted Python Code</title>\n"
   "    <style>\n"
   "        .keyword { color: blue; }\n"
   "        .number { color: red; }\n"
   "        .string { color: green; }\n"
   "        .identifier { color: black; }\n"
   "        .operator { color: yellow; }\n"
   "        .variables { color: purple; }\n"
   "    </style>\n"
   "</head>\n"
   "<body>\n"
   "    <pre><code>"
   highlighted-html
   "</code></pre>\n"
   "</body>\n"
   "</html>"))

(with-output-to-file "highlighted_code.html"
  #:exists 'replace
  (lambda ()
    (display html-template)))