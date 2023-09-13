#lang eopl

; Definición de una función personalizada my-car para obtener el primer elemento de una lista
(define (my-car lst)
  ; Verifica si la lista está vacía
  (if (null? lst)
      '() ; Si está vacía, devuelve una lista vacía
      (car lst))) ; Si no está vacía, devuelve el primer elemento de la lista

; Definición de una función personalizada my-cdr para obtener el resto de una lista
(define (my-cdr lst)
  ; Verifica si la lista está vacía
  (if (null? lst)
      '() ; Si está vacía, devuelve una lista vacía
      (cdr lst))) ; Si no está vacía, devuelve el resto de la lista

; Definición de la función filter-in que toma un predicado P y una lista L como argumentos
(define (filter-in P L)
  ; Definición de una función auxiliar llamada filter-helper
  (define (filter-helper P L result)
    (cond
      ; Si la lista está vacía, devuelve el resultado invertido
      [(null? L) (reverse result)]
      ; Si el primer elemento cumple con el predicado, consérvalo y sigue filtrando el resto de la lista
      [(P (my-car L)) (filter-helper P (my-cdr L) (cons (my-car L) result))]
      ; Si el primer elemento no cumple con el predicado, omítelo y sigue filtrando el resto de la lista
      [else (filter-helper P (my-cdr L) result)]))

  ; Inicializa el resultado como una lista vacía y llama a la función auxiliar
  (filter-helper P L '()))

; Ejemplos de uso de la función filter-in con diferentes predicados y listas
;(display (filter-in number? '(a 2 (1 3) b 7))) ; Debería imprimir (2 7)
;(display (filter-in symbol? '(a (b c) 17 foo))) ; Debería imprimir (a foo)
(display (filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))) ; Debería imprimir ("univalle" "racket" "flp")
