; Pro dany seznam vrati podposloupnost skladajici se
; z kazdeho k-teho prvku puvodniho seznamu, pricemz "offset" je nastaven
; tak, aby uplne prvni prvek seznamu byl povazovan za kty

(defun kth (k lst)
  (if (null lst)
    nil
    (cons (car lst) (kth k (nthcdr k lst)))
) )

(print (kth 3 '(1 2 3 4 5 6 7 8 9)))

; Totez, ale tail-recursive. Potrebuje to otacet.
(defun kthTail (k lst acc)
  (if (null lst)
    (reverse acc)
    (kthTail k (nthcdr k lst) (cons (car lst) acc))
) )
(print (kthTail 3 '(1 2 3 4 5 6 7 8 9) nil))

; Opet funkce kth, ale tentokrat vraci jak seznam k-tych prvku,
; tak zbytek seznamu s odstranenymi k-tymi polozkami
(defun kthKeep (k lst)
  (defun compute (i lst lstSelect lstRest)
    (if (null lst)
      (cons lstSelect lstRest)
      (if (= i k)
        (compute 1 (cdr lst) (cons (car lst) lstSelect) lstRest)
        (compute (+ 1 i) (cdr lst) lstSelect (cons (car lst) lstRest))
) ) )
    (compute k lst nil nil))
    
(print (kthKeep 3 '(1 2 3 4 5 6 7)))

;; Preusporada prvky v seznamu tak, aby za sebou sly vzdy ty, ktere maji
; stejny zbytek po deleni k.. rostoucne

    