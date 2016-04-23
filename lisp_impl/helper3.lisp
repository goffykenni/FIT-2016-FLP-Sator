; Precti prvni sloupec dane matice
(defun firstCol (m)
  (if (null m)
    nil
    (if (listp (car m))
      (cons (car (car m)) (firstCol (cdr m)))
) ) )

(print (firstCol (list '(1 2 3 4) '(5 6 7 8) '(9 10 11 12) '(13 14 15 16))))

; Pomoci predchozi ideje se matice da transponovat, ale nebude to moc efektivni,
; protoze k ziskani k-teho sloupecku musime provest k * n kroku.. (v kazdem radku
; se nejprve musime docist na k-tou pozici, nez si vezmem ten prvek

(defun transpose (m stripped)
  (if (null m)
    (if (null stripped)
      nil
      (transpose (reverse stripped) nil)
    )
    (if (listp (car m))
      (if (null (cdr (car m)))
        (cons (car (car m)) (transpose (cdr m) stripped))
        (cons (car (car m)) (transpose (cdr m) (cons (cdr (car m)) stripped) ))
      )
) ) )

; Jeste tail recursive verze
(defun transpose2 (m stripped acc)
  (if (null m)
    (if (null stripped)
      (reverse acc)
      (transpose2 (reverse stripped) nil acc)
    )
    (if (listp (car m))
      (if (null (cdr (car m)))
        (transpose2 (cdr m) stripped (cons (car (car m)) acc))
        (transpose2 (cdr m) (cons (cdr (car m)) stripped) (cons (car (car m)) acc))
      )
    )
) )

; Stejne jako transpose2, ovsem rovnou vraci matici a ne jednorozmerny seznam
(defun transpose3 (m stripped acc)
  (if (null m)
    (if (null stripped)
      (reverse acc)
      (transpose3 (reverse stripped) nil (cons nil (cons (reverse (car acc)) (cdr acc)) ))
    )
    (if (listp (car m))
      (if (null (cdr (car m)))
        (transpose3 (cdr m) stripped (cons (cons (car (car m)) (car acc)) (cdr acc)))
        (transpose3 (cdr m) (cons (cdr (car m)) stripped) (cons (cons (car (car m)) (car acc)) (cdr acc)))
      )
    )
) )
  
 

(print (transpose3 (list '(1 2 3 4) '(5 6 7 8) '(9 10 11 12) '(13 14 15 16)) nil nil))