; Linearizuje transpozici dane matice, tzn. vezme vstupni matici a
; vrati list prvku pruchodem matice po sloupcich.
; @param m - Matice prvku zadana jako seznam radkovych "vektoru", tj. seznam seznamu.
; @param keepReversed - Pokud je true, vrati linearizaci obracenou (od konce), pokud
;       false, tak linearizaci jeste pred vracenim otoci zavolanim reverse.
(defun transpose (m keepReversed)

  (defun transpose2 (m stripped acc)
    (if (null m) ; Dosli jsme nakonec sloupecku?
      (if (null stripped) ; Byl toto uz posledni sloupecek?
        acc
        (transpose2 (reverse stripped) nil acc) ; Zacni cist dalsi sloupecek
      )
      (if (listp (car m))
        (if (null (cdr (car m))) ; Ma radek vic nez jeden prvek?
          ; Nema, tak uz nic nepridame do stripped
          (transpose2 (cdr m) stripped (cons (car (car m)) acc))
          ; Ma, pridame zbytek radku do stripped
          (transpose2 (cdr m) (cons (cdr (car m)) stripped) (cons (car (car m)) acc))
        )
        nil
      )
  ) )
  
  (if (equalp keepReversed T)
    (transpose2 m nil nil)
    (reverse (transpose2 m nil nil))
  )
)

;
(defun toMatrix (lst colCount keepReversed)
  
  (defun work (lst i n acc)
    (if (null lst)
      (cons (prependNil (car acc) i n) (cdr acc))
      (if (= i n) ; Docetl se radek?
        (work lst 0 n (cons nil acc))
        (work (cdr lst) (+ 1 i) n (cons ( cons (car lst) (car acc) ) (cdr acc)) )
      )
  ) )
  
  (defun prependNil (acc i n)
    (if (= i n)
      acc
      (prependNil (cons nil acc) (+ 1 i) n)
  ) )
  
  (if (equalp keepReversed T)
    (work lst 0 colCount nil)
    (work (reverse lst) 0 colCount nil)    
  )
)

(print (toMatrix '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) 3 T))