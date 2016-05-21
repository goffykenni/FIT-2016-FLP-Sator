(defparameter *forbidden-chars* (make-hash-table))

; Overi, jestli je retez palindromem, bez potreby obracet list.
; Vraci cons bunku jejiz car cast je T/nil podle toho, jestli je retez
; palindrom a cdr je delka retezce (spocetla se piggybackingem)
(defun isPalindrome (lst)
  (defun srch (slow fast acc elemCount)
    (cond 
      ( (null fast) (cons (cmpLists acc slow) (* 2 elemCount)) )
      ( (null (cdr fast)) (cons (cmpLists acc (cdr slow)) (+ 1 (* 2 elemCount))) ) 
      ( T ( srch (cdr slow) (cdr (cdr fast)) (cons (car slow) acc) (+ 1 elemCount) ))
) )
  (srch lst lst nil 0)
)

; Porovna dva seznamy prvek po prvku
(defun cmpLists (lstA lstB)
  (cond 
    ( (null lstA) (null lstB) )
    ( (null lstB) (null lstA) )
    ( (equalp (car lstA) (car lstB)) (cmpLists (cdr lstA) (cdr lstB)))
    ( T nil )
) )

; Vezme seznam a vytvori z nej matici (seznam radkovych vektoru - seznamu),
; ktera bude mit zadany pocet sloupecku. V pripade, ze delku vstupniho seznamu
; nelze celociselne delit pozadovanym poctem sloupecku, pak budou na zacatek prvniho
; radku pripnuty hodnoty NIL.
; @param lst - vstupni seznam
; @param colCount - pozadovany pocet prvku matice
; @param keepReversed - Pokud true, pak se bude seznam cist pozpatku, jinak 
;       normalne od predu (a zavola se pred vracenim reverse)
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
        

; Linearizuje transpozici dane matice, tzn. vezme vstupni matici a
; vrati list prvku pruchodem matice po sloupcich.
; @param m - Matice prvku zadana jako seznam radkovych "vektoru", tj. seznam seznamu.
;       Obecne neni nutne, aby radky meli stejny pocet prvku.
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

; Posloupnosti testu rozhodne, zda dany retezec reprezentovany jako seznam
; jednotlivych znaku, vyhovuje podminkam zadani.
; @param lst - Jiz odfiltrovany seznam znaku v reteci
(defun sator (lst)
  ((lambda (pal)
    ((lambda (gridsize)
      (if (and
            ; Vstup je palindrom
            (not (null (car pal)))
            ; Da se presne vepsat do ctverce
            (= (square gridsize) (cdr pal))
            ; Transponovany vstup se cte stejne jako puvodni retez
            (cmpLists lst (transpose (toMatrix lst (cdr pal) T) T))
          )      
        gridsize
        0
      )
    ) (isqrt (cdr pal)) ) ; Velikost mrizky
  ) (isPalindrome lst) ) ; Je vstup palindrom
)
    

(defun square (n)
  (* n n))

; Otevre soubour o nemz predpoklada format pozadovany zadanim a pak
; ho v souladu s timto formatem cte radek po radku.
; Nejdriv precte pocet radku a pak na kazdy radek aplikuje algoritmus.
(defun get-file (filename)
(let ((in (open filename :if-does-not-exist nil)))
  (when in
    (let ( (cnt (parse-integer (read-line in nil 0))) )
      (loop for i from 1 to cnt
        do (format T "test ~A: ~A~%" i
          (sator (filter-input (str-to-lst (read-line in nil)) *forbidden-chars* )))
    ) )
         
    (close in)
  )
))
  
; Nacte retezec do seznamu znaku
(defun str-to-lst (str)
  (with-input-from-string (s str)
    (loop for ch = (read-char s nil)
      while ch collect ch)
) )

; Filtruje vstupni seznam znaku podle hashovaci tabulky
; zakazanych znaku.
(defun filter-input (lst h)
  (defun filter (lst acc)
    (if (null lst)
      (reverse acc)
      (if (gethash (car lst) h)
        (filter (cdr lst) acc)
        (filter (cdr lst) (cons (car lst) acc))
      )
) ) (filter lst nil) )

; Naplni hashovaci tabulku znaky v seznamu zakazanych znaku
(defun fill-hash (h forbidden-list)
  (if (not (null forbidden-list))
    (progn 
      (setf (gethash (car forbidden-list) h) T)
      (fill-hash h (cdr forbidden-list))
) ) )
          



(fill-hash *forbidden-chars* '(#\Space #\, #\. #\! #\( #\)))
(get-file "input.txt")

  ; Over jestli je to klasicka palindrom
  ; Pouzij zjistenou delku a zjiti, jestli je to ctverec
  ; Chapej lst jako ctvercovou matici a cti ji po sloupeckach
  ; Zjisti, jestli takto ctena je taky palindrom
  ; Porovnej puvodni a transponovanou

;(print (isPalindrome '(s a t o r a r e p o t e n e t o p e r a r o t a s)))
;(print (cmpLists '(1 2 3) '(1 2 3)))
;(print (transpose '( (1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)) nil nil ))
;(print (transpose (toMatrix '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) 4 T) T))
;(print (toMatrix '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) 4 nil))
;(print (sator '(s a t o r a r e p o t e n e t o p e r a r o t a s)))