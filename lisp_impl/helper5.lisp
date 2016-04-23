; Overi, jestli je retez palindromem, bez potreby obraces list.
; Vraci seznam, jehoz prvni prvek je T/nil podle toho, jestli je retez
; palindrom a druhy je delka retezce (spocetla se piggybackingem)
(defun isPalindrome (lst)
  (defun srch (slow fast acc elemCount)
    (cond 
      ( (null fast) (list (cmpLists acc slow) (* 2 elemCount)) )
      ( (null (cdr fast)) (list (cmpLists acc (cdr slow)) (+ 1 (* 2 elemCount))) ) 
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

(defun toMatrix (lst colCount keepReversed)
  (defun initEmpty (n)
    (if (= 0 n)
      nil
      (cons () (initEmpty (- n 1)))
  ) )
  
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
        

; Vezme matici a projde ji po sloupcich a vrati seznam
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

(defun sator (lst)
  (setf pal (isPalindrome lst))
  (if (and
      ; Vstup je palindrom
      (not (null (car pal))) 
      ; Da se presne vepsat do ctverce
      (= ( square (isqrt (car (cdr pal))) ) (car (cdr pal)) )
      ; Transponovany vstup je taky palindrom a cte se stejne jako puvodni retez
      (let ( (trans (transpose (toMatrix lst (car (cdr pal)) T) T)) )
        (and (equalp T (car (isPalindrome trans))) (cmpLists lst trans)) 
      ))
    (isqrt (car (cdr pal)))
    0
) )
    

(defun square (n)
  (* n n))

(defun get-file (filename)
(let ((in (open filename :if-does-not-exist nil)))
  (when in
    (let ( (cnt (parse-integer (read-line in nil 0))) )
      (loop for i from 1 to cnt
        do (format T "test ~A: ~A~%" i
          (sator (filter-input (str-to-lst (read-line in nil)) *forbidden-chars* )))
    ))
         
    (close in)
  )
))
          
(defun str-to-lst (str)
  (with-input-from-string (s str)
    (loop for ch = (read-char s nil)
      while ch collect ch)
) )

(defun filter-input (lst h)
  (defun work (lst acc)
    (if (null lst)
      (reverse acc)
      (if (gethash (car lst) h)
        (work (cdr lst) acc)
        (work (cdr lst) (cons (car lst) acc))
      )
) ) (work lst nil) )

(defun fill-hash (h forbidden-list)
  (if (not (null forbidden-list))
    (progn 
      (setf (gethash (car forbidden-list) h) T)
      (fill-hash h (cdr forbidden-list))
) ) )
          


(defparameter *forbidden-chars* (make-hash-table))
(fill-hash *forbidden-chars* '(#\Space #\, #\. #\! #\( #\)))
(get-file "loki.txt")

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