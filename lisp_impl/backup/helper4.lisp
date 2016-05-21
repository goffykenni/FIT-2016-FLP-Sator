(defun shuffle (lst act next)
  (if (null lst)
    next ; ??
    (if (null act) ; Bylo dokonceno kolecko
      (shuffle lst (reverse next) nil)
      (shuffle (cdr lst) (cdr act) (cons (cons (car lst) (car act)) next) )
) ) )
    
(defun initEmpty (n)
  (if (= 0 n)
    nil
    (cons () (initEmpty (- n 1)))
) )

(defun deepReverse (lst)
  (defun work (lst acc)
    (if (null lst)
      acc
      (if (listp (car lst))
        (work (cdr lst) (cons (deepReverse (car lst)) acc))
        (work (cdr lst) (cons (car lst) acc))
      )
) ) (work lst nil) )
        

(print (shuffle (reverse '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)) (initEmpty 4) nil))

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



(print (isPalindrome '(s a t o r a r e p o t e n e t o p e r a r o t a s)))
(print (cmpLists '(1 2 3) '(1 2 3)))
(print (deepReverse '(1 2 (3 4 (1 2 3)) 5)))