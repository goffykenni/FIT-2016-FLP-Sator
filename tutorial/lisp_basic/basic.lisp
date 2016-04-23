(defun factorial (N)
  (if (= N 1)
    1
    (* N (factorial (- N 1)))
) )

; Computes the n-th triangle number
(defun triangle (n)
  (if (= n 1)
    1
    (+ n (triangle (- n 1)))
) )

; Checks whether given element is in the given list.
; Return a list whose car is that element or nil
; if the element is not in the list
(defun ismem (elem lst)
  (cond ((null lst) nil)
        ((eql elem (car lst)) lst)
        (T (ismem elem (cdr lst)))
) )

; Swaps the first and the second element in the given list
(defun swapFirstTwo (lst)
  (cond ((null lst) nil)
        ((null (cdr lst)) lst)
        (T (cons (car (cdr lst)) (cons (car lst) (cddr lst))))
) )
        

(print (swapFirstTwo '(1 2 3 4 5)))