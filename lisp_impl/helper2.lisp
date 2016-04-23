(defun kthTail (k lst acc)
  (if (null lst)
    acc
    (kthTail k (nthcdr k lst) (cons (car lst) acc))
) )

; Preusporada seznam tak, aby po sobe sli prvky s indexy, jez po deleni
; k davaji stejny zbytek
(defun rearrange (k lst)
  (defun work (i lst acc)
    (if (= i k)
      acc
      (work (+ 1 i) (cdr lst) (kthTail k lst acc))
  ) )
  (work 0 lst nil)
)

(print (reverse(rearrange 3 '(1 2 3 4 5 6 7 8))))

; Working, but how abou the number of required steps?
; For lst of size 10000 and k = 100 (limit value for sator), this
; requires 100 traverse the whole lst, yielding 1 000 000 steps.
; Then the final reverse, but that only requires 10000 steps.

; Can we do better?
; In fact, we can. Knowing k, we can preallocate a list of k empty lists.
; Then we can iterate through lst spreading its elements to appropriate
; lists. This only requires one iteration over lst, that is 10000 steps.
; Then, we have to flatten our list of lists, which would take another 10000 steps.
; So in total, it needs 20000 computing steps + the steps required for prealocation.
; Wow.. much better, hey?

(defun initBuckets(n) 
  (if (= n 0)
    nil
    (cons () (initBuckets (- n 1)))
) )

(defun spread(lst buckets current)
  (if (null lst)
    buckets
    ( (
) )

(print (initBuckets 4))



