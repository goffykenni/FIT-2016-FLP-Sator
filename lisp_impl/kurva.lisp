(defun factorial(n acc)
  (if (= 0 n)
    acc
    (factorial (- n 1) (+ n acc))
) )

(defun fak(n)
  (if (= 0 n)
    1
    (+ n (fak (- n 1)))
) )

(defun factorial-opt (n &key (result 1))
    (if (= n 1)
        result
        (factorial-opt (- n 1) :result (+ result n))))

(trace fak)
(print (fak 10000))