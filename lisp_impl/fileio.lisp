(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (str-to-lst line)) ))
          
(defun str-to-lst (str)
  (with-input-from-string (s str)
    (loop for ch = (read-char s nil)
      while ch collect ch)
) )



(let ((in (open "loki.txt" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
         while line do (format t "~a~%" line))
    (close in)))