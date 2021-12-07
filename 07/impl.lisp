(defun split-by-comma (string)
  (loop for i = 0 then (1+ j)
        as j = (position #\, string :start i)
        collect (subseq string i j)
        while j))

(defun get-input (filename)
  (with-open-file (s filename)
    (map 'list #'parse-integer (split-by-comma (read-line s)))))

(defun total-fuel-consumption (l)
  (reduce '+ l))

(defun fuel-consumption (l best-pos)
  (map 'list (lambda (x) (abs (- best-pos x))) l))

(defun fuel-to-pos-2 (num)
  (/ (+ (* num num) num) 2))

(defun total-fuel-consumption-2 (l)
  (reduce '+ (map 'list #'fuel-to-pos-2 l)))

(defun main-1 ()
  (let* ((l (get-input "input"))
         (minimum (reduce #'min l))
         (maximum (reduce #'max l)))
    (reduce #'min (loop for i from minimum to maximum
                        collect (total-fuel-consumption (fuel-consumption l i))))))

(defun main-2 ()
  (let* ((l (get-input "input"))
         (minimum (reduce #'min l))
         (maximum (reduce #'max l)))
    (reduce #'min (loop for i from minimum to maximum
                        collect (total-fuel-consumption-2 (fuel-consumption l i))))))
