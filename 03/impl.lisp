(defun to-number (x)
  (parse-integer (coerce x 'string) :radix 2))

(defun calc-result (arg1 arg2)
  (* (to-number arg1) (to-number arg2)))

(defun split-into-parts (s)
  (coerce s 'list))

(defun handle-file (filename)
  (with-open-file (input-file filename :direction :input)
    (loop for line = (read-line input-file nil nil)
          while line
          collect (split-into-parts line))))

(defun get-all-of-pos (l idx)
  (map 'list (lambda (x) (nth idx x)) l))

(defun decide-gamma (l)
  (let ((zeros (count #\0 l))
        (ones (count #\1 l)))
    (if (< ones zeros)
        #\0
        #\1)))

(defun get-gamma (l)
  (loop for n from 0 to (- (length (car l)) 1)
        collect (decide-gamma (get-all-of-pos l n))))

(defun invert-pos (x)
  (if (eq x #\0)
      #\1
      #\0))

(defun oxygen-scrubber-rating (l idx)
  (let* ((len (length l))
         (vals-at-pos (map 'list (lambda (x) (nth idx x)) l))
         (zeros (count #\0 vals-at-pos))
         (ones (count #\1 vals-at-pos)))
    (if (or (every (lambda (x) (eq x NIL)) vals-at-pos) (eq len 1))
        (car l)
        (if (< ones zeros)
            (oxygen-scrubber-rating (remove-if-not (lambda (x) (eq (nth idx x) #\1)) l) (1+ idx))
            (oxygen-scrubber-rating (remove-if-not (lambda (x) (eq (nth idx x) #\0)) l) (1+ idx))))))

(defun co2-scrubber-rating (l idx)
  (let* ((len (length l))
         (vals-at-pos (map 'list (lambda (x) (nth idx x)) l))
         (zeros (count #\0 vals-at-pos))
         (ones (count #\1 vals-at-pos)))
    (if (or (every (lambda (x) (eq x NIL)) vals-at-pos) (eq len 1))
        (car l)
        (if (> zeros ones)
            (co2-scrubber-rating (remove-if-not (lambda (x) (eq (nth idx x) #\0)) l) (1+ idx))
            (co2-scrubber-rating (remove-if-not (lambda (x) (eq (nth idx x) #\1)) l) (1+ idx))))))

(defun main ()
  (let* ((l (handle-file "input"))
         (gamma (get-gamma l))
         (epsilon (map 'list 'invert-pos gamma))
         (oxygen (oxygen-scrubber-rating l 0))
         (co2 (co2-scrubber-rating l 0)))
    (format *standard-output* "res (1) is: ~d" (calc-result gamma epsilon))
    (fresh-line)
    (format *standard-output* "res (2) is: ~d" (calc-result co2 oxygen))))
