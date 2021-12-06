(defun read-input (filename)
  (with-open-file (f filename :direction :input)
    (map
     'list
     (lambda (x) (- (char-int x) (char-int #\0))) (remove-if-not #'digit-char-p
                                                                 (coerce (read-line f nil nil) 'list)))))

(defun count-fish (l)
  (loop for i from 0 to 8
        collect (cons i (count-if (lambda (x) (= x i)) l))))

(defun get-input (filename)
  (count-fish (read-input filename)))

(defun simulate-step (fish)
  (let* ((curr (car fish))
         (next (1- curr))
         (num (cdr fish)))
    (if (= curr 0)
        `((6 . ,num) (8 . ,num))
        `((,next . ,num)))))

(defun merge-2 (x y)
  (let ((i (car x))
        (num (+ (cdr x) (cdr y))))
    `(,i . ,num)))

(defun remove-if-not-key (l key)
  (remove-if-not (lambda (x) (= (car x) key)) l))

(defun get-by-key (l key)
  (let* ((ll (remove-if-not-key l key))
         (s (length ll)))
    (case s
      ((2) (merge-2 (car ll) (car (cdr ll))))
      ((1) (car ll))
      (otherwise nil))))

(defun merge-into (l)
  (remove-if (lambda (x) (equal x nil)) (loop for i from 0 to 8
                                              collect (get-by-key l i))))

(defun simulate (l days)
  (if (= days 0)
      l
      (simulate (merge-into (reduce #'append (map 'list #'simulate-step l))) (1- days))))

(defun count-fish-result (fish-list)
  (reduce #'+ (mapcar #'cdr fish-list)))

(defun main-1 ()
  (count-fish-result (simulate (get-input "input") 80)))

(defun main-2 ()
  (count-fish-result (simulate (get-input "input") 256)))
