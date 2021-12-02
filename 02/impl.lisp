(defparameter *depth* 0)
(defparameter *pos* 0)
(defparameter *forward-str* "forward")
(defparameter *up-str* "up")
(defparameter *down-str* "down")

(defun init ()
  (setf *depth* 0)
  (setf *pos* 0))

(defun calc-result ()
  (* *depth* *pos*))

(defun find-command (command)
  (cond ((search *up-str* command) 'UP)
        ((search *down-str* command) 'DOWN)
        ((search *forward-str* command) 'FORWARD)
        (t' 'UNKOWN)))

(defun handle-pos (l)
  (let ((command (car l))
        (val (cdr l)))
    (case command
      ((UP) (setf *depth* (- *depth* val)))
      ((DOWN) (setf *depth* (+ *depth* val)))
      ((FORWARD) (setf *pos* (+ *pos* val))))
    l))

(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (subseq string i j)
        while j))

(defun handle-line (l)
  (cons (find-command (car l)) (parse-integer (cadr l))))

(defun read-file-as-lines (filename)
  (with-open-file (input-file filename :direction :input)
    (loop for line = (read-line input-file nil nil)
          while line
          collect (handle-pos (handle-line (split-by-one-space line))))))

(defun main-1 (filename s)
  (init)
  (read-file-as-lines filename)
  (format s "Result is ~d" (calc-result)))
