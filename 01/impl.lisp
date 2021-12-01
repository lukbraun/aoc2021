(defparameter *counter* 0)
(defparameter *current-number* -1)
(defparameter *increase-str* "increased")
(defparameter *decrease-str* "decreased")
(defparameter *no-change-str* "no change")

(defun init ()
  (setf *counter* 0)
  (setf *current-number* -1))

(defun read-file-as-lines (filename)
  (with-open-file (input-file filename :direction :input)
    (setf *current-number* (parse-integer (read-line input-file)))
    (loop for line = (read-line input-file nil nil)
          while line
          collect (parse-integer line))))

(defun is-higher? (n)
  (> n *current-number*))

(defun increment-counter (n)
  (if (is-higher? n)
      (+ *counter* 1)
      *counter*))

(defun print-to (l s)
  (format s "~d (~a)~%" (car l) (cadr l)))

(defun get-status-string (n)
  (cond ((eq n *current-number*) *no-change-str*)
        ((< n *current-number*) *decrease-str*)
        ((> n *current-number*) *increase-str*)))

(defun count-measurements-1 (filename s)
  (loop for m in (read-file-as-lines filename)
        while m
        collect (let* ((status (get-status-string m))
                       (combined (list m status)))
                  (setf *counter* (increment-counter m))
                  (setf *current-number* m)
                  (print-to combined s))))

(defun print-increases (s)
  (format s "Found ~d increases~%" *counter*))

(defun main-1 (s)
  (progn
    (count-measurements-1 "input" s)
    (print-increases s)))

(defun sum-next-3 (l)
  (+ (car l) (cadr l) (caddr l)))

(defun mark-increase-and-set (win s)
  (let* ((status (get-status-string win))
         (combined (list win status)))
    (setf *counter* (increment-counter win))
    (setf *current-number* win)
    (print-to combined s)))

(defun more-than-3-left? (l)
  (and (car l) (cadr l) (caddr l)))

(defun read-file-as-lines-2 (filename)
  (with-open-file (input-file filename :direction :input)
    (loop for line = (read-line input-file nil nil)
          while line
          collect (parse-integer line))))

(defun calculate-windows (l acc)
  (if (more-than-3-left? l)
      (calculate-windows (cdr l) (append acc (list (sum-next-3 l))))
      acc))

(defun count-measurements-2 (filename s)
  (loop for m in (calculate-windows (read-file-as-lines-2 filename) '())
        while m
        collect (mark-increase-and-set m s)))

(defun main-2 (s)
  (progn
    (count-measurements-2 "input" s)
    (setf *counter* (- *counter* 1))
    (print-increases s)))

(defun main ()
  (init)
  (print "============")
  (with-open-file (output-file "version_1.txt"
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (main-1 output-file))
  (print "============")
  (init)
  (with-open-file (output-file "version_2.txt"
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (main-2 output-file))
  (print "============"))
