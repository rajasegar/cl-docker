(defpackage :cl-docker.utils
	(:use :cl)
	(:export :file-size-human-readable
					:format-table))

(in-package :cl-docker.utils)

(defvar *CELL-FORMATS* '(:left   "~vA"
                              :center "~v:@<~A~>"
                              :right  "~v@A"))

;; Copied from https://gist.github.com/WetHat/a49e6f2140b401a190d45d31e052af8f
(defun format-table (stream data &key (column-label (loop for i from 1 to (length (car data))
                                                          collect (format nil "COL~D" i)))
                                   (column-align (loop for i from 1 to (length (car data))
                                                       collect :left)))
  (let* ((col-count (length column-label))
         (strtable  (cons column-label ; table header
                          (loop for row in data ; table body with all cells as strings
																collect (loop for cell in row
																							collect (if (stringp cell)
																													cell
																				;else
																													(format nil "~A" cell))))))
         (col-widths (loop with widths = (make-array col-count :initial-element 0)
                           for row in strtable
                           do (loop for cell in row
                                    for i from 0
																		do (setf (aref widths i)
																						 (max (aref widths i) (length cell))))
                           finally (return widths))))
		;------------------------------------------------------------------------------------
		; splice in the header separator
    (setq strtable
          (nconc (list (car strtable) ; table header
                       (loop for align in column-align ; generate separator
                             for width across col-widths
                             collect (case align
                                       (:left   (format nil ":~v@{~A~:*~}"
                                                        (1- width)  "-"))
                                       (:right  (format nil "~v@{~A~:*~}:"
                                                        (1- width)  "-"))
                                       (:center (format nil ":~v@{~A~:*~}:"
                                                        (- width 2) "-")))))
                 (cdr strtable))) ; table body
		;------------------------------------------------------------------------------------
		; Generate the formatted table
    (let ((row-fmt (format nil "| ~{~A~^ | ~} |~~%" ; compile the row format
                           (loop for align in column-align
																 collect (getf *CELL-FORMATS* align))))
          (widths  (loop for w across col-widths collect w)))
																				; write each line to the given stream
      (dolist (row strtable)
        (apply #'format stream row-fmt (mapcan #'list widths row)))))
	)

;; Shamelessly copied and modified from
;; https://www.reddit.com/r/lisp/comments/c3nfzo/humanreadable_file_size_in_lisp/
;; This is mostly inspired by Emacs 26.2.
(defun file-size-human-readable (file-size &optional flavor)
  "Produce a string showing FILE-SIZE in human-readable form.

Optional second argument FLAVOR controls the units and the display format:

 If FLAVOR is nil or omitted, each kilobyte is 1024 bytes and the produced
    suffixes are \"k\", \"M\", \"G\", \"T\", etc.
 If FLAVOR is `si', each kilobyte is 1000 bytes and the produced suffixes
    are \"k\", \"M\", \"G\", \"T\", etc.
 If FLAVOR is `iec', each kilobyte is 1024 bytes and the produced suffixes
    are \"KiB\", \"MiB\", \"GiB\", \"TiB\", etc."
  (let ((power (if (or (null flavor) (eq flavor 'iec))
                   1024.0
                   1000.0))
        (post-fixes
          ;; none, kilo, mega, giga, tera, peta, exa, zetta, yotta
          (list "" "KB" "MB" "GB" "TB" "PB" "EB" "ZB" "YB"))
        (format-string "~d~a~a"))
    (loop while (and (>= file-size power) (rest post-fixes))
          do (setf file-size (/ file-size power)
                   post-fixes (rest post-fixes)))
    (if (> (abs (- file-size (round file-size))) 0.05)
        (setf format-string "~,1f~a~a")
        (setf file-size (round file-size)))
    (format nil format-string
            file-size
            (if (and (eq flavor 'iec) (string= (first post-fixes) "k"))
                "K"
                (first post-fixes))
            (cond
              ((and (eq flavor 'iec)
                    (string= (first post-fixes) ""))
               "B")
              ((eq flavor 'iec) "iB")
              (t "")))))

