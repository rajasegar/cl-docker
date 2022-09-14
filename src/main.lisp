(defpackage cl-docker
	(:nicknames :docker)
  (:use :cl)
	(:export  :build
						:commit
						:cp
						:diff
						:docker-load
						:exec
						:image-prune
						:images
						:inspekt
						:port
						:ps
						:pull
						:rename
						:rm
						:run
						:save
						:start
						:stats
						:stop
						:version))
(in-package :cl-docker)

(defparameter *docker-program* "docker")
(defconstant +CELL-FORMATS+ '(:left   "~vA"
                              :center "~v:@<~A~>"
                              :right  "~v@A"))

;; (defconstant +IMAGES+
;; 	'(("minimum_app" "latest" "2342342342" "6 days ago" "784MB")
;; 		("my-sbcl-app" "latest" "2342342342" "7 days ago" "874MB")))

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
                               collect (getf +CELL-FORMATS+ align))))
              (widths  (loop for w across col-widths collect w)))
            ; write each line to the given stream
            (dolist (row strtable)
                (apply #'format stream row-fmt (mapcan #'list widths row)))))
)

(defun cmd (command-format &rest args)
  (concatenate 'string *docker-program* " "
               (apply #'format (append (list nil command-format)
                                       args))))

(defun run-cmd (command &key (dry nil))
	(if dry
			 command
	(uiop:run-program command :output :string)))

(defun docker-url (url)
	"Get complete url of the API endpoint"
	(concatenate 'string "http://localhost/v1.41" url))

(defun docker-api (url)
	(let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
		(sb-bsd-sockets:socket-connect socket "/var/run/docker.sock")
		(let ((stream (sb-bsd-sockets:socket-make-stream socket
																										 :element-type '(unsigned-byte 8)
																										 :input t
																										 :output t
																										 :buffering :none)))
			(let ((wrapped-stream (flexi-streams:make-flexi-stream (drakma::make-chunked-stream stream)
																														 :external-format :utf-8)))
				(yason:parse (dex:get (docker-url url) :stream wrapped-stream :want-stream t) :object-as :alist)))))

(defun ps (&optional all)
	"Show a list of containers"
	(run-cmd (if all
							 (cmd "ps -a")
							 (cmd "ps"))))

(defun assoc-cdr (prop list)
	(cdr (assoc prop list :test #'string=)))

(defun get-created (time)
	(multiple-value-bind
				(second minute hour day month year day-of-week dst-p tz)
			(decode-universal-time time)
		(format nil "Created ~d months ~d days ago" month day)))

(defun format-size (size)
	(multiple-value-bind (q r)
			(ceiling size (* 1000 1000))
		(if (> q 999)
				(multiple-value-bind (q1 r1) (floor q 1000) (format nil "~dGB" q1))
				(format nil "~dMB" q))))

(defun images ()
	"Show a list of all images"
	;; (run-cmd (cmd "images"))

	(format-table t 
	(loop for image in (docker-api "/images/json")
				for repo-tags = (cl-ppcre:split ":" (car (assoc-cdr "RepoTags" image)))
				for repo = (car repo-tags)
				for tag = (car (cdr repo-tags))
				for id = (assoc-cdr "Id" image)
				for size = (format-size (assoc-cdr "Size" image))
				for created = (get-created (assoc-cdr "Created" image))

				collect `(,repo ,tag "xxxxxxxx" ,created ,size)
				)
								:column-label '("REPOSITORY" "TAG" "IMAGE ID" "CREATED" "SIZE")
								:column-align '(:left :left :left :left :left))
	)

(defun image-prune (&optional a)
	"Delete dangling images"
	(run-cmd (if a
							 (cmd "image prune -a")
							 (cmd "image prune"))))

(defun build (dir)
	"Build an image from a Dockerfile"
	(run-cmd (cmd "build ~A" dir)))

(defun tag (image tag)
	"Tag an image"
	(run-cmd (cmd "tag ~A ~A" image tag)))

(defun save (image file)
	"Save an image to file"
	(run-cmd (cmd "save ~A > ~A" image file)))

(defun docker-load (file)
	"Load an image from a file"
	(run-cmd (cmd "load -i ~A" file)))

(defun stats ()
	"Show stats of running containers"
	(run-cmd (cmd "stats")))

(defun version ()
	"Show installed docker version"
	(run-cmd (cmd "version")))

(defun rm (container &optional f)
	"Delete a running container"
	(run-cmd (cmd "rm ~A" container (if f "-f" ""))))

(defun run (image &key publish-all rm interactive tty detach name publish)
	"Start a new container from an image"
	(run-cmd (cmd "run ~A ~A ~A ~A ~A ~A ~A ~A"
								(if interactive "-i" "")
								(if tty "-t" "")
								(if rm "--rm" "")
								(if publish (format nil "-p ~S" publish) "")
								(if name (format nil "--name ~A" name) "")
								(if publish-all "-P" "")
								(if detach "-d" "")
								image) :dry t))

(defun inspekt (container)
	"Get detailed info about an object"
	(run-cmd (cmd "inspect ~A" container)))

(defun pull (image)
	"Pull an image/repo from a registry"
	(run-cmd (cmd "pull ~A" image)))

(defun exec (container command &optional detach interactive tty)
	"Run commands in a container"
	(run-cmd (cmd "exec ~A ~A"
								 container
								 command
								 (if detach "-d" "")
								 (if interactive "-i" "")
								 (if tty "-t" ""))))

(defun start (container)
	"Start a container"
	(run-cmd (cmd "start ~A" container)))

(defun stop (container)
	"stop a container"
	(run-cmd (cmd "stop ~A" container)))

(defun cp (source target)
	"Copy a file from a container to the host and vice versa"
	(run-cmd (cmd "cp ~A ~A" source target)))

(defun rename (old new)
	"Rename a container"
	(run-cmd (cmd "rename ~A ~A" old new)))

(defun commit (container)
	"Create an image out of container"
	(run-cmd (cmd "commit ~A" container)))

(defun diff (container)
	"Show all modified files in a container"
	(run-cmd (cmd "diff ~A" container)))

(defun port (container)
	"Show mapped ports of a container"
	(run-cmd (cmd "port ~A" container)))



