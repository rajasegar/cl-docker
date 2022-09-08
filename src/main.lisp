(defpackage cl-docker
	(:nicknames :docker)
  (:use :cl)
	(:export :ps
	 :images
					 :stats
	 :version
					 :rm
	 :inspekt
					 :pull
	 :exec
					 :start
	 :stop
					 :cp
	 :rename
					 :commit
	 :image-prune
					 :build
	 :diff
	 :port))
(in-package :cl-docker)

(defparameter *docker-program* "docker")

(defun cmd (command-format &rest args)
  (concatenate 'string *docker-program* " "
               (apply #'format (append (list nil command-format)
                                       args))))

(defun run-cmd (command)
	(uiop:run-program command :output :string))

(defun ps (&optional all)
	"Show a list of containers"
	(run-cmd (if all
							 (cmd "ps -a")
							 (cmd "ps"))))

(defun images ()
	(run-cmd (cmd "images")))

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

(defun stats ()
	(run-cmd (cmd "stats")))

(defun version ()
	(run-cmd (cmd "version")))

(defun rm (container &optional f)
	"Delete a running container"
	(run-cmd (cmd "rm ~A" container)))

(defun run (image)
	"Start a new container from an image"
	(run-cmd (cmd "run ~A" image)))

(defun inspekt (container)
	"Get detailed info about an object"
	(run-cmd (cmd "inspect ~A" container)))

(defun pull (image)
	"Pull an image/repo from a registry"
	(run-cmd (cmd "pull ~A" image)))

(defun exec (container command &optional detach interactive tty)
	"Run commands in a container"
	(run-cmd (cond
						 (detach (cmd "exec -d ~S ~S" container command))
						 (interactive (cmd "exec -i ~S ~S" container command))
						 (tty (cmd "exec -t ~S ~S" container command))
						 (t (cmd "exec ~A ~A" container command))
						 )))

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
