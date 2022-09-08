# cl-docker
Run your docker commands from your favorite Common Lisp REPLs

## Usage

```lisp
(ql:quickload :cl-docker)
;; List all runnning containers
(docker:ps)
;; List all containers
(docker:ps :a)
```

## Installation
```lisp
(ql:quickload :cl-docker)
```

