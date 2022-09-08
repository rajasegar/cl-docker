# cl-docker
![Build and Deploy](https://github.com/rajasegar/cl-docker/workflows/CI/badge.svg)
[![Quicklisp](http://quickdocs.org/badge/cl-docker.svg)](http://quickdocs.org/cl-docker/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

Run your docker commands from your favorite Common Lisp REPLs

![Screenshot](/screenshot.png)


## Installation
```lisp
(ql:quickload :cl-docker)
```

## Usage

```lisp
;; List all docker containers
(docker:ps :a)
```

### Output:
```
"CONTAINER ID   IMAGE               COMMAND                  CREATED         STATUS                    PORTS     NAMES
673b20976166   docker101tutorial   \"/docker-entrypoint.…\"   19 months ago   Exited (0) 16 hours ago             docker-tutorial
"
NIL
0
```


This package uses `uiop:run-program` to run docker commands in the REPL


This package is inspired by [cl-virtualbox](https://github.com/eudoxia0/cl-virtualbox) by [Fernando Borretti](https://github.com/eudoxia0)

