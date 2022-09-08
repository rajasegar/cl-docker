# cl-docker
![Build and Deploy](https://github.com/rajasegar/cl-docker/workflows/CI/badge.svg)
[![Quicklisp](http://quickdocs.org/badge/cl-docker.svg)](http://quickdocs.org/cl-docker/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

Run your docker commands from your favorite Common Lisp REPLs


## Installation
```lisp
(ql:quickload :cl-docker)
```

## Usage

```lisp
(ql:quickload :cl-docker)
;; List all runnning containers
(docker:ps)
;; List all containers
(docker:ps :a)
```

This package uses `uiop:run-program` to run docker commands in the REPL


This package is inspired by [cl-virtualbox](https://github.com/eudoxia0/cl-virtualbox) by [Fernando Borretti](https://github.com/eudoxia0)

