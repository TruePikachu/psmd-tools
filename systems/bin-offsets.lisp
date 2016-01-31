(require "psmd-dir")
(defpackage :rom-offsets
  (:nicknames :bo)
  (:use :common-lisp)
  (:export :offset))
(in-package :rom-offsets)
(defvar *offset-list*
  (with-open-file (in (psmd-dir:path-to #P"data/offsets.dat"))
    (with-standard-io-syntax
      (read in))))
(defun offset (name)
  "Get the offset that has the specified name keyword"
  (getf *offset-list* name))
