(require "psmd-dir")
(defpackage :string-table
  (:nicknames :st)
  (:use :common-lisp)
  (:export :uuid-string))
(in-package :string-table)
(defvar *table*
  (with-open-file (in (psmd-dir:path-to #P"data/string-table.dat"))
    (with-standard-io-syntax
      (read in))))
(defun uuid-string (uuid)
  "Get the string from the specified UUID"
  (gethash uuid *table*))
