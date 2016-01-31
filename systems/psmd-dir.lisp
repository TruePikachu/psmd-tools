(require "memfile")
(defpackage :psmd-dir
  (:use :common-lisp)
  (:export :path-to :memfile-of))
(in-package :psmd-dir)
(defvar *memfile-hash* (make-hash-table :test #'equal))
(defvar *repo-root* #P"/psmd/") ; TODO This should be automatically determined
(defun path-to (repopath)
  "Convert the repo-based path to the filesystem-based path"
  (merge-pathnames repopath *repo-root*))
(defun memfile-of (repopath)
  "Get the memfile of the specified *repo* path"
  (let ((result (gethash repopath *memfile-hash*)))
    (unless result
      (setf result
            (with-open-file (in (path-to repopath)
                                :element-type '(unsigned-byte 8))
              (mf:load-mem in)))
      (setf (gethash repopath *memfile-hash*) result))
    result))
