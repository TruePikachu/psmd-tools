(defpackage :memfile
  (:nicknames :mf)
  (:use :common-lisp)
  (:export :memfile
           :chunk-mem :data-le :get-size :load-mem :save-mem))
(in-package :memfile)

(defgeneric chunk-mem (memfile position length)
            (:documentation
              "Fetch a chunk of the memfile into a new memfile.
              Returns new memfile and new position"))
(defgeneric data-le (memfile position length)
            (:documentation
              "Fetch a chunk of LE data.
              Returns data and new position"))
(defgeneric get-size (memfile)
            (:documentation
              "Get the size of loaded memfile data"))
(defgeneric load-mem (stream &optional length)
            (:documentation
              "Load a memfile with stream data"))
(defgeneric save-mem (memfile stream)
            (:documentation
              "Save the memfile to the specified stream"))

(defclass memfile ()
  ((data :documentation "Data array"
         :type array))
  (:documentation "Class for in-memory files"))

(defmethod chunk-mem ((this memfile) position length)
  (assert (<= length (get-size this)))
  (let ((result (make-instance 'memfile :length length)))
    (loop for d from 0 to (1- length)
          for s = position then (1+ s)
          do (setf (aref (slot-value result 'data) d)
                   (aref (slot-value this 'data) s)))
    (values result (+ position length))))
(defmethod data-le ((this memfile) position length)
  (values
    (loop for n from 0 to (1- length)
          for s = position then (1+ s)
          for shift = 0 then (+ shift 8)
          summing (ash (aref (slot-value this 'data) s) shift))
    (+ position length)))
(defmethod get-size ((this memfile))
  (array-dimension (slot-value this 'data) 0))
(defmethod initialize-instance ((this memfile) &key length)
  (setf (slot-value this 'data) (make-array (list length)
                                            :element-type '(unsigned-byte 8)
                                            :initial-element 0)))
(defmethod load-mem (stream &optional length)
  (let* ((length (if length
                   length
                   (file-length stream)))
         (result (make-instance 'memfile :length length)))
    (read-sequence (slot-value result 'data) stream)
    result))
(defmethod print-object ((this memfile) strm)
  (print-unreadable-object (this strm :type t :identity t)
    (format strm "0x~X bytes" (get-size this))))
(defmethod save-mem ((this memfile) stream)
  (write-sequence (slot-value this 'data) stream))
