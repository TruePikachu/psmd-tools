#!/usr/bin/sbcl --script
(require "asdf")
(require "farc")
(require "memfile")
(use-package :memfile)
(defvar *filename-map*
  (with-open-file (in "/psmd/data/string-name-map.dat")
    (with-standard-io-syntax
      (read in))))
(defvar *all-strings* (make-hash-table))
(defun do-unknown-character (memfile position)
  (let* ((base-code (data-le memfile position #x2))
         (length
           (case (ldb (byte 8 8) base-code)
             ((#xA0       ; Likely a glyph
               #xA1) 1)   ;
             ((#xA2       ; Job PkMn
               #xA3) 2)   ; Job item
             ((#xA4) 1)   ;
             ((#xA7) 2)   ;
             ((#xA8) 1)   ;
             ((#xB0) 2)   ;
             ((#xB1       ;
               #xB2) 1)   ;
             ((#xB3) 3)   ;
             ((#xB4) 1)   ;
             ((#xB5) 3)   ; Number???
             ((#xB6) 2)   ;
             ((#xB8       ;
               #xB9) 3)   ;
             ((#xBD       ;
               #xBE) 2)   ;
             ((#xBF       ;
               #xC1       ;
               #xC2       ;
               #xC3       ;
               #xC4) 1)   ;
             ((#xC5       ;
               #xC6) 2)   ;
             ((#xC7       ;
               #xC8) 1)   ;
             ((#xC9) 2)   ;
             ((#xCA) 3)   ; <a href=>
             ((#xCB       ; </a>
               #xD3       ;
               #xD5       ;
               #xD6) 1)   ;
             ((#xD7       ;
               #xD8       ;
               #xD9       ; Move (by ID?)
               #xDA       ; Type (by ID?)
               #xDD       ;
               #xDE       ; Status (by ID?)
               #xDF       ; Ability (by ID?)
               #xE0) 2)   ; Skill (by ID?)
             ((#xE4       ; Number (in code)
               #xE5       ; Number (arg?)
               #xE6) 3)   ;
             ((#xEA       ;
               #xEB) 1)   ;
             ((#xEC) 2)   ;
             ((#xEE       ;
               #xF0       ;
               #xF1) 1)   ;
             ((#xF4       ; Item from arg list? \ This might be
               #xF5       ; Item from arg list? /   a trap, IDK
               #xF6       ; Pokémon from arg list
               #xF7       ; Number from arg list (damage?)
               #xF8) 2)   ; String from arg list
             ((#xFC) 1)   ;
             ((#xFD) 2))));
    (when length
      (values
        (format nil "«~{~4,'0X~#^ ~}»"
                (loop for i from 0 to (1- length) collecting
                      (data-le memfile (+ position (* i #x2)) #x2)))
        (+ position (* length #x2))))))
(defun read-character (memfile position)
  (let ((code (data-le memfile position #x2)))
    (incf position 2)
    (case code
      ; Known values
      (#x0000 (values nil position))
      (#x0009 (values (list #\SYMBOL_FOR_HORIZONTAL_TABULATION) position))
      (#x000A (values (list #\SYMBOL_FOR_LINE_FEED) position))
      (#xA04E (values "«MIST CONTINENT»" position))
      (#xA052 (values "«ICON NRM»" position))
      (#xA053 (values "«ICON FIR»" position))
      (#xA054 (values "«ICON GRS»" position))
      (#xA055 (values "«ICON WTR»" position))
      (#xA056 (values "«ICON ELE»" position))
      (#xA057 (values "«ICON GND»" position))
      (#xA058 (values "«ICON FLY»" position))
      (#xA05E (values "«SAND CONTINENT»" position))
      (#xA05F (values "«AIR CONTINENT»" position))
      (#xA060 (values "«WATER CONTINENT»" position))
      (#xA061 (values "«GRASS CONTINENT»" position))
      (#xA063 (values "«ICON RCK»" position))
      (#xA064 (values "«ICON GHO»" position))
      (#xA065 (values "«ICON FGT»" position))
      (#xA067 (values "«ICON ICE»" position))
      (#xA068 (values "«ICON STL»" position))
      (#xA069 (values "«ICON DRG»" position))
      (#xA06A (values "«ICON DRK»" position))
      (#xA072 (values "«$»" position))
      (#xA09B (values "«(A)»" position)) ; A button
      (#xA09C (values "«(B)»" position)) ; B button
      (#xA09D (values "«(X)»" position)) ; X button
      (#xA09E (values "«(Y)»" position)) ; Y button
      (#xA09F (values "«[L]»" position)) ; L button
      (#xA0A0 (values "«[R]»" position)) ; R button
      (#xA0C2 (values "«MoveCC»" position)) ; Move Cuts Corners
      (#xD000 (values "«Team»" position))
      (#xD100 (values "«Hero»" position))
      (#xD200 (values "«Partner»" position))
      ; Default
      (otherwise
        (multiple-value-bind
          (unk-chr unk-pos) (do-unknown-character memfile (- position 2))
          (cond
            (unk-chr (values unk-chr unk-pos))
            ((or (<= #x20 code #x9FFF)
                 (<= #xFF00 code #xFFFF))
             (values (list (code-char code)) position))
            (t (values (format nil "«U+~4,'0X»" code) position))))))))
(defun read-string (memfile position)
  (let ((result (make-string 0)))
    (loop
      (multiple-value-bind (chr newpos) (read-character memfile position)
        (setf position newpos)
        (unless chr (return))
        (setf result
              (concatenate 'string result chr))))
    (if (plusp (length result))
      result
      "«NULL»")))
(defun dump-string-table (memfile)
  (let* ((result (make-hash-table))
         (ptr-data-head (data-le memfile #x4 #x4))
         (n-strings (data-le memfile (+ ptr-data-head #x0) #x4))
         (table-ptr (data-le memfile (+ ptr-data-head #x4) #x4)))
    (dotimes (i n-strings)
      (let ((entry-base (+ table-ptr (* #xC i))))
        (setf (gethash (data-le memfile (+ #x4 entry-base) #x4) result)
              (read-string memfile (data-le memfile entry-base #x4)))))
    result))
(ensure-directories-exist "/psmd/data/string-table/")
(let ((fileinfo-hash (farc:get-files
                       (with-open-file (in "/psmd/romfs/message_us.bin"
                                           :element-type '(unsigned-byte 8))
                         (mf:load-mem in)))))
  (loop for file-id being each hash-key in fileinfo-hash
        using (hash-value file-mem)
        do (with-open-file (out (concatenate 'string
                                             "/psmd/data/string-table/"
                                             (getf *filename-map* file-id))
                                :direction :output
                                :if-exists :supersede)
             (let ((table (dump-string-table file-mem)))
               (loop for str-id being each hash-key in table
                     using (hash-value str-data) do
                     (format out "0x~8,'0X ~A~%"
                             str-id str-data)
                     (setf (gethash str-id *all-strings*) str-data))))))
(with-open-file (out "/psmd/data/string-table.dat"
                     :direction :output
                     :if-exists :supersede)
  (with-standard-io-syntax
    (print *all-strings* out)))
