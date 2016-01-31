#!/usr/bin/sbcl --script
(require "asdf")
(require "bin-offsets")
(require "memfile")
(require "psmd-dir")
(require "string-table")
(use-package :memfile)
(defstruct item-data
  unk_00
  cost-buy
  value-sell
  unk_06
  unk_08
  unk_0A
  unk_0C
  unk_10
  unk_14
  unk_18
  unk_1C
  unk_20)
(defvar *item-data-info*
  (psmd-dir:memfile-of #P"romfs/item_data_info.bin"))
(defvar *n-items* 894) ; TODO File actually goes beyond this point, what's there?
(defvar *the-data*
  (loop for pos = #x0 then (+ pos #x24)
        for i from 0 to (1- *n-items*)
        collect (make-item-data
                  :unk_00 (data-le *item-data-info* (+ pos #x00) #x2)
                  :cost-buy (data-le *item-data-info* (+ pos #x02) #x2)
                  :value-sell (data-le *item-data-info* (+ pos #x04) #x2)
                  :unk_06 (data-le *item-data-info* (+ pos #x06) #x2)
                  :unk_08 (data-le *item-data-info* (+ pos #x08) #x2)
                  :unk_0A (data-le *item-data-info* (+ pos #x0A) #x2)
                  :unk_0C (data-le *item-data-info* (+ pos #x0C) #x4)
                  :unk_10 (data-le *item-data-info* (+ pos #x10) #x4)
                  :unk_14 (data-le *item-data-info* (+ pos #x14) #x4)
                  :unk_18 (data-le *item-data-info* (+ pos #x18) #x4)
                  :unk_1C (data-le *item-data-info* (+ pos #x1C) #x4)
                  :unk_20 (data-le *item-data-info* (+ pos #x20) #x4))))
(defvar *name-lookup*
  (let ((memfile
          (psmd-dir:memfile-of #P"exefs/code.bin")))
    (loop for i from 0 to (1- *n-items*)
          for pos = (bo:offset :item-uuid-list) then (+ pos #x4)
          collect (data-le memfile pos #x4))))
(loop for i = 0 then (1+ i)
      for elem in *the-data*
      do (format t "[~3D] 0x~4,'0X ~5DP ~5DP ~{0x~4,'0X~#^ ~} ~{0x~8,'0X~#^ ~} (~A)~%"
                 i
                 (item-data-unk_00 elem)
                 (item-data-cost-buy elem)
                 (item-data-value-sell elem)
                 (list (item-data-unk_06 elem)
                       (item-data-unk_08 elem)
                       (item-data-unk_0A elem))
                 (list (item-data-unk_0C elem)
                       (item-data-unk_10 elem)
                       (item-data-unk_14 elem)
                       (item-data-unk_18 elem)
                       (item-data-unk_1C elem)
                       (item-data-unk_20 elem))
                 (st:uuid-string (nth i *name-lookup*))))
