;; object-serialize.lisp
#|
The MIT license.

Copyright (c) 2021 Paul L. Krueger

Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
and associated documentation files (the "Software"), to deal in the Software without restriction, 
including without limitation the rights to use, copy, modify, merge, publish, distribute, 
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial 
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT 
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#

#|
This provides a serialization capability for storing and restoring a collection of lisp objects. It is not
meant to replace databases for very large amounts of data. All data is stored in a way that is readable by
normal lisp read functions. It does have some advantages over databases in that the structures that are 
created when data is restored from disk may be arbitrarily connected, even in circular reference loops. 
Non-list bjects (e.g. class or structure instances, etc) that are eq prior to the dump will be eq in the
restored objects. Lists that are equal prior to the dump will be equal after the restore, but in general will
not be eq, even if they were so prior to the dump. Arbitrary lisp objects may be dumped. Object and structure
instances, hash-tables, arbitrary arrays, functions, etc. should all be restored correctly.

The user must specify an initial list of objects to be dumped. One useful idiom is to use the instance-tracking
class as a :metaclass for any class you define for which you want all instances to be dumped. As instances are
created they will be added to class-allocated slot. When dumping, it is only necessary to put the class into the 
list of objects that need to be dumped and then all instances will also be dumped. Note that if you then restored
from the saved file without deleting the existing instances of that class, you would duplicate all of the instances
that had been dumped. And if you want to dump all instances of all classes for which you have specified instance-tracking
as the :metaclass, then you can simply call (pk-inst-track::instance-tracking-classes) to get a list of all those
classes. If that is passed to dump-to-file as the dumpfile argument, then all instances of all those classes will
be dumped.

If you want to restrict the slots that are dumped for any class instance you can define the method slots-to-dump:
(defmethod slots-to-dump ((obj <your-class>)) ...
that returns a list of slot-names that should be dumped. 
Alternatively, if you want all slots with a few exceptions yo ucan define the method slots-to-not-dump:
(defmethod slots-to-not-dump ((obj <your-class>)) ...
that returns a list of slot-names that should not be dumped (all others WILL be dumped).

Note that when instances are restored, the initialize-instance methods for that class will NOT be run. 
So if anything additional needs to be done to set values for instance slots that are not dumped and restored, that 
should be done after the restore is completed.

You can also put individual symbol names into the list and the symbol-value of that symbol will be saved and restored
when the data is re-loaded.

|#

(defpackage :pk-serialize
  (:use :common-lisp)
  (:export
   dump-to-file
   restore-from-file
   slots-to-dump
   slots-to-not-dump))

(in-package :pk-serialize)

;;;;;;;;;;;;;;;;;;;;;;;
;;; Global vars

(defvar *debug-serialize* nil)

;;;;;;;;;;;;;;;;;;;;;;;
;;; utility methods

(defmethod class-instance-slots ((cl class))
  (remove :class (c2mop::class-slots cl) :key #'c2mop::slot-definition-allocation))

(defmethod class-class-slots ((cl class))
  (remove :instance (c2mop::class-slots cl) :key #'c2mop::slot-definition-allocation))

(defmethod all-superclasses ((cl class))
  (remove-duplicates (cons cl (mapcan #'all-superclasses (c2mop::class-direct-superclasses cl)))))

(defun pure-list-p (lst)
  (cond 
   ((null lst)
    t)
   ((consp lst)
    (pure-list-p (rest lst)))
   (t
    nil)))

(defun dotted-pair-p (c)
  (and (consp c)
       (not (consp (cdr c)))))

;; to preserve correct package on all symbols that are restored they must be fully qualified 
;; in the external file. In general printing object names will not show the package qualifier
;; so we have the name-with-package methods that create the sort of string that we need.

(defmethod name-with-package ((sym symbol))
    (format nil "~a::~a" (package-name (symbol-package sym)) (symbol-name sym)))

(defmethod name-with-package ((class class))
  (format nil "~a" (name-with-package (class-name class))))

(defmethod name-with-package ((obj standard-object))
  (name-with-package (class-name (class-of obj))))

(defmethod name-with-package ((obj structure-object))
  (name-with-package (class-name (class-of obj))))

(defmethod constructor-name-with-package ((obj function))
  (flet ((fn (func)
           (multiple-value-bind (x y name)
                                (function-lambda-expression func)
             (declare (ignore x y))
             name)))
    (format nil "#'~a" (name-with-package (fn obj)))))

(defmethod constructor-name ((obj structure-object))
  (let ((cname (class-name (class-of obj))))
    (format nil "~a::make-~a" (package-name (symbol-package cname)) cname)))

(defmethod slots-to-dump ((obj standard-object))
  ;; This method should be specialized for any class for which only some
  ;; of its slots should be dumped. It should return a list of slot-names 
  ;; to be dumped.
  )

(defmethod slots-to-not-dump ((obj standard-object))
  ;; This method should be specialized for any class for which only some
  ;; of its slots should be dumped. It should return a list of slot-names 
  ;; that should not be dumped. All others will be dumped.
  )

(defun set-slot-vals (inst slot-list slot-vals)
  ;; allow for the possibility that a slot has been deleted from the definition of inst
  ;; and throw away any previous value that was saved
  (mapcar #'(lambda (slot val)
              (when (slot-exists-p inst slot)
                (setf (slot-value inst slot) val)))
          slot-list
          slot-vals))

(defmethod array-contents ((ar array))
  ;; return a list of array-contents using row-major-aref 
  (let ((contents nil))
    (dotimes (i (array-total-size ar))
      (push (row-major-aref ar i) contents))
    (nreverse contents)))

(defun pretty-init-args (init-form &key (width 100))
  (with-output-to-string (str)
    (let ((*print-pretty* t)
          (*print-right-margin* width))
      (pprint-newline :fill str)
      (pprint-logical-block (str init-form :prefix "(:init " :suffix ")")
        (write (pprint-pop) :stream str)
        (write-char #\space str)
        (pprint-newline :fill str)
        (loop
          (pprint-exit-if-list-exhausted)
          (pprint-newline :fill str)
          (pprint-logical-block (str (pprint-pop) :prefix "( " :suffix ")")
            (loop 
              (pprint-exit-if-list-exhausted)
              (write (pprint-pop) :stream str)
              (write-char #\space str)
              (pprint-newline :fill str)))
          (write-char #\space str))))))

(let ((*high-ref* 0)
      (*init-line-width* 100)
      (*obj-ref-hash* (make-hash-table))
      (*init-form-hash* (make-hash-table))
      (*classes-needing-refs* (list (find-class 'standard-object)
                                    (find-class 'structure-object)
                                    (find-class 'hash-table)
                                    (find-class 'array)
                                    (find-class 'function)
                                    (find-class 'pathname)))
      (*classes-not-needing-refs* (list (find-class 'string)))
      (*objs* nil))

    ;; ref-hash uses objects as the key and a value that is a list of two items: ( <ref>  <init-form> )
    ;; <ref> is generated when a key is first added to the hash
    ;; <init-form> is 

  (defun init-for-dump ()
    (setf *high-ref* 0)
    (setf *obj-ref-hash* (make-hash-table))
    (setf *init-form-hash* (make-hash-table))
    (setf *objs* nil))

  (defun get-ref (obj &optional (push-to-objs t))
    (or (gethash obj *obj-ref-hash*)
        (prog1
          (setf (gethash obj *obj-ref-hash*) (read-from-string (format nil ":REF~s" (incf *high-ref*))))
          (when push-to-objs (pushnew obj *objs*)))))

  (defmethod alloc-form-for ((obj stream))
    (error "Cannot dump a stream object: ~s" obj))

  (defmethod alloc-form-for ((obj class))
    ;; This is mostly used to create initialization forms for slots that have "class allocation"
    ;; but if a slot value really is a class, then this will create a reference that can be used
    ;; to restore that slot value correctly
    (format nil "(:cls ~s ~a)" (get-ref obj) (name-with-package obj)))

  (defmethod alloc-form-for ((obj symbol))
    ;; for setting a  global variable
    (format nil "(:var ~s ~a)" (get-ref obj) (name-with-package obj)))

  (defmethod alloc-form-for ((obj standard-object))
    (format nil "(:obj ~s ~a)" (get-ref obj) (name-with-package (class-of obj))))

  (defmethod alloc-form-for ((obj structure-object))
    (format nil "(:struct ~s ~a)" (get-ref obj) (constructor-name obj)))

  (defmethod alloc-form-for ((obj pathname))
    ;; no additional init will be required
    (format nil "(:path ~s (:host ~s :device ~s :directory ~s :name ~s :type ~s :version ~s))"
            (get-ref obj) 
            (let ((host (pathname-host obj)))
              ;; in some lisps, host can be an proprietary object with an unreadable printed representaation
              ;; For now just leave :host nil to use a default, which is typically how it was created in the first place
              (if (member (type-of host) '(symbol string))
                  host
                  nil))
            (pathname-device obj)
            (pathname-directory obj)
            (pathname-name obj)
            (pathname-type obj)
            (pathname-version obj)))

  (defmethod alloc-form-for ((obj hash-table))
    (format nil "(:ht ~s (:test ~s :size ~s :rehash-size ~s :rehash-threshold ~s))"
            (get-ref obj)
            (hash-table-test obj)
            (hash-table-size obj)
            (hash-table-rehash-size obj)
            (hash-table-rehash-threshold obj)))

  (defmethod alloc-form-for ((obj array))
    (format nil "(:arr ~s (~s :element-type ~s :adjustable ~s :fill-pointer ~s))"
            (get-ref obj)
            (array-dimensions obj) 
            (array-element-type obj)
            (adjustable-array-p obj)
            (array-has-fill-pointer-p obj)))

  (defmethod alloc-form-for ((obj standard-generic-function))
    (format nil "(:func ~s ~a)" (get-ref obj) (constructor-name-with-package obj)))

  (defmethod alloc-form-for ((obj standard-method))
    (format nil "(:func ~s ~a)" (get-ref obj) (constructor-name-with-package obj)))

  (defmethod alloc-form-for ((obj function))
    (format nil "(:func ~s ~a)" (get-ref obj) (constructor-name-with-package obj)))

  (defun val-for-init (obj)
    ;; walks through the obj structure (if any) and replaces any objects needing refs 
    ;; (that do not have a superclass that does not need a ref) with their ref
    (cond ((consp obj)
           (cons (val-for-init (car obj)) (val-for-init (cdr obj))))
          ((and (intersection (all-superclasses (class-of obj))  *classes-needing-refs*)
                (null (intersection (all-superclasses (class-of obj))  *classes-not-needing-refs*)))
           (get-ref obj))
          (t ;; things like strings, symbols, numbers, or nil ...
           obj)))

  ;; Collectively the init-form-for methods replace references to standard objects an arbitrary
  ;; literal object
  (defmethod init-form-for ((obj t))
    (error "No init-form-for ~s" obj))

  (defmethod init-form-for ((obj structure-object)) 
    (let* ((obj-class (class-of obj))
           (slot-names (mapcar #'c2mop::slot-definition-name (c2mop::class-slots obj-class)))
           (vals (mapcar #'(lambda (sl)
                             (val-for-init (slot-value obj sl)))
                         slot-names)))
      (pretty-init-args (list (get-ref obj) slot-names vals) :width *init-line-width*)))

  (defmethod init-form-for ((obj class))
    ;; This will initialize any slots with "allocation: class" for this class
    (let* ((slot-names (mapcar #'c2mop::slot-definition-name (class-class-slots obj)))
           (proto (c2mop::class-prototype obj))
           (slot-vals (mapcar #'(lambda (s)
                                  (val-for-init (slot-value proto s)))
                              slot-names)))
      (if slot-names
          (pretty-init-args (list (get-ref obj) slot-names slot-vals) :width *init-line-width*)
          nil)))

  (defmethod init-form-for ((obj standard-object)) 
    (let* ((instance-slots (class-instance-slots (class-of obj)))
           (slot-names (mapcar #'c2mop::slot-definition-name 
                               (or (intersection (slots-to-dump obj) instance-slots)
                                   (set-difference instance-slots (slots-to-not-dump obj) :test #'string-equal))))
           (slot-vals (mapcar #'(lambda (s)
                                  (val-for-init (slot-value obj s)))
                              slot-names)))
      (pretty-init-args (list (get-ref obj) slot-names slot-vals) :width *init-line-width*)))

  (defmethod init-form-for ((obj symbol))
    ;; initialize a variable
    (format nil "(:init ~s ~s)" (get-ref obj) (val-for-init (symbol-value obj))))

  (defmethod init-form-for ((obj pathname))
    ;; No additional initialization required
    nil)

  (defmethod init-form-for ((obj function))
    ;; No additional initialization required
    nil)

  (defmethod init-form-for ((obj standard-generic-function))
    ;; No additional initialization required
    nil)

  (defmethod init-form-for ((obj standard-method))
    ;; No additional initialization required
    nil)

  (defmethod init-form-for ((obj hash-table))
    (multiple-value-bind (keys vals)  
                         (loop for key being the hash-key of obj using (hash-value val)
                           collect (val-for-init key) into keys
                           collect (val-for-init val) into vals
                           finally (return (values keys vals)))
      (pretty-init-args (list (get-ref obj) keys (mapcar #'val-for-init vals)) :width *init-line-width*)))
           

  (defmethod init-form-for ((obj array))
    (pretty-init-args (list (get-ref obj) (array-contents obj))))

  (defmethod dump-to-file (file-path (dump-list list))
    (init-for-dump)
    (if (probe-file file-path)
        ;; rename existing version of back up file <file-path>(n) where n increments by 1
        ;; for each new backup created
        (rename-data file-path))
    (with-open-file (df file-path :direction :output :if-exists :supersede)
      (format df "(in-package ~s)~%" (package-name *package*))
      (setf *objs* dump-list)
      (loop for obj = (pop *objs*) then (pop *objs*)
        while obj
        ;; By definition, all these objects require a reference
        ;; For eaech object output an allocation form to the file and create an init for that will be 
        ;; put into *init-form-hash* as the value of the :ref<> key
        do (let ((ref  (get-ref obj nil)))
             (when *debug-serialize* (format t "~% ref ~s for ~s" ref obj))
             (write-line (alloc-form-for obj) df)
             (setf (gethash ref *init-form-hash*) (init-form-for obj))))
      ;; now write out all of the init forms that have been stuck into *ref-init-hash*
      (maphash #'(lambda (k v)
                   (declare (ignore k))
                   (when v
                     (write-line v df)))
               *init-form-hash*)))

)

(let ((*ref-hash* (make-hash-table)))

  ;; replace-refs-in methods are used to replace reference keywords that are part of init forms
  ;; with actual values. So the initform is read from the file, the ref keywords are replaced
  ;; and then the initform is eval'ed.
  ;; Since init forms are always hierarchies of lists, we only need to look for other sub-lists
  ;; or keywords within each list or sub-list.

  (defun init-for-restore ()
    (setf *ref-hash* (make-hash-table)))

  (defmethod replace-refs-in ((obj t))
    ;; For everything not a list or keyword just return the value itself
    obj)

  (defmethod replace-refs-in ((obj cons))
    ;; walk through the list to get values
    (cons (replace-refs-in (car obj))
          (replace-refs-in (cdr obj))))

  (defmethod replace-refs-in ((obj symbol))
    (let ((sname (symbol-name obj)))
      (if (and (keywordp obj)
               (> (length sname) 3)
               (string= sname "REF" :start1 0 :end1 3))
          (gethash obj *ref-hash*)
          obj)))

  (defmethod init ((obj structure-object) &rest args)
     (apply #'set-slot-vals obj args))

  (defmethod init ((obj class) &rest args)
     (apply #'set-slot-vals (c2mop::class-prototype obj) args))

  (defmethod init ((obj standard-object) &rest args)
     (apply #'set-slot-vals obj args))

  (defmethod init ((obj symbol) &rest args)
     (setf (symbol-value obj) (first args)))

  (defmethod init ((ht hash-table) &rest args)
    (mapcar #'(lambda (k v)
                (setf (gethash k ht) v))
            (first args) (second args)))

  (defmethod init ((ar array) &rest args)
    (loop 
      for i from 0 below (array-total-size ar)
      as v in (first args)
      do (setf (row-major-aref ar i) v)))

  (defmethod init ((obj null) &rest args)
    (declare (ignore obj args))
    ;; this might happen if a class that was previously dumped is no longer defined and we
    ;; wish to ignore it.
    nil)

  (defmethod restore-from-file ((path pathname))
    (init-for-restore)
    (with-open-file (rf path)
      (let* ((first-line (read-line rf nil nil))
             ;; make sure the package we are in when restoring is the same one used when dumping
             ;; so that symbol names are qualified properly by package
             (pkg-name (and (> (length first-line) 12)
                            (or (string= "(in-package " first-line :end2 12)
                                (error "File ~s is not a valid dump file " path))
                            (second (read-from-string first-line))))
             ;;bind *package* for the duration of he restore
             (*package* (or (find-package pkg-name)
                            (error "Cannot load ~s because package ~s does not exist" path pkg-name))))
        (loop
          for form = (read rf nil :eof)
          until (eql form :eof)
          for type = (first form) then (first form)
          if (eq type :init) ;; handle initialization forms
          do (let ((init-form (replace-refs-in (rest form))))
               (when *debug-serialize* (format t "~%init-form: ~s" init-form))
               (apply #'init init-form))
          else ;; handle allocation forms
          do (let ((ref  (second form))
                   (hash-val (case type
                               (:cls (find-class (third form) nil)) ;; note this will make ref nil if class is undefined
                               (:var (third form))
                               (:obj (allocate-instance (find-class (third form))))
                               (:struct (apply (symbol-function (third form)) nil))
                               (:ht (apply #'make-hash-table (third form)))
                               (:arr (apply #'make-array (third form)))
                               (:func (eval (third form)))
                               (:path (apply #'make-pathname (third form))))))
               (when *debug-serialize* (format t "~%setting ref ~s to ~s" ref hash-val))
               (setf (gethash ref *ref-hash*) hash-val))))))

)

;;; Some utilities for keeping multiple dump and recovery files


(defun rename-data (path)
  (when (probe-file path)
    ;; add an appropriate "(n)" to the path-name
    (rename-file path (increment-paren-num path))))

(defun increment-paren-num (pth)
  (let* ((name (pathname-name pth))
         (last (1- (length name)))
         (paren-num-end (and (char= #\) (elt name last)) last))
         (paren-num-start (and paren-num-end (position #\( name :test #'char= :from-end t)))
         (paren-num (1+ (if paren-num-start
                            (read-from-string name nil 0 :start (1+ paren-num-start) :end paren-num-end)
                            0)))
         (new-name (format nil "~a(~s)" (subseq name 0 paren-num-start) paren-num))
         (new-path (make-pathname :directory (pathname-directory pth)
                                  :name new-name
                                  :type (pathname-type pth))))
    (if (probe-file new-path)
        (increment-paren-num new-path)
        new-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test functions for serialization

(defvar ++global-ser-test-var++ nil)

(defstruct test-struct
  (ts1 1)
  (ts2 2))
 
(defclass ser-test-class ()
  ((null-slot :accessor null-slot
              :initform nil)
   (array-slot :accessor array-slot
               :initform (make-array '(2 3 4)))
   (hash-slot :accessor hash-slot
              :initform (make-hash-table))
   (func-slot :accessor func-slot
              :initform #'+)
   (list-slot :accessor list-slot
              :initform '(l1 l2 l3))
   (string-slot :accessor string-slot
                :initform "test string")
   (path-slot :accessor path-slot
              :initform (make-pathname :name "test" :type "txt" :defaults (truename "~/")))
   (class-alloc-slot :accessor class-alloc-slot
                     :allocation :class
                     :initform nil)))

(defmethod initialize-instance :after ((self ser-test-class) &key &allow-other-keys)
  (pushnew self (class-alloc-slot self)))

(defun array-equal (arr1 arr2)
  (cond ((not (typep arr1 'array))
         (format t "~%~s is not an array" arr1)
         nil)
        ((not (typep arr2 'array))
         (format t "~%~s is not an array" arr2)
         nil)
        ((not (= (array-total-size arr1) (array-total-size arr2)))
         (format t "~%Mismatch: ~s total size is ~s and ~s total size is ~s" 
                 arr1 (array-total-size arr1) arr2 (array-total-size arr2))
         nil)
        (t
         (loop
           with good = t
           for i from 0 below (array-total-size arr1)
           if (not (equal (row-major-aref arr1 i) (row-major-aref arr2 i)))
           do (progn
                (format t "~%Mismatch: Array index ~s: ~s and ~s"
                        i
                        (row-major-aref arr1 i)
                        (row-major-aref arr2 i))
                (setf good nil))
           finally (return good)))))

(defun hash-equal (ht1 ht2)
  (cond ((not (typep ht1 'hash-table))
         (format t "~%~s is not a hash-table" ht1)
         nil)
        ((not (typep ht2 'hash-table))
         (format t "~%~s is not a hash-table" ht2)
         nil)
        (t
         (let ((good t))
           (maphash #'(lambda (k v)
                        (let ((val (gethash k ht2 :no-value)))
                          (when (not (equal v val))
                            (setf good nil)
                            (format t "~%Mismatch: for hash key ~s, values are ~s and ~s" k v val))))
                    ht1)
           (maphash #'(lambda (k v)
                        (let ((val (gethash k ht1 :no-value)))
                          (when (not (equal v val))
                            (setf good nil)
                            (format t "~%Mismatch: for hash key ~s, values are ~s and ~s" k v val))))
                    ht2)
           good))))
         

(defun ser-test ()
  ;; in some lisps you need to make an instance of a class before you can access its class-prototype
  ;; so just create one to avoid the problem
  (make-instance 'ser-test-class)
  ;; Re-running ser-test can leave old values so get rid of them
  (setf (class-alloc-slot (c2mop::class-prototype (find-class 'ser-test-class))) nil)

  ;; make a couple of instances of ser-test-class and set some slot values
  (let ((mismatches 0)
        (str1 (make-test-struct :ts1 "new tst1 value" :ts2 41))
        (str2 (make-test-struct :ts1 "new ts2 value"))
        (obj1 (make-instance 'ser-test-class))
        (obj2 (make-instance 'ser-test-class)))
    ;; make a list with couple of structs and set the value of ++global-ser-test-var++ to that list
    (setf ++global-ser-test-var++ (list str1 str2))
    (loop 
      for i from 0 to 1
      do (loop for j from 0 to 2
           do (loop for k from 0 to 3
                do (setf (aref (array-slot obj1) i j k) (+ i j k)))))
    (setf (gethash :tk1 (hash-slot obj2)) "hash-value for key :tk1")
    (setf (gethash 14 (hash-slot obj2)) "hash-value for key 14")
    ;;; now dump everything and restore it
    (dump-to-file "~/test.clbk" (list '++global-ser-test-var++
                                       (find-class 'ser-test-class)))
    ;; now erase everything before we restore
    (setf ++global-ser-test-var++ nil)
    (setf (class-alloc-slot obj2) nil)
    (restore-from-file (pathname "~/test.clbk"))
    ;; Then compare old and new to find problems
    (if (= (length ++global-ser-test-var++) 2)
        (let ((new-str1 (first ++global-ser-test-var++))
              (new-str2 (second ++global-ser-test-var++)))
          ;; compare the two structs
          (unless (equal (test-struct-ts1 new-str1) (test-struct-ts1 str1))
            (incf mismatches)
            (format t "~% Mismatch: test-struct-ts1 (1) new: ~s old: ~s"
                    (test-struct-ts1 new-str1)
                    (test-struct-ts1 str1)))
          (unless (equal (test-struct-ts2 new-str1) (test-struct-ts2 str1))
            (incf mismatches)
            (format t "~% Mismatch: test-struct-ts2 (1) new: ~s old: ~s"
                    (test-struct-ts2 new-str1)
                    (test-struct-ts2 str1)))
          (unless (equal (test-struct-ts1 new-str2) (test-struct-ts1 str2))
            (incf mismatches)
            (format t "~% Mismatch: test-struct-ts1 (2) new: ~s old: ~s"
                    (test-struct-ts2 new-str1)
                    (test-struct-ts2 str1)))
          (unless (equal (test-struct-ts2 new-str2) (test-struct-ts2 str2))
            (incf mismatches)
            (format t "~% Mismatch: test-struct-ts2 (2) new: ~s old: ~s"
                     (test-struct-ts2 new-str2)
                     (test-struct-ts2 str2))))
        (progn
          (incf mismatches)
          (format t "~%The value of ++global-ser-test-var++: ~s, is not a list of two items" ++global-ser-test-var++)))
    (if (= (length (class-alloc-slot obj2)) 2)
        (let* ((objs (class-alloc-slot obj2))
               ;; note that since objects are pushed onto the class-allocated-slot their order is reversed
               ;; from the order of creation. We need to be careful to compare the old and new objects correctly.
               (new-obj2 (first objs))
               (new-obj1 (second objs)))
          ;; first object compares
          (unless (equal (null-slot new-obj1) (null-slot obj1))
            (incf mismatches)
            (format t "~%Mismatch: null-slot for new object 1 (~s) and old (~s)"
                    (null-slot new-obj1)
                    (null-slot obj1)))
          (unless (array-equal (array-slot new-obj1) (array-slot obj1))
            (incf mismatches)
            (format t "~%Mismatch: array-slot for new object 1 (~s) and old (~s)"
                    (array-slot new-obj1)
                    (array-slot obj1)))
          (unless (hash-equal (hash-slot new-obj1) (hash-slot obj1))
            (incf mismatches)
            (format t "~%Mismatch: hash-slot for new object 1 (~s) and old (~s)"
                    (hash-slot new-obj1)
                    (hash-slot obj1)))
          (unless (equal (func-slot new-obj1) (func-slot obj1))
            (incf mismatches)
            (format t "~%Mismatch: func-slot for new object 1 (~s) and old (~s)"
                    (func-slot new-obj1)
                    (func-slot obj1)))
          (unless (equal (list-slot new-obj1) (list-slot obj1))
            (incf mismatches)
            (format t "~%Mismatch: list-slot for new object 1 (~s) and old (~s)"
                    (list-slot new-obj1)
                    (list-slot obj1)))
          (unless (equal (string-slot new-obj1) (string-slot obj1))
            (incf mismatches)
            (format t "~%Mismatch: string-slot for new object 1 (~s) and old (~s)"
                    (string-slot new-obj1)
                    (string-slot obj1)))
          (unless (equal (path-slot new-obj1) (path-slot obj1))
            (incf mismatches)
            (format t "~%Mismatch: path-slot for new object 1 (~s) and old (~s)"
                    (path-slot new-obj1)
                    (path-slot obj1)))
          ;; second object compares
          (unless (equal (null-slot new-obj2) (null-slot obj2))
            (incf mismatches)
            (format t "~%Mismatch: null-slot for new object 2 (~s) and old (~s)"
                    (null-slot new-obj2)
                    (null-slot obj2)))
          (unless (array-equal (array-slot new-obj2) (array-slot obj2))
            (incf mismatches)
            (format t "~%Mismatch: array-slot for new object 2 (~s) and old (~s)"
                    (array-slot new-obj2)
                    (array-slot obj2)))
          (unless (hash-equal (hash-slot new-obj2) (hash-slot obj2))
            (incf mismatches)
            (format t "~%Mismatch: hash-slot for new object 2 (~s) and old (~s)"
                    (hash-slot new-obj2)
                    (hash-slot obj2)))
          (unless (equal (func-slot new-obj2) (func-slot obj2))
            (incf mismatches)
            (format t "~%Mismatch: func-slot for new object 2 (~s) and old (~s)"
                    (func-slot new-obj2)
                    (func-slot obj2)))
          (unless (equal (list-slot new-obj2) (list-slot obj2))
            (incf mismatches)
            (format t "~%Mismatch: list-slot for new object 2 (~s) and old (~s)"
                    (list-slot new-obj2)
                    (list-slot obj2)))
          (unless (equal (string-slot new-obj2) (string-slot obj2))
            (incf mismatches)
            (format t "~%Mismatch: string-slot for new object 2 (~s) and old (~s)"
                    (string-slot new-obj2)
                    (string-slot obj2)))
          (unless (equal (path-slot new-obj2) (path-slot obj2))
            (incf mismatches)
            (format t "~%Mismatch: path-slot for new object 2 (~s) and old (~s)"
                    (path-slot new-obj2)
                    (path-slot obj2))))
        (progn
          (incf mismatches)
          (format t "~%The value of class-alloc-slot: ~s, is not a list of two items" (class-alloc-slot obj2))))
    (if (zerop mismatches)
        (format t "~%NO ERRORS")
        (format t "~%~s ERRORS FOUND" mismatches)))
  (values))
          
          
      
  
  
