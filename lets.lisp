(in-package :zrseriesi)

(defmacro with-series-implicit-map (&body body)
  `(compiler-let ((*series-implicit-map* 'T))
     ,@body))

(defmacro letS*-1 (binds &body body)
  (if (endp binds)
      `(progn ,@body)
      (cl:let ((bind (car binds)))
        (if (consp (car bind))
            `(zrseries::destructuring-bindS ,(car bind)
                                            ,(cadr bind)
               (letS*-1 ,(cdr binds)
                 ,@body))
            `(zrseries::let ((,(car bind) ,(cadr bind)))
               (letS*-1 ,(cdr binds)
                 ,@body))))))

(defmacro letS* (binds &body body)
  `(with-series-implicit-map
     (letS*-1 ,binds
       ,@body)))

(defmacro defunS (name (&rest lambda-list) &body body)
  (multiple-value-bind (name valnum)
                       (if (consp name)
                           (values-list name)
                           (values name 1))
    `(zrseries:defun ,name (,@lambda-list)
       (declare (optimizable-series-function ,valnum))
       ,@body)))

;; The Basic Sequence Functions

(defmacro mapS (fn &rest Zs)
  `(map-fn t ,fn ,@Zs))

;; Generators [4/4]
(defunS Gsequence (object)
  (series object))

(defunS Glist (list)
  (catenate (scan list)
            (series nil)))

(defunS Gsublist (list)
  (catenate (scan-sublists list)
            (series nil)))

(defunS Grange (&optional (first 1) (step-size 1))
  (scan-range :from first :by step-size))

;; Enumerators [8/8]
(defunS Elist (list)
  (scan 'list list))

(defunS Esublists (list)
  (scan-sublists list))

#|(defunS Elist* (list)
  (do ((L list (cdr L))
       (ans () (cons (car L) ans)))
      ((atom L) (scan
                 (nreverse
                  (if (null L)
                      ans
                      (cons L ans)))))))|#

(defunS Eplist (plist)
  (mapping (((key val) (scan-plist plist)))
    (cons key val)))

(defunS Ealist (alist)
  (mapping (((key val) (scan-alist alist)))
    (cons key val)))

(defunS Erange (first last &optional (step-size 1))
  (scan-range :from first :upto last :by step-size))

(defunS Evector (vector)
  (scan 'vector vector))

(defunS Efile (file)
  (scan-file file))

;; Filters and Terminators [4/4]
#|(defunS Fselect (Z boolean-Z)
  (choose Z boolean-Z))|#

#|(defunS Fpositive (Z)
  (choose-if #'plusp Z))|#

#|(defunS Fgreater (Z limit)
  (choose-if (lambda (x) (> x limit)) Z))|#

(defunS Tselect (Z boolean-Z)
  (until boolean-Z Z))

;; Reducers [/23]
(defunS Rlast (Z)
  (collect-last Z))

(defunS Rignore (Z)
  (collect-ignore Z))

(defunS Rlist (Z)
  (collect 'list Z))

(defunS Rbag (Z)
  (collect 'bag Z))

#|(defunS Rlist* (Z))|#

(defunS Rnconc (Z)
  (collect-nconc Z))

(defunS Rappend (Z)
  (collect-append Z))

(defunS Rset (Z)
  (collect 'set Z))

#|(defunS Reqset (Z)
  (delete-duplicates (collect 'set Z)
                     :test #'eq
                     :from-end T))|#

(defs Reqset (z)
  "Reqset"
  (let* ((generator (generator z))
         items
         (table (make-hash-table :test #'eq))
         (lst nil))
    (declare (type generator generator) (type list lst))
    (tagbody
     LL
       (setq items (next-in generator (go end)))
       (setf (gethash items table) t)
       (go LL)
     end)
    (with-hash-table-iterator (next-entry table)
      (loop
         (multiple-value-bind (more key) (next-entry)
           (unless more (return lst))
           (push key lst))))
    lst)
  :optimizer (apply-frag (list->frag1 '((table lst items)
                                        ((items t nil nil))
                                        ((lst nil nil nil))
                                        (((table t (make-hash-table :test #'eq))
                                          (lst list nil)))
                                        nil
                                        nil
                                        ((setf (gethash items table) t))
                                        ((with-hash-table-iterator (next-entry table)
                                           (loop
                                              (multiple-value-bind (more key)
                                                  (next-entry)
                                                (unless more (return lst))
                                                (push key lst)))))
                                        nil
                                        nil))
                         (list z))
  :trigger T)

;; Rplist
;; Palist
;; Reqplist
;; Reqalist

(defunS Rvector (Z)
  (collect 'vector Z))

(defunS Rfile (file Z)
  (collect-file file Z))

(defunS Rsum (Z)
  (collect-sum Z))

;; Rsum$
;; Rmax
;; Rmin

(defunS Rcount (Z)
  (collect-length Z))

;; Rand
;; Rand-fast
;; Ror
;; Ror-fast
