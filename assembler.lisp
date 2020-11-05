;;; Assembler for the General Turtle TT2500.
;;;
;;; Compatible with the original Maclisp assembler "ZAP".

(in-package :minsky-forth)

(defvar *output*)
(defvar *symbols*)
(defvar *default*)
(defvar *pc*)
(defvar *listing*)

(defun silent (x)
  (declare (ignore x)))

(defun listing (x)
  (print x *terminal-io*))

(defun assemble (program)
  (let ((*symbols* (make-hash-table)))
    (initial-symbols)
    (let ((*pc* 0) (*output* nil) (*listing* #'silent) (*default* 0))
      (pass program))
    (let ((*pc* 0) (*output* nil) (*listing* #'listing) (*default* :error))
      (pass program)
      (nreverse *output*))))

(defun call (value symbol)
  (setf (gethash symbol *symbols*) value))

(defun initial-symbols ()
  (call 0 nil)
  (call #o14 'magic)
  (call #o15 'chartv)
  (call #o20 'ycor)
  (call #o21 'xcor)
  (call #o22 'scroll)
  (call #o23 'xr)
  (call #o24 'uart)
  (call #o25 'dsr)
  (call #o26 'key))

(defun args (lambda-list list)
  (let ((args nil))
    (dolist (arg lambda-list (values (nreverse args) list))
      (when (null list)
        (return-from args (values (nreverse args) list)))
      (case arg
        (&optional)
        (&rest (return-from args (values (append (nreverse args) list) nil)))
        (t (push (pop list) args))))))

(defun evaluate (op &rest args)
  (cond ((integerp op)
         (+ op (if args (or (apply #'evaluate args) 0) 0)))
        ((not (symbolp op))
         (error "Syntax error: (~S ~S)" op args))
        ((get op 'fn)
         (apply (get op 'fn) (args (get op 'lambda-list) args)))
        (t
         (gethash op *symbols* *default*))))

(defun assemble-line (op &rest args)
  (let ((value (apply #'evaluate op args)))
    (when value
      (emit value))))

(defun pass (program)
  (dolist (line program)
    (let ((address *pc*) code)
      (etypecase line
        (symbol (call *pc* line))
        (cons (apply #'assemble-line line)))
      (when (> *pc* address)
        (setq code (first *output*)))
      (funcall *listing* (list address code line)))))

(defmacro defasm (name args &body body)
  `(setf (get ',name 'lambda-list) ',args
         (get ',name 'fn) (lambda (,@args) ,@body)))

(defun emit (code)
  (push (logand code #o177777) *output*)
  (incf *pc*))

(defasm start (address)
  (setq *pc* address)
  nil)

(defasm call (value symbol)
  (call value symbol)
  nil)

(defun emit-reg (op a b)
  (+ op
     (ash (logand (evaluate a) 7) 6)
     (logand (evaluate b) #o17)))

(defasm t (&optional a b c)
  (logior #o10000 (evaluate a b c)))

(defasm ifc (&optional a b c)
  (logior #o30000 (evaluate a b c)))

(defasm a (a b)
  (emit-reg #o0000 a b))

(defasm nop (&optional (a 0))
  (emit-reg #o10000 a a))

(defasm lod (a)
  (emit-reg #o1000 a 0))

(defasm andn (a b)
  (emit-reg #o0020 a b))

(defasm andni (a &optional b)
  (emit-reg #o1020 a b))

(defasm and (a b)
  (emit-reg #o0040 a b))

(defasm andi (a &optional b)
  (emit-reg #o1040 a b))

(defasm nor (a b)
  (emit-reg #o0060 a b))

(defasm nori (a &optional b)
  (emit-reg #o1060 a b))

(defasm ior (a b)
  (emit-reg #o2000 a b))

(defasm iori (a &optional b)
  (emit-reg #o3000 a b))

(defasm xor (a b)
  (emit-reg #o2020 a b))

(defasm xori (a &optional b)
  (emit-reg #o3020 a b))

(defasm mrot (a b)
  (emit-reg #o2040 a b))

(defasm rot (a b)
  (emit-reg #o4000 a b))

(defasm dec (a)
  (emit-reg #o4020 a 0))

(defasm xadd (a b)
  (emit-reg #o4040 a b))

(defasm xaddi (a &optional b)
  (emit-reg #o5040 a b))

(defasm add (a b)
  (emit-reg #o4060 a b))

(defasm addi (a &optional b)
  (emit-reg #o5060 a b))

(defasm sub (a b)
  (emit-reg #o6000 a b))

(defasm subi (a &optional b)
  (emit-reg #o7000 a b))

(defasm cmp (a b)
  (emit-reg #o16000 a b))

(defasm cmpi (a &rest b)
  (when b
    (setq b (apply #'evaluate b)))
  (emit-reg #o17000 b a))

(defasm xsub (a b)
  (emit-reg #o6020 a b))

(defasm xsubi (a &optional b)
  (emit-reg #o7020 a b))

(defasm inc (a)
  (emit-reg #o6040 a 0))

(defasm ars (a b)
  (emit-reg #o6060 a b))

(defasm pushj (address)
  (+ #o40000 (evaluate address)))

(defasm jump (address)
  (+ #o50000 (evaluate address)))

(defasm error ()
  (evaluate 'jump *pc*))

(defasm dis (type mask)
  (+ #o72000
     (ecase type
       (bus    #o0000)
       (flags  #o0400)
       (ints   #o1000)
       (stars  #o1400))
     (ash (logand (lognot (evaluate mask)) #o17) 4)))

(defasm get (a b)
  (+ #o74000
     (ash (logand (evaluate a) #o17) 6)
     (logand (evaluate b) #o77)))

(defasm put (a b)
  (+ #o76000
     (ash (logand (evaluate a) 7) 6)
     (logand (evaluate b) #o77)))

(defasm popj ()
  #o076016)

(defasm magic ()
  #o075400)

(defasm chartv (a)
  (declare (ignore a))
  #o075500)

(defasm read (a b)
  (emit-reg #o020000 a b))

(defasm cread (a)
  (emit-reg #o021000 a 0))

(defasm cwrite (a)
  (emit-reg #o025000 a 0))

(defasm readi (a b)
  (emit-reg #o022040 a b))

(defasm write (a b)
  (emit-reg #o024000 a b))

(defasm writei (a b)
  (emit-reg #o026040 a b))

(defasm writed (a b)
  (emit-reg #o024020 a b))

(defun emit-branch (op address)
  (+ op (logand #o3777 (- (evaluate address) *pc* 1))))

(defasm bcc (address)
  (emit-branch #o100000 address))

(defasm bcs (address)
  (emit-branch #o104000 address))

(defasm bvs (address)
  (emit-branch #o110000 address))

(defasm bvc (address)
  (emit-branch #o114000 address))

(defasm bmi (address)
  (emit-branch #o120000 address))

(defasm bpl (address)
  (emit-branch #o124000 address))

(defasm bne (address)
  (emit-branch #o130000 address))

(defasm beq (address)
  (emit-branch #o134000 address))

(defasm bis (address)
  (emit-branch #o150000 address))

(defasm bic (address)
  (emit-branch #o154000 address))

(defasm bxci (address)
  (emit-branch #o160000 address))

(defasm bxsi (address)
  (emit-branch #o164000 address))

(defasm bfs (address)
  (emit-branch #o170000 address))

(defasm bfc (address)
  (emit-branch #o174000 address))

(defasm & (&rest comment)
  (declare (ignore comment))
  nil)

(defasm - (a &optional b)
  (if b
      (- (evaluate a) (evaluate b))
      (- (evaluate a))))

(defasm + (a &rest b)
  (+ (evaluate a) (apply #'evaluate b)))

(defasm * (a b)
  (* (evaluate a) (evaluate b)))

(defvar *checksum* 0)

(defun ascii-16 (word)
  (write-char (code-char (+ #o100 (ldb (byte 4 12) word))))
  (write-char (code-char (+ #o100 (ldb (byte 4  8) word))))
  (write-char (code-char (+ #o100 (ldb (byte 4  4) word))))
  (write-char (code-char (+ #o100 (ldb (byte 4  0) word)))))

(defun ascii-18 (word)
  (incf *checksum* word)
  (write-char (code-char (+ #o100 (ldb (byte 6 12) word))))
  (write-char (code-char (+ #o100 (ldb (byte 6  6) word))))
  (write-char (code-char (+ #o100 (ldb (byte 6  0) word)))))

(defun ascii-block (address count type program)
  (ascii-18 #o120116)
  (setq *checksum* 0)
  (ascii-18 type) ;Type
  (ascii-18 address)
  (ascii-18 (if (and (= type 2) (>= address #o170000)) (* 2 count) count))
  (loop repeat count do (ascii-18 (pop program)))
  (ascii-18 (logand (- *checksum*) #o177777))
  program)

(defun ascii (program &key (address 0) (start t) (type 1) (loader t))
  ;;Loader
  (when loader
    (ascii-16 #o147577)
    (ascii-16 0)  ;Address
    (ascii-16 0)) ;Count
  ;;Blocks
  (loop for count = (min (length program) 64)
        until (endp program) do
    (setq program (ascii-block address count type program))
    (when (and (= type 2) (>= address #o170000))
      (incf address count))
    (incf address count))
  ;;Start instruction
  (when start
    (ascii-18 #o120116)
    (ascii-18 0)
    (ascii-18 (+ #o50000 address))))

(defun write-program (file list)
  (with-open-file (*standard-output* file
                   :direction :output :if-exists :supersede)
    (dolist (program list)
      (ascii (assemble program)))))
