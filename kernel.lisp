;;; Kernel for MinskyForth.

(in-package :minsky-forth)

(setq *read-base* 8)

(defvar *minsky-forth* '(

		(call 40 sp)
		(call 41 rp)
                (call 42 here)

key		(dis flags 1)
		(jump key)
                (get 0 key)

emit		(dis flags 2)
                (jump emit)
                (nop)
                (put 0 uart)

check-ints	(dis ints 7)
		(popj)
		(jump write-chars)
                (jump read-uart)
                (jump write-chars)
                (jump write-page)
		(jump write-chars)
                (jump write-page)
		(jump write-chars)

check-flags	(dis flags 3)
                (popj)
                (jump read-key)
                (jump write-uart)
                (pushj read-key)
                (jump write-uart)

next            (pushj check-ints)
		(pushj check-flags)
                (popj)

forth-exit	
		(jump next)

forth-docol	
		(jump next)

forth-lit	(dec sp)
                (write sp 0)
		(jump next)

forth-store	(readi sp 1)
		(write 0 1)
                (readi sp 0)
		(jump next)

forth-fetch	(read 0 0)
		(jump next)

forth-plus	(readi sp 1)
                (add 0 1)
                (jump next)

forth-two-star	(pushj forth-dup)
		(pushj forth-plus)
		(popj)

forth-two-slash	(ars 0 1)
                (jump next)

forth-quit	

))

(setq *read-base* 10.)
