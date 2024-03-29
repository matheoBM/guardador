(defun solve-fwgc (state goal) (path state goal nil))

(defun path (state goal been-list)
   (cond ((null state) nil)
	 ((equal state goal) (reverse (cons state been-list)))
	 ((not (member state been-list :test #'equal))
	      (or (path (farmer-takes-self state) goal (cons state been-list))
	          (path (farmer-takes-wolf state) goal (cons state been-list))
	          (path (farmer-takes-goat state) goal (cons state been-list))
	          (path (farmer-takes-cabbage state) goal (cons state been-list))))))

(defun farmer-takes-self (state)
   (safe (make-state (opposite (farmer-side state))
		(wolf-side state)
	 	(goat-side state)
	 	(cabbage-side state))))


(defun farmer-takes-wolf (state)
   (cond ((equal (farmer-side state) (wolf-side state))
                     (safe (make-state (opposite (farmer-side state))
	                                        (opposite (wolf-side state))
	                                        (goat-side state)
	                                        (cabbage-side state))))
   	    (t nil)))

(defun farmer-takes-goat (state)
   (cond ((equal (farmer-side state) (goat-side state))
                  (safe (make-state (opposite (farmer-side state))
	                                     (wolf-side state)
	                                     (opposite (goat-side state))
	                                     (cabbage-side state)))) 
  	    (t nil)))

(defun farmer-takes-cabbage (state)
   (cond ((equal (farmer-side state) (cabbage-side state))
                    (safe (make-state (opposite (farmer-side state))
	                                       (wolf-side state)
	                                       (goat-side state)
	                                       (opposite (cabbage-side state)))))   
	   (t nil)))


(defun make-state (f w g c) (list f w g c))

(defun farmer-side ( state )
   (nth 0 state))

(defun wolf-side ( state )
   (nth 1 state))

(defun goat-side ( state )
   (nth 2 state))

(defun cabbage-side ( state )
   (nth 3 state))

(defun opposite (side)
   (cond ((equal side 'e) 'w)
             ((equal side 'w) 'e)))

(defun safe (state)
   (cond ((and (equal (goat-side state) (wolf-side state))
	             (not (equal (farmer-side state) (wolf-side state))))  nil)
            ((and (equal (goat-side state) (cabbage-side state))
	             (not (equal (farmer-side state) (goat-side state)))) nil)
	   (t state)))
       