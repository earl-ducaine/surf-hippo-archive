

;; This file can be loaded directly into Surf-Hippo.



;; Generate a bunch of passive hippos on an XZ grid. 
(topload
 (let ((count 0))			; Use this to count the created cells.
   (loop for x from -3 to 3 do
	 (loop for z from -4 to 2 do
	       (let ((*cell-name-suffix* ; Automatically add this suffix to the created cell name.
		      (format nil "-~D" (incf count))))
		 (move-cell
		  (dead-hippo)		; DEAD-HIPPO returns the created cell.
		  (list (* x 200)	; New X location in microns.
			(- (random 200) 100) ; New (random) Y location in microns.
			(* z 400))))))	; New Z location in microns.
   ))


;; Now draw them. First Barneyize 10 random hippos.
(color-cell-element (random-nth (cells) 10) 'purple)

;; Shift the drawing view to see the entire grid and draw.
(just-draw :scale 4.0			; microns/pixel
	   :phi-deg 30.0
	   :theta-deg 10.0)


