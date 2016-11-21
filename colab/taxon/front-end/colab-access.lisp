;***************************************************************************
;***************************************************************************
;*
;*  Module           : FRONT-END
;*  File             : colab-access
;*  Author           : A. Abecker
;*  Edited           : 01.08.91
;*  Last change      : 30-11-92 
;*  Details changed  : reduce abox should keep individual associated by (un)equations
;*
;*  Comment          : the file provides some functions of the TAXON-Toplevel
;*                     that are used to be access primitives in the context
;*                     of COLAB-interaction in particular between FORWARD and
;*                     TAXON
;*  Status           : NOT released
;*
;****************************************************************************


(setq *taxon-items* 
   '((user::apred apred) (user::cpred cpred) (user::prim prim) (user::indi make-indi)
 (user::conc conc) (user::ofam ofam) (user::cfam cfam) (user::role role)
 (user::hierarchy hierarchy) (user::sfam sfam)
 (user::attr attr) (user::doma doma) (user::asse asse) (user::attrterm attrterm)))

(defun add-taxon (item)
       (eval (cons (second (assoc (first item) *taxon-items*)) (rest item))))

(defmacro attrterm (l)
   (print (assert-ind? (second l) (first l) (third l))) NIL)



;;; ATTENTION PLEASE: this kind of abox-reduction is tailored in a special way
;;;      for the WBR92-example
;;;      in other examples this reduction may cause incompleteness !!

(defun reduce-abox (abox i)
   (if (not *reduced-abox?) 
       abox
   (union (remove-if-not #'(lambda (x) (and           ;  (or  (cpred-instantiation? x)
                                                 (apred-instantiation? x)
                                                (member i (get-ind-list-of-pred-instantiation 
                                                                          (abox-entry-extern x)))))
                         abox)
          (find-transitive-fillers abox i))))

(defun find-transitive-fillers (abox ii)
   (do ((abox (remove-if #'(lambda (x) (and     ; (cpred-instantiation? x)
                                                (apred-instantiation? x)
                                                (member ii (get-ind-list-of-pred-instantiation 
                                                                          (abox-entry-extern x)))))
                         abox))
        (indlist (list ii))
        (result nil))
       ((null indlist) result)
      ;  (print "indlist") (print indlist)
     (let* ((ia  (car indlist))
            (asselist (remove-if-not #'(lambda (x) (or (and (membership-assertion? x)
                                                            (equal ia (get-ind-of-member-asse (abox-entry-extern x))))
                                                       (and (equational-assertion? x)
                                                            (or (equal ia (get-ind1-of-filler-asse 
                                                                                   (abox-entry-extern x)))
                                                                (equal ia (get-ind2-of-filler-asse 
                                                                                   (abox-entry-extern x)))))
                                                       (and (cpred-instantiation? x)
                                                            (member ia (get-ind-list-of-pred-instantiation (abox-entry-extern x))))
                                                   (and (or (role-filler-assertion? x)
                                                           (attribute-filler-assertion? x))
                                                        (not (null (abox-entry-intern x)))
                                                       (equal ia
                                                           (get-ind1-of-filler-asse 
                                                                        (abox-entry-extern x))))))
                                    abox)))
       ;  (print "**************************************") (la asselist) (print "**************************************") 
         (setf abox (set-difference abox asselist :test #'equal))
                                   
         (if (not (null asselist))
             (progn (setf result
                          (union result asselist :test 'equal))
                    (dolist (asse asselist T)
                      (cond ((or (role-filler-assertion? asse)
                              (attribute-filler-assertion? asse))
                        (let ((i (get-ind2-of-filler-asse (abox-entry-extern asse))))
                           (setf indlist
                                 (adjoin i indlist :test 'equal))))
                            ((cpred-instantiation? asse)
                           (setf indlist (union indlist (remove-if #'numberp (get-ind-list-of-pred-instantiation (abox-entry-extern asse))) 
                                                 :test #'equal)))))
                      )
             (setf indlist 
                   (remove ia indlist))))))

