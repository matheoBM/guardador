
(defun qualLado (fazendeiro cabra lobo repolho)                   ;;fazendeiro cabra lobo repolho
    (list fazendeiro cabra lobo repolho)
)
(defun fazendeiro (lista)
    (nth 0 lista)
)
(defun cabra (lista)
    (nth 1 lista)
)
(defun lobo (lista)
    (nth 2 lista)
)
(defun repolho (lista)
    (nth 3 lista)
)

;;Pega o lado e retorna o oposto
(defun oposto (lado)
    (if (equal lado 'inicio)
        'chegada
        'inicio
    )
)

(defun sozinho (estado)
    (confere (qualLado (oposto (fazendeiro estado)) (cabra estado) (lobo estado) (repolho estado)))  
) 

(defun leva_cabra (estado)
    (cond ((equal (fazendeiro estado)(cabra estado))
        (confere (qualLado (oposto (fazendeiro estado)) (oposto (cabra estado)) (lobo estado) (repolho estado))))
        (t nil)
    )
)
(defun leva_lobo (estado)
    (cond ((equal (fazendeiro estado)(lobo estado))
        (confere (qualLado (oposto (fazendeiro estado)) (cabra estado) (oposto (lobo estado)) (repolho estado))))
        (t nil)
    )
)
(defun leva_repolho (estado)
    (cond ((equal (fazendeiro estado)(repolho estado))
        (confere (qualLado (oposto (fazendeiro estado)) (cabra estado) (lobo estado) (oposto (repolho estado)))))
        (t nil)
    )
)

;;Confere se os estados são validos (cabra não vai ficar com lobo ou repolho sozinhos)
(defun confere (estado)
    (cond 
        ((and (equal (cabra estado) (lobo estado)) (not (equal (fazendeiro estado) (cabra estado))) ) nil)
        ((and (equal (cabra estado) (repolho estado)) (not (equal (fazendeiro estado) (repolho estado))) ) nil)
        (t estado)
    )
)

(defun caminho (inicio fim &optional (aux nil))
    (cond 
        ((null inicio) nil)
        ((equal inicio fim) (reverse (cons inicio aux)))
        ((not (member inicio aux :test #'equal))
         (or 
            (caminho (sozinho inicio) fim (cons inicio aux))
            (caminho (leva_cabra inicio) fim (cons inicio aux)) 
            (caminho (leva_lobo inicio) fim (cons inicio aux)) 
            (caminho (leva_repolho inicio) fim (cons inicio aux)))
        )
    )
)

(defun caminho2 (inicio fim &optional (aux nil))
    (if (null inicio) 
        nil
        (if (equal inicio fim)
            (reverse (cons inicio aux))
            (if (not (member inicio aux :test #'equal))
                (or 
                    (caminho2 (sozinho inicio) fim (cons inicio aux))
                    (caminho2 (leva_cabra inicio) fim (cons inicio aux)) 
                    (caminho2 (leva_lobo inicio) fim (cons inicio aux)) 
                    (caminho2 (leva_repolho inicio) fim (cons inicio aux))
                )
            )   
        )
    )
)


(defun aux_fazendeiro (inicio fim)
    (caminho2 inicio fim)
)

(defun main()
    (aux_fazendeiro '(inicio inicio inicio inicio) '(chegada chegada chegada chegada))
)
