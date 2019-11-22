;;Cria uma lista com o lado em que cada membro está (inicio ou chegada)
(defun qualLado (fazendeiro cabra lobo repolho)                   
    (list fazendeiro cabra lobo repolho)
)

;;retorna o lado em que determinado item está,
;;fazendeiro é o primeiro da lista, então chama (nth 0 lista), cabra é o segundo, então chama ( nth 1 lista), etc
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

;;Confere se determinado estado é valido (cabra não vai ficar com lobo ou repolho sozinhos)
;;Se for valido, retorna o estado. Se não for, retorna nil
(defun confere (estado)
    (cond 
        ((and (equal (cabra estado) (lobo estado)) (not (equal (fazendeiro estado) (cabra estado))) ) nil)
        ((and (equal (cabra estado) (repolho estado)) (not (equal (fazendeiro estado) (repolho estado))) ) nil)
        (t estado)
    )
)

;;Funções que determinam as movimentações
;;O fazendeiro pode atravessar sozinho, com a cabra, o lobo ou o repolho
;;Cada uma confere se o fazendeiro está do mesmo lado que o item a ser levado, e confere se a movimentação é válida chamando a função "confere" 
;;Dado o resultado de "confere", as funções podem retornar nil ou o estado após a movimentação
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


;;Essa função confere os diferentes caminhos e retorna uma ordem de estados (Ex. (inicio inicio inicio...) (chegada inicio inicio))
;;Tem como parâmetros, o estado inicial (inicio), o estado final (fim), e uma lista auxiliar (aux) que formará a lista com a ordem dos estados
;;Confere se o estado inicial "inicio" é nulo, se sim, retorna nil, se não, confere se o estado inicial é igual ao final
;;Se sim, retorna o inverso de (cons inicio aux). Se não, 
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

(defun texto (lista &optional (aux nil))
    (cond 
        ((null (cdr lista)) (reverse (cons 'Fim aux)))
        ((not (equal (second (car lista)) (second (cadr lista)))) (texto (cdr lista) (cons 'Leva_cabra aux)))
        ((not (equal (third (car lista)) (third (cadr lista)))) (texto (cdr lista) (cons 'Leva_lobo aux)))
        ((not (equal (fourth (car lista)) (fourth (cadr lista)))) (texto (cdr lista) (cons 'Leva_repolho aux)))
        ((not (equal (car (car lista)) (car (cadr lista)))) (texto (cdr lista) (cons 'Atravessa_o_rio aux)))
    )
)

(defun aux_fazendeiro (inicio fim)
    ;;(caminho2 inicio fim)
    (texto (caminho2 inicio fim))
)

(defun main()
    (aux_fazendeiro '(inicio inicio inicio inicio) '(chegada chegada chegada chegada))
)
