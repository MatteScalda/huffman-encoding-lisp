;; Gruppo:
;; Scaldaferri Matteo - 912001
;; Iudice Samuele - 912002

;; Struttura per i nodi dell'albero di Huffman
(defstruct huffman-node
  symbol    ; simbolo (nil per nodi interni)
  weight    ; peso/frequenza
  left      ; figlio sinistro
  right)    ; figlio destro

;; Funzione per generare l'albero di Huffman
(defun hucodec-generate-huffman-tree (symbols-n-weights)
  (unless symbols-n-weights
    (error "La lista symbols-n-weights non puo` essere vuota"))
  
  (labels ((create-leaf (pair)
             (make-huffman-node 
              :symbol (car pair) 
              :weight (cdr pair)))
           
           (merge-nodes (node1 node2)
             (make-huffman-node
              :weight (+ (huffman-node-weight node1)
                        (huffman-node-weight node2))
              :left node1
              :right node2))
           
           (build-tree (nodes)
             (if (null (cdr nodes))
                 (car nodes)
                 (let* ((sorted-nodes (sort (copy-list nodes) #'< 
                                          :key #'huffman-node-weight))
                        (node1 (first sorted-nodes))
                        (node2 (second sorted-nodes))
                        (remaining (cddr sorted-nodes))
                        (new-node (merge-nodes node1 node2)))
                   (build-tree (cons new-node remaining))))))
    
    (build-tree (mapcar #'create-leaf symbols-n-weights))))

;; Funzione per generare la tabella dei bit per ogni simbolo
(defun hucodec-generate-symbol-bits-table (huffman-tree)
  (labels ((traverse (node bits acc)
             (cond ((null node) acc)
                   ((huffman-node-symbol node)
                    (cons (cons (huffman-node-symbol node) bits) acc))
                   (t
                    (append 
                     (traverse (huffman-node-left node) 
                              (append bits (list 0)) acc)
                     (traverse (huffman-node-right node) 
                              (append bits (list 1)) acc))))))
    (traverse huffman-tree nil nil)))

;; Funzione di codifica
(defun hucodec-encode (message huffman-tree)
  (unless message
    (error "Il messaggio non puo` essere vuoto"))
  
  ;; Controllo se l'albero ha una sola foglia
  (when (huffman-node-symbol huffman-tree)
    (error "L'albero di Huffman non puo` avere una sola foglia"))
  
  (let ((bits-table (hucodec-generate-symbol-bits-table huffman-tree)))
    (labels ((encode-symbol (sym)
               (or (cdr (assoc sym bits-table))
                   (error "Simbolo non trovato nell'albero: ~A" sym)))
             (encode-message (msg)
               (when msg
                 (append (encode-symbol (car msg))
                        (encode-message (cdr msg))))))
      (encode-message message))))

;; Funzione di decodifica
(defun hucodec-decode (bits huffman-tree)
  (unless bits
    (error "La sequenza di bit non puo` essere vuota"))
  
  (labels ((decode-bits (remaining-bits current-node acc)
             (cond 
               ;; Caso base: nessun bit rimanente
               ((null remaining-bits)
                (if (huffman-node-symbol current-node)
                    (reverse (cons (huffman-node-symbol current-node) acc))
                    (error "Sequenza di bit invalida")))
               
               ;; Se siamo su una foglia, aggiungiamo il simbolo e ripartiamo
               ;; dalla radice
               ((huffman-node-symbol current-node)
                (decode-bits remaining-bits huffman-tree
                            (cons (huffman-node-symbol current-node) acc)))
               
               ;; Altrimenti continuiamo a navigare l'albero
               (t
                (let ((next-node (if (zerop (car remaining-bits))
                                    (huffman-node-left current-node)
                                    (huffman-node-right current-node))))
                  (if (null next-node)
                      (error "Sequenza di bit invalida")
                      (decode-bits (cdr remaining-bits) next-node acc)))))))
    (decode-bits bits huffman-tree nil)))

;; Funzione per codificare un file
(defun hucodec-encode-file (filename huffman-tree)
  (handler-case 
      (with-open-file (stream filename :direction :input)
        (labels ((read-all-chars (acc)
                   (let ((char (read-char stream nil nil)))
                     (if char
                         (read-all-chars (cons char acc))
                         (reverse acc)))))
          (format t "Lettura del file: ~A~%" filename)
          (hucodec-encode (read-all-chars nil) huffman-tree)))
    (file-error (e) 
      (format t "Errore nell'apertura del file: ~A~%" e)
      (format t "Directory corrente: ~A~%" (truename "."))
      nil)))

;; Funzione per stampare l'albero (per debug)
(defun hucodec-print-huffman-tree (huffman-tree &optional (indent-level 0))
  (when huffman-tree
    (let ((indent (make-string (* indent-level 2) :initial-element #\Space)))
      (format t "~A[W:~A] " 
              indent 
              (huffman-node-weight huffman-tree))
      (if (huffman-node-symbol huffman-tree)
          (format t "Foglia: ~A~%" (huffman-node-symbol huffman-tree))
          (format t "Nodo interno~%"))
      (hucodec-print-huffman-tree (huffman-node-left huffman-tree) 
                                 (1+ indent-level))
      (hucodec-print-huffman-tree (huffman-node-right huffman-tree) 
                                 (1+ indent-level))))
  nil)