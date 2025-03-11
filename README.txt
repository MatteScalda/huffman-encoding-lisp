CODIFICA DI HUFFMAN - Implementazione in Common Lisp
=================================================
Autori
------
Scaldaferri Matteo - 912001
Iudice Samuele - 912002

Questo progetto implementa l'algoritmo di codifica di Huffman in Common Lisp. 
La codifica di Huffman è un metodo di compressione dati che utilizza codici a 
lunghezza variabile per rappresentare i simboli del messaggio originale.

Struttura del Progetto
---------------------
Il progetto è composto da un singolo file 'huffman-codes.lisp' che contiene 
tutte le funzionalità necessarie per la codifica e decodifica di Huffman.

Strutture Dati
-------------
huffman-node
- Una struttura che rappresenta un nodo dell'albero di Huffman
- Campi:
  * symbol: il simbolo contenuto nel nodo (nil per nodi interni)
  * weight: il peso/frequenza del simbolo
  * left: riferimento al figlio sinistro
  * right: riferimento al figlio destro

Funzioni Principali
------------------
1. hucodec-generate-huffman-tree (symbols-n-weights)
   - Input: lista di coppie (simbolo . peso)
   - Output: albero di Huffman completo
   - Descrizione: Genera l'albero di Huffman partendo dalla lista di simboli e 
                  relativi pesi
   - Esempio: (hucodec-generate-huffman-tree '((#\a . 52) (#\b . 21)))

2. hucodec-generate-symbol-bits-table (huffman-tree)
   - Input: albero di Huffman
   - Output: tabella di associazione simbolo-codifica
   - Descrizione: Genera la tabella di codifica per ogni simbolo 
                  attraversando l'albero
   - Esempio: (hucodec-generate-symbol-bits-table tree) → 
                ((#\a . (0)) (#\b . (1 0)))

3. hucodec-encode (message huffman-tree)
   - Input: messaggio (lista di caratteri) e albero di Huffman
   - Output: lista di bit (codifica del messaggio)
   - Descrizione: Codifica un messaggio usando l'albero di Huffman
   - Esempio: (hucodec-encode '(#\a #\b #\a) tree)

4. hucodec-decode (bits huffman-tree)
   - Input: lista di bit e albero di Huffman
   - Output: messaggio decodificato (lista di simboli)
   - Descrizione: Decodifica una sequenza di bit usando l'albero di Huffman
   - Esempio: (hucodec-decode '(0 1 0) tree)

5. hucodec-encode-file (filename huffman-tree)
   - Input: path del file e albero di Huffman
   - Output: lista di bit (codifica del contenuto del file)
   - Descrizione: Legge un file di testo e lo codifica usando l'albero 
                  di Huffman
   - Esempio: (hucodec-encode-file "test.txt" tree)
   - Nota: va inserito l'inteor path del file da codificare

6. hucodec-print-huffman-tree (huffman-tree &optional indent-level)
   - Input: albero di Huffman e livello di indentazione (opzionale)
   - Output: rappresentazione testuale dell'albero
   - Descrizione: Stampa l'albero di Huffman in formato leggibile per debug
   - Esempio: (hucodec-print-huffman-tree tree)

Come Utilizzare
--------------
1. Caricare il file 'huffman-codes.lisp' nel vostro ambiente Common Lisp
2. Creare un albero di Huffman con i simboli e pesi desiderati
3. Utilizzare le funzioni di codifica/decodifica secondo necessità

Esempio di Utilizzo Completo
---------------------------
;; Creazione dell'albero
(let* ((symbols-n-weights '((#\a . 52) (#\b . 21) (#\c . 20) 
                           (#\d . 13) (#\e . 14) (#\f . 2)))
       (tree (hucodec-generate-huffman-tree symbols-n-weights))
       (message '(#\a #\b #\c #\a #\d #\e #\f)))
  ;; Codifica
  (let ((encoded (hucodec-encode message tree)))
    ;; Decodifica
    (hucodec-decode encoded tree)))

Gestione degli Errori
--------------------
- Controllo input vuoti o invalidi
- Gestione errori di file non trovato o inaccessibile
- Validazione della sequenza di bit durante la decodifica