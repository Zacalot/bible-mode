(defvar bible-mode-book-module
  "KJV"
  "Book module for Diatheke to query.")

(defvar bible-mode-word-study-enabled
  nil
  "Display Hebrew and Greek words for study.")

(defconst bible-mode-map (make-keymap))
(define-key bible-mode-map "f" 'bible-mode-next-chapter)
(define-key bible-mode-map "b" 'bible-mode-previous-chapter)
(define-key bible-mode-map "g" 'bible-mode-select-book)
(define-key bible-mode-map "c" 'bible-mode-select-chapter)
(define-key bible-mode-map "s" 'bible-search)
(define-key bible-mode-map "m" 'bible-mode-select-module)
(define-key bible-mode-map "w" 'bible-mode-toggle-word-study)
(define-key bible-mode-map "x" 'bible-mode-split-display)

(defconst bible-search-mode-map (make-keymap))
(define-key bible-search-mode-map "s" 'bible-search)
(define-key bible-search-mode-map "w" 'bible-mode-toggle-word-study)
(define-key bible-search-mode-map (kbd "RET") 'bible-search-mode-follow-verse)

(defconst bible-term-hebrew-mode-map (make-keymap))
(defconst bible-term-greek-mode-map (make-keymap))

(defconst bible-mode-greek-keymap (make-sparse-keymap))
(define-key bible-mode-greek-keymap (kbd "RET") (lambda ()
                                                  (interactive)
                                                  (bible-term-greek (replace-regexp-in-string "[^0-9]*" "" (thing-at-point 'word t)))))

(defconst bible-mode-lemma-keymap (make-sparse-keymap))
(define-key bible-mode-lemma-keymap (kbd "RET") (lambda ()(interactive)))

(defconst bible-mode-hebrew-keymap (make-sparse-keymap))
(define-key bible-mode-hebrew-keymap (kbd "RET") (lambda ()
                                                   (interactive)
                                                   (bible-term-hebrew (replace-regexp-in-string "[a-z]+" "" (thing-at-point 'word t)))))

(define-derived-mode bible-mode special-mode "Bible"
  "Mode for reading the Bible.
\\{bible-mode-map}"
  (buffer-disable-undo)
  (font-lock-mode t)
  (use-local-map bible-mode-map)
  (setq buffer-read-only t)
  (setq word-wrap t))

(define-derived-mode bible-search-mode special-mode "Bible Search"
  "Mode for performing Bible searches.
\\{bible-search-mode-map}"
  (buffer-disable-undo)
  (font-lock-mode t)
  (use-local-map bible-search-mode-map)
  (setq buffer-read-only t)
  (setq word-wrap t))

(define-derived-mode bible-term-hebrew-mode special-mode "Hebrew Bible Term"
  "Mode for researching Hebrew terms in the Bible.
\\{bible-term-hebrew-mode-map}"
  (buffer-disable-undo)
  (font-lock-mode t)
  (use-local-map bible-term-hebrew-mode-map)
  (setq buffer-read-only t)
  (setq word-wrap t))

(define-derived-mode bible-term-greek-mode special-mode "Hebrew Bible Term"
  "Mode for researching Greek terms in the Bible.
\\{bible-term-greek-mode-map}"
  (buffer-disable-undo)
  (font-lock-mode t)
  (use-local-map bible-term-greek-mode-map)
  (setq buffer-read-only t)
  (setq word-wrap t))

(setq bible-mode-book-chapters '(("Genesis" 50)("Exodus" 40)("Leviticus" 27)("Numbers" 36)("Deuteronomy" 34)("Joshua" 24)("Judges" 21)("Ruth" 4)("I Samuel" 31)("II Samuel" 24)("I Kings" 22)("II Kings" 25)("I Chronicles" 29)("II Chronicles" 36)("Ezra" 10)("Nehemiah" 13)("Esther" 10)("Job" 42)("Psalms" 150)("Proverbs" 31)("Ecclesiastes" 12)("Song of Solomon" 8)("Isaiah" 66)("Jeremiah" 52)("Lamentations" 5)("Ezekiel" 48)("Daniel" 12)("Hosea" 14)("Joel" 3)("Amos" 9)("Obadiah" 1)("Jonah" 4)("Micah" 7)("Nahum" 3)("Habakkuk" 3)("Zephaniah" 3)("Haggai" 2)("Zechariah" 14)("Malachi" 4)("Matthew" 28)("Mark" 16)("Luke" 24)("John" 21)("Acts" 28)("Romans" 16)("I Corinthians" 16)("II Corinthians" 13)("Galations" 6)("Ephesians" 6)("Philippians" 4)("Colossians" 4)("I Thessalonians" 5)("II Thessalonians" 3)("I Timothy" 6)("II Timothy" 4)("Titus" 3)("Philemon" 1)("Hebrews" 13)("James" 5)("I Peter" 5)("II Peter" 3)("I John" 5)("II John" 1)("III John" 1)("Jude" 1)("Revelation of John" 22)))

(defun bible-open(&optional global-chapter verse)
  (interactive)
  (let 
      (
       (buf (get-buffer-create (generate-new-buffer-name "*bible*"))))
    (set-buffer buf)
    (bible-mode)
    (bible-mode-set-chapter (or global-chapter 1) verse)
    (set-window-buffer (get-buffer-window (current-buffer)) buf)))

(defun bible-search(query)
  (interactive "sBible Search: ")
  (if (> (length query) 0)
      (let* (
             (searchmode (completing-read "Search Mode: " '("lucene" "phrase"))))
        (bible-open-search query searchmode))))

(defun bible-term-hebrew(term)
  (interactive "sTerm: ")
  (bible-open-term-hebrew term))

(defun bible-term-greek(term)
  (interactive "sTerm: ")
  (bible-open-term-greek term))

(defun bible-open-term-hebrew(term)
  (let 
      (
       (buf (get-buffer-create (concat "*bible-term-hebrew-" term "*"))))
    (set-buffer buf)
    (bible-term-hebrew-mode)
    (bible-mode-display-term-hebrew term)
    (pop-to-buffer buf nil t)))

(defun bible-open-term-greek(term)
  (let 
      (
       (buf (get-buffer-create (concat "*bible-term-greek-" term "*"))))
    (set-buffer buf)
    (bible-term-greek-mode)
    (bible-mode-display-term-greek term)
    (pop-to-buffer buf nil t)))

(defun bible-open-search(query searchmode)
  (let 
      (
       (buf (get-buffer-create (concat "*bible-search-" (downcase bible-mode-book-module) "-" query "*"))))
    (set-buffer buf)
    (bible-search-mode)
    (bible-mode-display-search query searchmode)
    (pop-to-buffer buf nil t)))

(defun bible-mode-exec-diatheke(query &optional filter format searchtype module)
  (with-temp-buffer
    (let (
          (args (list "diatheke"
                      nil
                      (current-buffer)
                      t
                      "-b" (or module bible-mode-book-module))))
      (if filter (setq args (append args (list
                                          "-o" (pcase filter
                                                 ("jesus" "w"))
                                          ))))
      (if searchtype (setq args (append args (list
                                              "-s" (pcase searchtype
                                                     ("lucene" "lucene")
                                                     ("phrase" "phrase")
                                                     )
                                              ))))
      (setq args (append args (list
                               "-o" (pcase filter
                                      (_ "w"))
                               "-f" (pcase format
                                      ("plain" "plain")
                                      (_ "internal"))
                               "-k" query
                               )))
      (apply 'call-process args))
    (buffer-string)))

(defun bible-mode-verse-exists(verse)
  (equal (substring (bible-mode-exec-diatheke verse nil "plain") 0 (length verse)) verse))

(defun bible-mode-insert-node-recursive(node dom &optional iproperties notitle)
  (if (equal (dom-attr node 'who) "Jesus") 
      (setq iproperties (plist-put iproperties 'jesus t)))

  (if (and (not notitle) (equal (dom-tag node) 'title)) ;;newline at start of title (i.e. those in Psalms)
      (insert "\n"))

  (dolist (subnode (dom-children node))
    (if (and notitle (equal (dom-tag node) 'title))
        (return))
    (if (stringp subnode)
        (progn
          (let* (
                 (verse-start (string-match ".+?:[0-9]?[0-9]?[0-9]?:" subnode))
                 verse-start-text
                 verse-match)
            (if verse-start
                (setq verse-match (string-trim (match-string 0 subnode))
                      verse-start-text (string-trim-left (substring subnode verse-start (length subnode)))
                      subnode (concat (substring subnode 0 verse-start) verse-start-text)))
            (insert (string-trim-right subnode))
            (cond
             ((plist-get iproperties 'jesus)
              (put-text-property (- (point) (length (string-trim-right subnode))) (point) 'font-lock-face '(:foreground "red")))
             (verse-start
              (let* (
                     (start (- (point) (length (string-trim-right verse-start-text)))))
                (put-text-property start (+ start (length (string-trim-right verse-match))) 'font-lock-face '(:foreground "purple")))))))
      (progn
        (if (and (not (eq (dom-tag subnode) 'p)) (not (eq (dom-tag subnode) 'q)) (not (eq "" (dom-text subnode))))
            (insert " "))

        (bible-mode-insert-node-recursive subnode dom iproperties notitle)

        (if (and bible-mode-word-study-enabled (not (stringp subnode)));;word study. Must be done after subnode is inserted recursively.
            (let (
                  (savlm (dom-attr subnode 'savlm))
                  (match 0)
                  (matchstrlen 0)
                  (iter 0)
                  floating
                  refstart
                  refend)
              (if savlm
                  (progn
                    (while match ;;Greek
                      (if (> match 0)
                          (progn
                            (setq floating (or (> matchstrlen 0) (string-empty-p (dom-text subnode)))
                                  matchstrlen (length (match-string 0 savlm)))
                            (insert (if floating " " "") (match-string 0 savlm))
                            (setq refstart (- (point) matchstrlen)
                                  refend (point))
                            (put-text-property refstart refend 'font-lock-face `(
                                                                                 :foreground "cyan"
                                                                                 :height ,(if (not floating) .7)))
                            (put-text-property refstart refend 'keymap bible-mode-greek-keymap)
                            (if (not floating)
                                (put-text-property refstart refend 'display '(raise .6)))))
                      (setq match (string-match "G[0-9]+" savlm (+ match matchstrlen))))

                    (if (string-match "lemma.TR:.*" savlm) ;;Lemma
                        (dolist (word (split-string (match-string 0 savlm) " "))
                          (setq word (replace-regexp-in-string "[.:a-zA-Z0-9]+" "" word))
                          (insert " " word)
                          (setq refstart (- (point) (length word))
                                refend (point))
                          (put-text-property refstart refend 'font-lock-face `(:foreground "cyan"))
                          (put-text-property refstart refend 'keymap bible-mode-lemma-keymap)))

                    (if (string-match "strong:H.*" savlm) ;;Hebrew
                        (dolist (word (split-string (match-string 0 savlm) " "))
                          (setq iter (+ iter 1))
                          (setq word (replace-regexp-in-string "strong:" "" word))
                          (insert (if (eq iter 1) "" " ") word)
                          (setq refstart (- (point) (length word))
                                refend (point))
                          (put-text-property refstart refend 'font-lock-face `(
                                                                               :foreground "cyan"
                                                                               :height ,(if (eq iter 1) .7)))
                          (put-text-property refstart refend 'keymap bible-mode-hebrew-keymap))))))))))

  (if (equal (dom-tag node) 'title) ;;newline at end of title (i.e. those in Psalms)
      (insert "\n")))

(defun bible-mode-display(&optional verse)
  (setq buffer-read-only nil)
  (erase-buffer)

  (insert (bible-mode-exec-diatheke (concat "Genesis " (number-to-string bible-mode-chapter))))

  (let* (
         (html-dom-tree (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (bible-mode-insert-node-recursive (dom-by-tag html-dom-tree 'body) html-dom-tree)
    (goto-char (point-min))
    (while (search-forward (concat "(" bible-mode-book-module ")") nil t)
      (replace-match "")))

  (setq mode-name (concat "Bible (" bible-mode-book-module ")"))
  (setq buffer-read-only t)
  (goto-char (point-min))
  (if verse
      (progn
        (goto-char (string-match (regexp-opt `(,(concat ":" (number-to-string verse) ": "))) (buffer-string)))
        (beginning-of-line))))

(defun bible-mode-display-term-hebrew(number)
  (setq buffer-read-only nil)
  (erase-buffer)

  (insert (replace-regexp-in-string (regexp-opt '("(StrongsHebrew)")) "" (bible-mode-exec-diatheke number nil nil nil "StrongsHebrew")))

  (let* (
         (text (buffer-string))
         (match 0)
         (matchstrlen 0)
         refstart
         refend)
    (while match
      (if (> match 0)
          (progn
            (setq matchstrlen (length (match-string 0 text)))
            (setq refstart match
                  refend (+ match matchstrlen 1))
            (put-text-property refstart refend 'font-lock-face `(:foreground "cyan"))
            (put-text-property refstart refend 'keymap bible-mode-hebrew-keymap)))
      (setq match (string-match "[0-9]+" text (+ match matchstrlen)))))
  (setq buffer-read-only t)
  (goto-char (point-min)))

(defun bible-mode-display-term-greek(number)
  (setq buffer-read-only nil)
  (erase-buffer)

  (insert (replace-regexp-in-string (regexp-opt '("(StrongsGreek)")) "" (bible-mode-exec-diatheke number nil nil nil "StrongsGreek")))

  (let* (
         (text (buffer-string))
         (match 0)
         (matchstrlen 0)
         refstart
         refend)
    (while match
      (if (> match 0)
          (progn
            (setq matchstrlen (length (match-string 0 text)))
            (setq refstart match
                  refend (+ match matchstrlen 1))
            (put-text-property refstart refend 'font-lock-face `(:foreground "cyan"))
            (put-text-property refstart refend 'keymap bible-mode-greek-keymap)))
      (setq match (string-match "[0-9]+" text (+ match matchstrlen)))))
  (setq buffer-read-only t)
  (goto-char (point-min)))

(defun bible-mode-display-search(query searchmode)
  (setq buffer-read-only nil)
  (erase-buffer)

  (if (catch 'no-results (let* (
                                (term query)
                                (result (string-trim (replace-regexp-in-string "Entries .+?--" "" (bible-mode-exec-diatheke query nil "plain" searchmode))))
                                (match 0)
                                (matchstr "")
                                (verses "")
                                fullverses)
                           (if (equal result (concat "none (" bible-mode-book-module ")"))
                               (throw 'no-results t))
                           (while match
                             (setq match (string-match ".+?:[0-9]?[0-9]?"
                                                       result (+ match (length matchstr)))
                                   matchstr (match-string 0 result))
                             (if match
                                 (setq verses (concat verses (replace-regexp-in-string ".+; " "" matchstr) ";"))))

                           (setq match 0)
                           (setq fullverses (bible-mode-exec-diatheke verses))

                           (insert fullverses)
                           (let* (
                                  (html-dom-tree (libxml-parse-html-region (point-min) (point-max))))
                             (erase-buffer)
                             (bible-mode-insert-node-recursive (dom-by-tag html-dom-tree 'body) html-dom-tree nil t)

                             (goto-char (point-min))
                             (while (search-forward (concat "(" bible-mode-book-module ")") nil t)
                               (replace-match "")))))
      (insert (concat "No results found." (if (equal searchmode "lucene") " Verify index has been build with mkfastmod."))))

  (setq mode-name (concat "Bible Search (" bible-mode-book-module ")"))
  (setq buffer-read-only t)
  (setq-local bible-mode-search-query query)
  (setq-local bible-mode-search-mode searchmode)
  (goto-char (point-min)))

(defun bible-mode-set-chapter(chapter &optional verse)
  (setq-local bible-mode-chapter chapter)
  (bible-mode-display verse))

(defun bible-mode-next-chapter()
  (interactive)
  (bible-mode-set-chapter (+ bible-mode-chapter 1)))

(defun bible-mode-previous-chapter()
  (interactive)
  (bible-mode-set-chapter (max 1 (- bible-mode-chapter 1))))

(defun bible-mode-list-all-books()
  (let* (books)
    (progn
      (dolist (book bible-mode-book-chapters)
        (setq books (cons
                     (cons
                      (nth 0 book)
                      (nth 0 book))
                     books)))
      books)))

(defun bible-mode-list-numbers(min max &optional prefix)
  (let* (
         (num max)
         nums)
    (while (>= num min)
      (setq nums (cons
                  (cons
                   (concat prefix (number-to-string num))
                   num)
                  nums))
      (setq num (- num 1)))
    nums))

(defun bible-mode-select-book()
  (interactive)
  (let* (
         (chapterData (assoc (completing-read "Book: " bible-mode-book-chapters nil t) bible-mode-book-chapters))
         (bookChapter (bible-mode-get-book-global-chapter (nth 0 chapterData))))
    (if bookChapter
        (let* (
               (chapter (string-to-number (completing-read "Chapter: " (bible-mode-list-numbers 1 (nth 1 chapterData)) nil t))))
          (bible-mode-set-chapter (+ bookChapter chapter))))))

(defun bible-mode-select-chapter()
  (interactive)
  (let* (
         (chapterData (assoc (bible-mode-get-current-book) bible-mode-book-chapters))
         (bookChapter (bible-mode-get-book-global-chapter (nth 0 chapterData))))
    (if bookChapter
        (let* (
               (chapter (string-to-number (completing-read "Chapter: " (bible-mode-list-numbers 1 (nth 1 chapterData)) nil t))))
          (bible-mode-set-chapter (+ bookChapter chapter))))))

(defun bible-mode-select-module()
  (interactive)
  (let* (
         (module (completing-read "Module: " (bible-mode-list-biblical-modules)))
         )
    (setq bible-mode-book-module module)
    (bible-mode-display)))

(defun bible-mode-get-book-global-chapter(book)
  (let (
        (sumChapter 0))    
    (dolist (curBook bible-mode-book-chapters)
      (when (equal (nth 0 curBook) book)
        (return sumChapter))
      (setq sumChapter (+ sumChapter (nth 1 curBook))))))

(defun bible-mode-get-book(chapter)
  (let (
        (sumChapter 0))
    (dolist (curBook bible-mode-book-chapters)      
      (when (and (> chapter sumChapter) (<= chapter (+ sumChapter (nth 1 curBook))))
        (return (nth 0 curBook)))
      (setq sumChapter (+ sumChapter (nth 1 curBook))))))

(defun bible-mode-get-current-book()
  (bible-mode-get-book bible-mode-chapter))

(defun bible-mode-list-biblical-modules()
  (let* (
         (text (bible-mode-exec-diatheke "modulelist" nil nil nil "system"))
         modules
         )
    (dolist (line (split-string text "\n"))
      (if (equal line "Commentaries:")
          (return))
      (if (not (equal "Biblical Texts:" line))
          (setq modules (cons
                         (split-string line " : ")
                         modules))))
    modules))

(defun bible-mode-toggle-word-study()
  (interactive)
  (setq bible-mode-word-study-enabled (not bible-mode-word-study-enabled))
  (if (equal major-mode 'bible-search-mode)
      (bible-mode-display-search bible-mode-search-query bible-mode-search-mode)
    (bible-mode-display)))

(defun bible-mode-split-display()
  (interactive)
  (split-and-follow-vertically)
  (bible-open bible-mode-chapter))

(defun bible-search-mode-follow-verse()
  (interactive)
  (let* (
         (text (thing-at-point 'line t))
         book
         chapter
         verse)
    (string-match ".+ [0-9]?[0-9]?[0-9]?:[0-9]?[0-9]?[0-9]?:" text)
    (setq text (match-string 0 text))

    (string-match " [0-9]?[0-9]?[0-9]?:" text)
    (setq chapter (replace-regexp-in-string "[^0-9]" "" (match-string 0 text)))

    (string-match ":[0-9]?[0-9]?[0-9]?" text)
    (setq verse (replace-regexp-in-string "[^0-9]" "" (match-string 0 text)))
    (setq book (replace-regexp-in-string "[ ][0-9]?[0-9]?[0-9]?:[0-9]?[0-9]?[0-9]?:$" "" text))
    (bible-open (+ (bible-mode-get-book-global-chapter book) (string-to-number chapter)) (string-to-number verse))))
