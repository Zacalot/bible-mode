;;; bible-mode.el --- A browsing interface for the SWORD Project's Diatheke CLI

;; Author: Zacalot
;; Url: https://github.com/Zacalot/bible-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: diatheke, sword, research, bible

;;; Commentary:

;; This package uses the `diatheke' program to browse and search 
;; Biblical texts provided by the Sword project. 
;; Word study is also supported.

;;; Usage:

;; First install `diatheke'.  On Debian/Ubuntu it's in the `diatheke'
;; package.

;; Use M-x `bible-open' to open a Bible buffer.
;; Use C-h f `bible-mode' to see available keybindings.

;; You may customize `bible-mode-book-module' to set a
;; default browsing module, as well as `bible-mode-word-study-enabled'
;; to enable word study by default.

;;; Code:

;;;; Requirements
(require 'cl)
(require 'dom)

;;;; Variables

(defgroup bible-mode nil
  "Settings for `bible-mode'."
  :link '(url-link "https://github.com/Zacalot/bible-mode"))

(defcustom bible-mode-book-module
  "KJV"
  "Book module for Diatheke to query."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Module abbreviation (e.g. \"KJV\")"))
  :local t
  :group 'bible-mode)

(defcustom bible-mode-word-study-enabled
  nil
  "Display Strong Hebrew, Strong Greek, and Lemma words for study."
  :type 'boolean
  :local t
  :group 'bible-mode)

(defcustom bible-mode-superscript-enabled
  t
  "Display Strong Hebrew, Strong Greek, and Lemma words in a smaller superscript form."
  :type 'boolean
  :group 'bible-mode)

(defvar bible-mode-book-chapters
  '(("Genesis" 50)("Exodus" 40)("Leviticus" 27)("Numbers" 36)("Deuteronomy" 34)("Joshua" 24)("Judges" 21)("Ruth" 4)("I Samuel" 31)("II Samuel" 24)("I Kings" 22)("II Kings" 25)("I Chronicles" 29)("II Chronicles" 36)("Ezra" 10)("Nehemiah" 13)("Esther" 10)("Job" 42)("Psalms" 150)("Proverbs" 31)("Ecclesiastes" 12)("Song of Solomon" 8)("Isaiah" 66)("Jeremiah" 52)("Lamentations" 5)("Ezekiel" 48)("Daniel" 12)("Hosea" 14)("Joel" 3)("Amos" 9)("Obadiah" 1)("Jonah" 4)("Micah" 7)("Nahum" 3)("Habakkuk" 3)("Zephaniah" 3)("Haggai" 2)("Zechariah" 14)("Malachi" 4)("Matthew" 28)("Mark" 16)("Luke" 24)("John" 21)("Acts" 28)("Romans" 16)("I Corinthians" 16)("II Corinthians" 13)("Galatians" 6)("Ephesians" 6)("Philippians" 4)("Colossians" 4)("I Thessalonians" 5)("II Thessalonians" 3)("I Timothy" 6)("II Timothy" 4)("Titus" 3)("Philemon" 1)("Hebrews" 13)("James" 5)("I Peter" 5)("II Peter" 3)("I John" 5)("II John" 1)("III John" 1)("Jude" 1)("Revelation of John" 22))
  "List of books in the Bible paired with their number of chapters.")

;;;; Keymaps

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

;;;; Modes

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

;;;; Functions

;;;;; Commands

;;;###autoload
(defun bible-open(&optional global-chapter verse)
  "Creates and opens a `bible-mode' buffer"
  (interactive)
  (let 
      (
       (buf (get-buffer-create (generate-new-buffer-name "*bible*"))))
    (set-buffer buf)
    (bible-mode)
    (bible-mode--set-global-chapter (or global-chapter 1) verse)
    (set-window-buffer (get-buffer-window (current-buffer)) buf)))

;;;###autoload
(defun bible-mode-next-chapter()
  "Pages to the next chapter for the active `bible-mode' buffer."
  (interactive)
  (bible-mode--set-global-chapter (+ bible-mode-global-chapter 1)))

;;;###autoload
(defun bible-mode-previous-chapter()
  "Pages to the previous chapter for the active `bible-mode' buffer."
  (interactive)
  (bible-mode--set-global-chapter (max 1 (- bible-mode-global-chapter 1))))

;;;###autoload
(defun bible-mode-select-book()
  "Queries user to select a new book and chapter for the current `bible-mode' buffer."
  (interactive)
  (let* (
         (chapterData (assoc (completing-read "Book: " bible-mode-book-chapters nil t) bible-mode-book-chapters))
         (bookChapter (bible-mode--get-book-global-chapter (nth 0 chapterData)))
         (chapter (if bookChapter (string-to-number (completing-read "Chapter: " (bible-mode--list-number-range 1 (nth 1 chapterData)) nil t)))))
    (if chapter
        (bible-mode--set-global-chapter (+ bookChapter chapter)))))

;;;###autoload
(defun bible-mode-select-chapter()
  "Queries user to select a new chapter for the current `bible-mode' buffer."
  (interactive)
  (let* (
         (chapterData (assoc (bible-mode--get-current-book) bible-mode-book-chapters))
         (bookChapter (bible-mode--get-book-global-chapter (nth 0 chapterData)))
         (chapter (if bookChapter (string-to-number (completing-read "Chapter: " (bible-mode--list-number-range 1 (nth 1 chapterData)) nil t)))))
    (if chapter
        (bible-mode--set-global-chapter (+ bookChapter chapter)))))

;;;###autoload
(defun bible-mode-select-module()
  "Queries user to select a new reading module for the current `bible-mode' buffer."
  (interactive)
  (let* (
         (module (completing-read "Module: " (bible-mode--list-biblical-modules))))
    (setq bible-mode-book-module module)
    (bible-mode--display)))

;;;###autoload
(defun bible-mode-toggle-word-study()
  "Toggles the inclusion of word study for the active `bible-mode' buffer."
  (interactive)
  (setq bible-mode-word-study-enabled (not bible-mode-word-study-enabled))
  (if (equal major-mode 'bible-search-mode)
      (bible-mode--display-search bible-mode-search-query bible-mode-search-mode)
    (bible-mode--display)))

;;;###autoload
(defun bible-mode-split-display()
  "Copies the active `bible-mode' buffer into a new buffer in another window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1)
  (bible-open bible-mode-global-chapter))

;;;###autoload
(defun bible-search(query)
  "Queries the user for a Bible search query. 
'lucene' mode requires an index to be built using the `mkfastmod' program."
  (interactive "sBible Search: ")
  (if (> (length query) 0)
      (let* (
             (searchmode (completing-read "Search Mode: " '("lucene" "phrase"))))
        (bible-mode--open-search query searchmode))))

;;;###autoload
(defun bible-search-mode-follow-verse()
  "Follows the hovered verse in a `bible-search-mode' buffer,
creating a new `bible-mode' buffer positioned at the specified verse."
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
    (bible-open (+ (bible-mode--get-book-global-chapter book) (string-to-number chapter)) (string-to-number verse))))

;;;###autoload
(defun bible-term-hebrew(term)
  "Queries user for a Strong Hebrew Lexicon term."
  (interactive "sTerm: ")
  (bible-mode--open-term-hebrew term))

;;;###autoload
(defun bible-term-greek(term)
  "Queries user for a Strong Greek Lexicon term."
  (interactive "sTerm: ")
  (bible-mode--open-term-greek term))

;;;###autoload
(defun bible-insert()
  "Queries user to select a verse for insertion into the current buffer."
  (interactive)
  (let* (
         (chapterData (assoc (completing-read "Book: " bible-mode-book-chapters nil t) bible-mode-book-chapters))
         (chapter (if chapterData (completing-read "Chapter: " (bible-mode--list-number-range 1 (nth 1 chapterData)) nil t)))
         (verse (if chapter (read-from-minibuffer "Verse: "))))
    (if verse
        (insert (string-trim (replace-regexp-in-string (regexp-opt `(,(concat "(" bible-mode-book-module ")"))) "" (bible-mode--exec-diatheke (concat (nth 0 chapterData) " " chapter ":" verse) nil "plain")))))))

;;;;; Support

(defun bible-mode--exec-diatheke(query &optional filter format searchtype module)
  "Executes `diatheke' with specified query options, returning the output."
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

(defun bible-mode--insert-domnode-recursive(node dom &optional iproperties notitle)
  "Recursively parses a domnode from `libxml-parse-html-region''s usage on text
produced by `bible-mode-exec-diatheke'. Outputs text to active buffer with properties."
  (if (equal (dom-attr node 'who) "Jesus") 
      (setq iproperties (plist-put iproperties 'jesus t)))

  (if (and (not notitle) (equal (dom-tag node) 'title)) ;;newline at start of title (i.e. those in Psalms)
      (insert "\n"))

  (cl-dolist (subnode (dom-children node))
    (if (and notitle (equal (dom-tag node) 'title))
        (cl-return))
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

        (bible-mode--insert-domnode-recursive subnode dom iproperties notitle)

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
                            (put-text-property refstart refend 'font-lock-face (if bible-mode-superscript-enabled
                                                                                   `(
                                                                                     :foreground "cyan"
                                                                                     :height ,(if (not floating) .7))
                                                                                 `(:foreground "cyan")))
                            (put-text-property refstart refend 'keymap bible-mode-greek-keymap)
                            (if (not floating)
                                (put-text-property refstart refend 'display '(raise .6)))))
                      (setq match (string-match "G[0-9]+" savlm (+ match matchstrlen))))

                    (if (string-match "lemma.TR:.*" savlm) ;;Lemma
                        (cl-dolist (word (split-string (match-string 0 savlm) " "))
                          (setq word (replace-regexp-in-string "[.:a-zA-Z0-9]+" "" word))
                          (insert " " word)
                          (setq refstart (- (point) (length word))
                                refend (point))
                          (put-text-property refstart refend 'font-lock-face `(:foreground "cyan"))
                          (put-text-property refstart refend 'keymap bible-mode-lemma-keymap)))

                    (if (string-match "strong:H.*" savlm) ;;Hebrew
                        (cl-dolist (word (split-string (match-string 0 savlm) " "))
                          (setq iter (+ iter 1))
                          (setq word (replace-regexp-in-string "strong:" "" word))
                          (insert (if (eq iter 1) "" " ") word)
                          (setq refstart (- (point) (length word))
                                refend (point))
                          (put-text-property refstart refend 'font-lock-face (if bible-mode-superscript-enabled
                                                                                 `(
                                                                                   :foreground "cyan"
                                                                                   :height ,(if (eq iter 1) .7))
                                                                               `(:foreground "cyan")))
                          (put-text-property refstart refend 'keymap bible-mode-hebrew-keymap))))))))))

  (if (equal (dom-tag node) 'title) ;;newline at end of title (i.e. those in Psalms)
      (insert "\n")))

(defun bible-mode--display(&optional verse)
  "Renders text for `bible-mode'"
  (setq buffer-read-only nil)
  (erase-buffer)

  (insert (bible-mode--exec-diatheke (concat "Genesis " (number-to-string bible-mode-global-chapter))))

  (let* (
         (html-dom-tree (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (bible-mode--insert-domnode-recursive (dom-by-tag html-dom-tree 'body) html-dom-tree)
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

(defun bible-mode--get-book(global-chapter)
  "Returns the book GLOBAL-CHAPTER points towards."
  (let (
        (sumChapter 0))
    (cl-dolist (curBook bible-mode-book-chapters)      
      (when (and (> global-chapter sumChapter) (<= global-chapter (+ sumChapter (nth 1 curBook))))
        (cl-return (nth 0 curBook)))
      (setq sumChapter (+ sumChapter (nth 1 curBook))))))

(defun bible-mode--get-book-global-chapter(book)
  "Returns BOOK's first chapter as a GLOBAL-CHAPTER,
the number of chapters between it and Genesis 1."
  (let (
        (sumChapter 0))    
    (cl-dolist (curBook bible-mode-book-chapters)
      (when (equal (nth 0 curBook) book)
        (cl-return sumChapter))
      (setq sumChapter (+ sumChapter (nth 1 curBook))))))

(defun bible-mode--get-current-book()
  "Returns the book the active `bible-mode' buffer is viewing."
  (bible-mode--get-book bible-mode-global-chapter))

(defun bible-mode--list-biblical-modules()
  "Returns a list of accessible Biblical Text modules."
  (let* (
         (text (bible-mode--exec-diatheke "modulelist" nil nil nil "system"))
         modules)
    (cl-dolist (line (split-string text "\n"))
      (if (equal line "Commentaries:")
          (cl-return))
      (if (not (equal "Biblical Texts:" line))
          (setq modules (cons
                         (split-string line " : ")
                         modules))))
    modules))

;;;;; Bible Searching

(defun bible-mode--open-search(query searchmode)
  "Opens a search buffer of QUERY using SEARCHMODE."
  (let 
      (
       (buf (get-buffer-create (concat "*bible-search-" (downcase bible-mode-book-module) "-" query "*"))))
    (set-buffer buf)
    (bible-search-mode)
    (bible-mode--display-search query searchmode)
    (pop-to-buffer buf nil t)))

(defun bible-mode--display-search(query searchmode)
  "Renders results of search QUERY from SEARHCMODE"
  (setq buffer-read-only nil)
  (erase-buffer)

  (if (catch 'no-results (let* (
                                (term query)
                                (result (string-trim (replace-regexp-in-string "Entries .+?--" "" (bible-mode--exec-diatheke query nil "plain" searchmode))))
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
                           (setq fullverses (bible-mode--exec-diatheke verses))

                           (insert fullverses)
                           (let* (
                                  (html-dom-tree (libxml-parse-html-region (point-min) (point-max))))
                             (erase-buffer)
                             (bible-mode--insert-domnode-recursive (dom-by-tag html-dom-tree 'body) html-dom-tree nil t)

                             (goto-char (point-min))
                             (while (search-forward (concat "(" bible-mode-book-module ")") nil t)
                               (replace-match "")))))
      (insert (concat "No results found." (if (equal searchmode "lucene") " Verify index has been build with mkfastmod."))))

  (setq mode-name (concat "Bible Search (" bible-mode-book-module ")"))
  (setq buffer-read-only t)
  (setq-local bible-mode-search-query query)
  (setq-local bible-mode-search-mode searchmode)
  (goto-char (point-min)))

;;;;; Terms

(defun bible-mode--open-term-hebrew(term)
  "Opens a buffer of the Strong Hebrew TERM's definition"
  (let 
      (
       (buf (get-buffer-create (concat "*bible-term-hebrew-" term "*"))))
    (set-buffer buf)
    (bible-term-hebrew-mode)
    (bible-mode--display-term-hebrew term)
    (pop-to-buffer buf nil t)))

(defun bible-mode--open-term-greek(term)
  "Opens a buffer of the Strong Greek TERM's definition"
  (let 
      (
       (buf (get-buffer-create (concat "*bible-term-greek-" term "*"))))
    (set-buffer buf)
    (bible-term-greek-mode)
    (bible-mode--display-term-greek term)
    (pop-to-buffer buf nil t)))

(defun bible-mode--display-term-hebrew(term)
  "Render the definition of the Strong Hebrew TERM."
  (setq buffer-read-only nil)
  (erase-buffer)

  (insert (replace-regexp-in-string (regexp-opt '("(StrongsHebrew)")) "" (bible-mode--exec-diatheke term nil nil nil "StrongsHebrew")))

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

(defun bible-mode--display-term-greek(term)
  "Render the definition of the Strong Greek TERM."
  (setq buffer-read-only nil)
  (erase-buffer)

  (insert (replace-regexp-in-string (regexp-opt '("(StrongsGreek)")) "" (bible-mode--exec-diatheke term nil nil nil "StrongsGreek")))

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

(defun bible-mode--set-global-chapter(chapter &optional verse)
  "Sets the global chapter of the active `bible-mode' buffer."
  (setq-local bible-mode-global-chapter chapter)
  (bible-mode--display verse))

;;;;; Utilities

(defun bible-mode--list-number-range(min max &optional prefix)
  "Returns a list containing entries for each integer between min and max.
Used in tandem with `completing-read' for chapter selection."
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

(provide 'bible-mode)

