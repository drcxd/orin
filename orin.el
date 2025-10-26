;;; orin.el --- Search information in .org files -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude and drcxd
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, org, search
;; URL: https://github.com/drcxd/orin

;;; Commentary:

;; orin (ORg INsight) helps you search information in .org files using
;; ripgrep. It provides two operating modes:
;;
;; - Classic mode: Uses dedicated buffers with live preview while typing
;; - Modern mode: Integrated with vertico for dynamic completion
;;
;; Features:
;; - Fast ripgrep-based searching with smart-case matching
;; - OR/AND search modes (toggle with C-c C-o)
;; - Live preview of search results
;; - Group results by file with #+title: extraction
;; - Results sorted by line number within each file
;; - Navigate between matches and groups
;; - Preview files with keyword highlighting
;; - Debounced search for better performance
;;
;; Usage:
;; Set `orin-dir' to your org files directory, then run M-x orin.
;; See USAGE.md for detailed documentation.

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup orin nil
  "Search information in .org files."
  :group 'convenience
  :prefix "orin-")

(defcustom orin-dir "~/org"
  "Directory to search for .org files."
  :type 'directory
  :group 'orin)

(defcustom orin-ripgrep-executable "rg"
  "Path to the ripgrep executable."
  :type 'string
  :group 'orin)

(defcustom orin-operating-mode 'classic
  "Operating mode for orin.
Can be \\='classic for dedicated buffers or \\='modern for vertico integration."
  :type '(choice (const :tag "Classic" classic)
                 (const :tag "Modern (vertico)" modern))
  :group 'orin)

(defcustom orin-debounce-delay 0.3
  "Delay in seconds before triggering search during input.
This prevents searching on every keystroke for better performance."
  :type 'number
  :group 'orin)

(defface orin-match-line-face
  '((t :inherit highlight))
  "Face for highlighting the matching line in preview buffer."
  :group 'orin)

(defface orin-match-keyword-face
  '((t :inherit match))
  "Face for highlighting matching keywords in preview buffer."
  :group 'orin)

(defface orin-group-name-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for group names in result buffer."
  :group 'orin)

;;; Internal Variables

(defvar orin--search-mode 'or
  "Current search mode: \\='or or \\='and.")

(defvar orin--current-keywords nil
  "Current search keywords as a list of strings.")

(defvar orin--debounce-timer nil
  "Timer for debouncing search input.")

(defvar orin--preview-window nil
  "Window displaying the matching preview buffer.")

(defvar orin--result-window nil
  "Window displaying the result buffer.")

(defvar orin--search-results nil
  "Cached search results as a list of plists.")

(defvar orin--dynamic-candidates nil
  "Cached candidates for dynamic completion.")

(defvar orin--dynamic-last-input nil
  "Last input string for dynamic completion.")

(defvar orin--dynamic-last-mode nil
  "Last search mode used for dynamic completion.")

;;; Utility Functions

(defun orin--get-file-title (file)
  "Extract #+title: property from FILE if it's an .org file.
Returns the title if found, otherwise returns the file name."
  (if (string-suffix-p ".org" file)
      (with-temp-buffer
        (insert-file-contents file nil nil 1000) ; Read first 1000 bytes
        (goto-char (point-min))
        (if (re-search-forward "^#\\+title:\\s-*\\(.+\\)$" nil t)
            (match-string 1)
          (file-name-nondirectory file)))
    (file-name-nondirectory file)))

(defun orin--run-ripgrep (keywords mode)
  "Run ripgrep with KEYWORDS in MODE (\\='or or \\='and).
Returns a list of plists with :file, :line, :column, and :text keys."
  (let* ((pattern (if (eq mode 'and)
                     ;; For AND mode, search for first keyword, filter later
                     (regexp-quote (car keywords))
                   ;; For OR mode, search for all keywords with |
                   (mapconcat 'regexp-quote keywords "|")))
         (rg-args (list "--line-number"
                       "--column"
                       "--no-heading"
                       "--color=never"
                       "--smart-case"
                       "--glob=*.org"
                       "--"
                       pattern
                       (expand-file-name orin-dir))))

    (let* ((output (with-output-to-string
                    (with-current-buffer standard-output
                      (apply 'call-process orin-ripgrep-executable nil t nil rg-args))))
           (lines (split-string output "\n" t))
           (results '()))

      (dolist (line lines)
        (when (string-match "^\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)$" line)
          (let ((file (match-string 1 line))
                (line-num (string-to-number (match-string 2 line)))
                (column (string-to-number (match-string 3 line)))
                (text (match-string 4 line)))
            ;; For AND mode, check if all keywords are present
            (when (or (eq mode 'or)
                     (cl-every (lambda (kw)
                                (string-match-p (regexp-quote kw) text))
                              keywords))
              (push (list :file file
                         :line line-num
                         :column column
                         :text text)
                   results)))))
      (nreverse results))))

(defun orin--group-results (results)
  "Group RESULTS by file and sort by line number within each group.
Returns an alist of (file . results) where results are plists sorted by line."
  (let ((groups '()))
    (dolist (result results)
      (let* ((file (plist-get result :file))
             (group (assoc file groups)))
        (if group
            (setcdr group (append (cdr group) (list result)))
          (push (cons file (list result)) groups))))
    ;; Sort results within each group by line number
    (mapcar (lambda (group)
              (cons (car group)
                    (sort (copy-sequence (cdr group))
                          (lambda (a b)
                            (< (plist-get a :line)
                               (plist-get b :line))))))
            (nreverse groups))))

;;; Result Buffer Mode

(defvar orin-result-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'orin-next-match)
    (define-key map (kbd "p") 'orin-previous-match)
    (define-key map (kbd "N") 'orin-next-group)
    (define-key map (kbd "P") 'orin-previous-group)
    (define-key map (kbd "RET") 'orin-open-match)
    (define-key map (kbd "q") 'orin-quit)
    map)
  "Keymap for orin result buffer.")

(define-derived-mode orin-result-mode special-mode "Orin-Result"
  "Major mode for displaying orin search results."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun orin--render-results (buffer results &optional preview-mode)
  "Render RESULTS into BUFFER.
If PREVIEW-MODE is non-nil, don't enable preview on cursor movement."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (orin-result-mode)

      (if (null results)
          (insert "No matches found.\n")
        (let ((groups (orin--group-results results)))
          (dolist (group groups)
            (let ((file (car group))
                  (matches (cdr group)))
              ;; Insert group name
              (let ((start (point)))
                (insert (orin--get-file-title file) "\n")
                (put-text-property start (1- (point)) 'face 'orin-group-name-face)
                (put-text-property start (1- (point)) 'orin-group-header t))

              ;; Insert matches
              (dolist (match matches)
                (let ((start (point))
                      (text (plist-get match :text))
                      (line (plist-get match :line)))
                  (insert (format "  %4d: " line))
                  (let ((text-start (point)))
                    (insert text "\n")
                    ;; Highlight keywords in the text
                    (when orin--current-keywords
                      (let ((text-end (1- (point)))
                            (case-fold-search t))  ; Case-insensitive search
                        (save-excursion
                          (dolist (keyword orin--current-keywords)
                            (goto-char text-start)
                            (while (search-forward keyword text-end t)
                              (put-text-property (match-beginning 0) (match-end 0)
                                               'face 'orin-match-keyword-face)))))))
                  (put-text-property start (1- (point)) 'orin-match match)))

              (insert "\n")))))

      ;; Move to first match
      (goto-char (point-min))
      (orin--goto-next-match)

      ;; Set up preview hooks if not in preview mode
      (unless preview-mode
        (add-hook 'post-command-hook 'orin--update-preview nil t)))))

(defun orin--goto-next-match ()
  "Move point to the next match line."
  (let ((pos (point)))
    (while (and (not (eobp))
                (not (get-text-property (point) 'orin-match)))
      (forward-line 1))
    (if (get-text-property (point) 'orin-match)
        t
      (goto-char pos)
      nil)))

(defun orin--goto-previous-match ()
  "Move point to the previous match line."
  (let ((pos (point)))
    (while (and (not (bobp))
                (not (get-text-property (point) 'orin-match)))
      (forward-line -1))
    (if (get-text-property (point) 'orin-match)
        t
      (goto-char pos)
      nil)))

(defun orin-next-match ()
  "Move to the next match in the result buffer."
  (interactive)
  (let ((start-pos (point)))
    (forward-line 1)
    (unless (orin--goto-next-match)
      ;; No next match found, go back to start
      (goto-char start-pos))))

(defun orin-previous-match ()
  "Move to the previous match in the result buffer."
  (interactive)
  (let ((start-pos (point)))
    (forward-line -1)
    (unless (orin--goto-previous-match)
      ;; No previous match found, go back to start
      (goto-char start-pos))))

(defun orin-next-group ()
  "Move to the first match of the next group."
  (interactive)
  (let ((found nil))
    ;; Move past current match
    (forward-line 1)
    ;; Find next group header
    (while (and (not (eobp)) (not found))
      (if (get-text-property (point) 'orin-group-header)
          (progn
            (forward-line 1)
            (orin--goto-next-match)
            (setq found t))
        (forward-line 1)))
    (unless found
      (message "No next group"))))

(defun orin-previous-group ()
  "Move to the first match of the previous group."
  (interactive)
  (let ((found nil))
    ;; Move to start of current group
    (while (and (not (bobp))
                (not (get-text-property (point) 'orin-group-header)))
      (forward-line -1))
    ;; Move to previous group header
    (when (not (bobp))
      (forward-line -1)
      (while (and (not (bobp))
                  (not (get-text-property (point) 'orin-group-header)))
        (forward-line -1))
      (when (get-text-property (point) 'orin-group-header)
        (forward-line 1)
        (orin--goto-next-match)
        (setq found t)))
    (unless found
      (message "No previous group"))))

(defun orin-open-match ()
  "Open the file at the current match."
  (interactive)
  (let ((match (get-text-property (point) 'orin-match)))
    (when match
      (let ((file (plist-get match :file))
            (line (plist-get match :line)))
        (find-file-other-window file)
        (goto-char (point-min))
        (forward-line (1- line))
        (recenter)))))

(defun orin-quit ()
  "Quit the orin result buffer and close associated windows."
  (interactive)
  (when (window-live-p orin--preview-window)
    (delete-window orin--preview-window))
  (setq orin--preview-window nil)
  (quit-window t))

;;; Preview Buffer Functions

(defun orin--update-preview ()
  "Update the preview buffer based on current match."
  (let ((match (get-text-property (point) 'orin-match)))
    (when match
      (orin--show-preview match))))

(defun orin--show-preview (match)
  "Show preview for MATCH in a separate window."
  (let* ((file (plist-get match :file))
         (line (plist-get match :line))
         (preview-buffer (get-buffer-create "*orin-preview*")))

    (with-current-buffer preview-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Read file content without opening it persistently
        (insert-file-contents file nil nil nil t)
        (goto-char (point-min))
        (forward-line (1- line))

        ;; Highlight the matching line
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position)))
          (put-text-property line-start line-end 'face 'orin-match-line-face))

        ;; Highlight keywords
        (let ((case-fold-search t))  ; Case-insensitive search
          (dolist (keyword orin--current-keywords)
            (goto-char (point-min))
            (while (search-forward keyword nil t)
              (put-text-property (match-beginning 0) (match-end 0)
                               'face 'orin-match-keyword-face))))

        (goto-char (point-min))
        (forward-line (1- line))
        (setq buffer-read-only t)))

    ;; Display the preview buffer
    (unless (window-live-p orin--preview-window)
      (setq orin--preview-window
            (display-buffer preview-buffer
                          '((display-buffer-in-side-window)
                            (side . right)
                            (window-width . 0.5)))))

    (with-selected-window orin--preview-window
      (switch-to-buffer preview-buffer)
      (goto-char (point-min))
      (forward-line (1- line))
      (recenter))))

;;; Classic Mode Implementation

(defvar orin--minibuffer-local-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-c C-o") 'orin-toggle-search-mode)
    (define-key map (kbd "C-n") 'orin-preview-next-match)
    (define-key map (kbd "C-p") 'orin-preview-previous-match)
    (define-key map (kbd "M-n") 'orin-preview-next-match)
    (define-key map (kbd "M-p") 'orin-preview-previous-match)
    (define-key map (kbd "<C-down>") 'orin-preview-next-match)
    (define-key map (kbd "<C-up>") 'orin-preview-previous-match)
    map)
  "Keymap for orin minibuffer.")

(defun orin-toggle-search-mode ()
  "Toggle between OR and AND search modes."
  (interactive)
  (setq orin--search-mode (if (eq orin--search-mode 'or) 'and 'or))
  (message "Search mode: %s" (upcase (symbol-name orin--search-mode)))
  (if (eq orin-operating-mode 'modern)
      ;; In modern mode, clear vertico's cache and force update
      (when (minibufferp)
        ;; Reset the last-mode to force recomputation
        (setq orin--dynamic-last-mode nil)
        ;; Clear vertico's internal state
        (when (boundp 'vertico--input)
          (setq vertico--input t))  ; Set to non-string to invalidate cache
        ;; Trigger vertico to update only if it's properly initialized
        (when (and (fboundp 'vertico--exhibit)
                   (boundp 'vertico--candidates))
          (vertico--exhibit)))
    ;; In classic mode, update the preview buffer
    (orin--schedule-preview-update)))

(defun orin-preview-next-match ()
  "Move to next match in preview results buffer from minibuffer."
  (interactive)
  (orin--navigate-preview-buffer 'next))

(defun orin-preview-previous-match ()
  "Move to previous match in preview results buffer from minibuffer."
  (interactive)
  (orin--navigate-preview-buffer 'previous))

(defun orin--navigate-preview-buffer (direction)
  "Navigate preview results buffer in DIRECTION (\\='next or \\='previous).
Also updates the file preview window."
  (let ((preview-buffer (get-buffer "*orin-preview-results*")))
    (when (and preview-buffer
               (window-live-p orin--result-window))
      (save-selected-window
        (select-window orin--result-window)
        (switch-to-buffer preview-buffer)
        (if (eq direction 'next)
            (orin-next-match)
          (orin-previous-match))
        ;; Update the file preview
        (orin--update-preview)))))

(defun orin--schedule-preview-update ()
  "Schedule a preview update after debounce delay."
  (when orin--debounce-timer
    (cancel-timer orin--debounce-timer))
  (setq orin--debounce-timer
        (run-with-idle-timer orin-debounce-delay nil 'orin--update-preview-buffer)))

(defun orin--update-preview-buffer ()
  "Update the preview buffer with current minibuffer input."
  (let* ((input (minibuffer-contents))
         (keywords (split-string input nil t)))
    (if keywords
        (progn
          (setq orin--current-keywords keywords)
          (let ((results (orin--run-ripgrep keywords orin--search-mode))
                (preview-buffer (get-buffer-create "*orin-preview-results*")))
            (setq orin--search-results results)
            (orin--render-results preview-buffer results t)

            (unless (window-live-p orin--result-window)
              (setq orin--result-window
                    (display-buffer preview-buffer
                                  '((display-buffer-at-bottom)
                                    (window-height . 0.5)))))

            ;; Show preview for the first match
            (when results
              (with-current-buffer preview-buffer
                (let ((first-match (get-text-property (point) 'orin-match)))
                  (when first-match
                    (orin--show-preview first-match)))))))
      ;; If no keywords, clear the preview buffer but keep window
      (when (get-buffer "*orin-preview-results*")
        (with-current-buffer "*orin-preview-results*"
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "Type keywords to search...\n")))))))

(defun orin--minibuffer-setup ()
  "Setup function for minibuffer."
  ;; Bind keys directly in the minibuffer
  (local-set-key (kbd "C-c C-o") 'orin-toggle-search-mode)
  (local-set-key (kbd "M-n") 'orin-preview-next-match)
  (local-set-key (kbd "M-p") 'orin-preview-previous-match)
  (local-set-key (kbd "<C-down>") 'orin-preview-next-match)
  (local-set-key (kbd "<C-up>") 'orin-preview-previous-match)
  (add-hook 'after-change-functions 'orin--minibuffer-change nil t))

(defun orin--minibuffer-change (&rest _)
  "Handle changes in minibuffer input."
  (orin--schedule-preview-update))

(defun orin--classic-search ()
  "Perform search in classic mode."
  (setq orin--search-mode 'or)
  (setq orin--preview-window nil)
  (setq orin--result-window nil)

  (minibuffer-with-setup-hook
      'orin--minibuffer-setup
    (let* ((input (read-from-minibuffer "Search keywords: "))
           (keywords (split-string input nil t)))

      ;; Clean up preview window
      (when (window-live-p orin--result-window)
        (delete-window orin--result-window))
      (when (get-buffer "*orin-preview-results*")
        (kill-buffer "*orin-preview-results*"))

      (when keywords
        (setq orin--current-keywords keywords)
        (let ((results (or orin--search-results
                          (orin--run-ripgrep keywords orin--search-mode)))
              (result-buffer (get-buffer-create "*orin-results*")))
          (orin--render-results result-buffer results)
          (pop-to-buffer result-buffer))))))

;;; Modern Mode Implementation (vertico integration)

(declare-function vertico--candidate "ext:vertico")
(defvar vertico--input)
(defvar vertico--candidates)
(defvar vertico--index)

(defun orin--modern-search ()
  "Perform search in modern mode using vertico.
Search happens dynamically as you type."
  (interactive)
  (setq orin--search-mode 'or)  ; Initialize to OR mode
  (let ((selected (orin--dynamic-completing-read)))
    (when (and selected (not (string-empty-p selected)))
      ;; Find the actual candidate with text properties from our cache
      (let* ((candidate (or (cl-find selected orin--dynamic-candidates :test #'equal)
                           selected))
             (match (get-text-property 0 'orin-match candidate)))
        (if match
            (let ((file (plist-get match :file))
                  (line (plist-get match :line)))
              (find-file-other-window file)
              (goto-char (point-min))
              (forward-line (1- line))
              (recenter))
          (message "Error: No match data found for selected candidate"))))))

(defun orin--group-function (cand transform)
  "Group function for orin candidates.
Return group title for CAND or TRANSFORM the candidate."
  (if transform
      cand  ; Return candidate as-is when transforming
    (get-text-property 0 'orin-group cand)))  ; Return group title

(defun orin--dynamic-completing-read ()
  "Dynamic completing-read that searches as you type."
  (let* ((collection
          (lambda (string pred action)
            (if (eq action 'metadata)
                '(metadata (category . orin-match)
                          (group-function . orin--group-function)
                          (display-sort-function . identity))
              ;; Update candidates when input or mode changes
              (let ((keywords (split-string string nil t)))
                (when (and keywords
                          (or (not (equal string orin--dynamic-last-input))
                              (not (eq orin--search-mode orin--dynamic-last-mode))))
                  (setq orin--dynamic-last-input string)
                  (setq orin--dynamic-last-mode orin--search-mode)
                  (setq orin--current-keywords keywords)
                  (let* ((results (orin--run-ripgrep keywords orin--search-mode))
                         (groups (orin--group-results results))
                         (candidates '()))
                    (dolist (group groups)
                      (let ((matches (cdr group))
                            (title (orin--get-file-title (car group))))
                        (dolist (match matches)
                          (let ((cand (orin--format-match-for-completion-grouped match)))
                            ;; Store group title as property
                            (put-text-property 0 (length cand) 'orin-group title cand)
                            ;; Store match data as property
                            (put-text-property 0 (length cand) 'orin-match match cand)
                            (push cand candidates)))))
                    (setq orin--dynamic-candidates (nreverse candidates))))
                ;; Handle completion actions explicitly to bypass default filtering
                (cond
                 ((eq action nil)  ; try-completion
                  ;; For try-completion, find exact match or return t if complete
                  (cond
                   ((null orin--dynamic-candidates) nil)
                   ((member string orin--dynamic-candidates)
                    ;; Exact match found - return t to indicate completion is complete
                    t)
                   (t
                    ;; Return first candidate as default
                    (car orin--dynamic-candidates))))
                 ((eq action t)    ; all-completions
                  ;; Return all candidates without filtering
                  orin--dynamic-candidates)
                 ((eq action 'lambda)  ; test-completion
                  ;; Test if string is a valid completion
                  (and (member string orin--dynamic-candidates) t))
                 (t
                  ;; Fall back to default for other actions
                  (complete-with-action action orin--dynamic-candidates string pred))))))))

    (setq orin--dynamic-last-input nil)
    (setq orin--dynamic-last-mode nil)
    (setq orin--dynamic-candidates nil)

    (unwind-protect
        (progn
          (advice-add 'vertico--exhibit :after #'orin--vertico-preview-update)
          (minibuffer-with-setup-hook
              (lambda ()
                ;; Only set up the toggle key for modern mode
                (local-set-key (kbd "C-c C-o") 'orin-toggle-search-mode))
            (completing-read "Search (type keywords): " collection nil t)))
      (advice-remove 'vertico--exhibit #'orin--vertico-preview-update)
      (when (window-live-p orin--preview-window)
        (delete-window orin--preview-window)
        (setq orin--preview-window nil))
      (when (get-buffer "*orin-preview*")
        (kill-buffer "*orin-preview*")))))

(defun orin--format-match-for-completion (match group-title)
  "Format MATCH with GROUP-TITLE for display in completion."
  (let ((line (plist-get match :line))
        (text (plist-get match :text)))
    (format "%-30s %4d: %s"
            (truncate-string-to-width group-title 30 nil nil t)
            line
            text)))

(defun orin--format-match-for-completion-grouped (match)
  "Format MATCH for grouped completion (without group title).
The group title will be shown separately by the group function."
  (let ((line (plist-get match :line))
        (text (plist-get match :text)))
    (format "%4d: %s" line text)))

(defun orin--vertico-preview-function (cand)
  "Show preview for candidate CAND in vertico."
  (when cand
    (let ((match (get-text-property 0 'orin-match cand)))
      (when match
        (orin--show-preview match)))))


(defun orin--vertico-preview-update (&rest _)
  "Update preview based on current vertico candidate."
  (when (minibufferp)
    (when-let* ((cand (and (boundp 'vertico--input)
                          (boundp 'vertico--candidates)
                          (boundp 'vertico--index)
                          vertico--candidates
                          (>= vertico--index 0)
                          (< vertico--index (length vertico--candidates))
                          (nth vertico--index vertico--candidates))))
      (orin--vertico-preview-function cand))))

;;; Main Entry Point

;;;###autoload
(defun orin ()
  "Search for keywords in org files using ripgrep.

Enter space-separated keywords to search. Results are grouped by file
and sorted by line number. Press C-c C-o to toggle between OR and AND
search modes.

The operating mode is controlled by `orin-operating-mode':
- \\='classic: Uses dedicated buffers with live preview
- \\='modern: Integrates with vertico for dynamic completion

Results are searched in `orin-dir' directory."
  (interactive)
  (if (eq orin-operating-mode 'modern)
      (call-interactively 'orin--modern-search)
    (orin--classic-search)))

(provide 'orin)

;;; orin.el ends here
