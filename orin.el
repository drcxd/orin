;;; orin.el --- Search information in .org files -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, org, search
;; URL: https://github.com/yourusername/orin

;;; Commentary:

;; orin (ORg INsight) helps you search information in .org files using
;; ripgrep. It provides two operating modes: classic mode with dedicated
;; buffers, and modern mode integrated with vertico and consult.

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
  "Group RESULTS by file.
Returns an alist of (file . results) where results are plists."
  (let ((groups '()))
    (dolist (result results)
      (let* ((file (plist-get result :file))
             (group (assoc file groups)))
        (if group
            (setcdr group (append (cdr group) (list result)))
          (push (cons file (list result)) groups))))
    (nreverse groups)))

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
    (when (eobp)
      (goto-char pos)
      nil)))

(defun orin--goto-previous-match ()
  "Move point to the previous match line."
  (let ((pos (point)))
    (while (and (not (bobp))
                (not (get-text-property (point) 'orin-match)))
      (forward-line -1))
    (when (bobp)
      (goto-char pos)
      nil)))

(defun orin-next-match ()
  "Move to the next match in the result buffer."
  (interactive)
  (forward-line 1)
  (orin--goto-next-match))

(defun orin-previous-match ()
  "Move to the previous match in the result buffer."
  (interactive)
  (forward-line -1)
  (orin--goto-previous-match))

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
                          '((display-buffer-at-bottom)
                            (window-height . 0.4)))))

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
    map)
  "Keymap for orin minibuffer.")

(defun orin-toggle-search-mode ()
  "Toggle between OR and AND search modes."
  (interactive)
  (setq orin--search-mode (if (eq orin--search-mode 'or) 'and 'or))
  (message "Search mode: %s" (upcase (symbol-name orin--search-mode)))
  (orin--schedule-preview-update))

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
                                    (window-height . 0.5)))))))
      ;; If no keywords, clear the preview buffer but keep window
      (when (get-buffer "*orin-preview-results*")
        (with-current-buffer "*orin-preview-results*"
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "Type keywords to search...\n")))))))

(defun orin--minibuffer-setup ()
  "Setup function for minibuffer."
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
    (let* ((input (read-from-minibuffer
                   "Search keywords: "
                   nil
                   orin--minibuffer-local-map))
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

;;; Modern Mode Implementation (vertico/consult integration)

(defun orin--modern-search ()
  "Perform search in modern mode using vertico."
  (if (not (require 'consult nil t))
      (progn
        (message "Modern mode requires consult package. Falling back to classic mode.")
        (orin--classic-search))

    (setq orin--search-mode 'or)
    (setq orin--preview-window nil)

    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'after-change-functions 'orin--modern-minibuffer-change nil t))

      (let* ((input (read-from-minibuffer
                     "Search keywords: "
                     nil
                     orin--minibuffer-local-map))
             (keywords (split-string input nil t)))

        ;; Clean up preview window
        (when (window-live-p orin--preview-window)
          (delete-window orin--preview-window)
          (setq orin--preview-window nil))
        (when (get-buffer "*orin-preview*")
          (kill-buffer "*orin-preview*"))

        (when keywords
          (setq orin--current-keywords keywords)
          (let ((results (orin--run-ripgrep keywords orin--search-mode)))
            (when results
              (orin--vertico-select results))))))))

(defun orin--modern-minibuffer-change (&rest _)
  "Handle changes in minibuffer for modern mode."
  (orin--schedule-preview-update))

(defun orin--vertico-select (results)
  "Use vertico to select from RESULTS."
  (let* ((groups (orin--group-results results))
         (candidates '())
         (match-map (make-hash-table :test 'equal)))

    ;; Build candidates list with group headers
    (dolist (group groups)
      (let ((matches (cdr group))
            (title (orin--get-file-title (car group))))
        ;; Add group header
        (push (propertize title 'face 'orin-group-name-face) candidates)

        ;; Add matches
        (dolist (match matches)
          (let* ((line (plist-get match :line))
                 (text (plist-get match :text))
                 (cand (format "  %4d: %s" line text)))
            (push cand candidates)
            (puthash cand match match-map)))))

    (setq candidates (nreverse candidates))

    ;; Use completing-read with preview
    (let ((selected
           (completing-read "Select match: " candidates nil t nil nil)))

      (when (window-live-p orin--preview-window)
        (delete-window orin--preview-window))

      (let ((match (gethash selected match-map)))
        (when match
          (let ((file (plist-get match :file))
                (line (plist-get match :line)))
            (find-file-other-window file)
            (goto-char (point-min))
            (forward-line (1- line))
            (recenter)))))))

;;; Main Entry Point

;;;###autoload
(defun orin ()
  "Search for keywords in org files.
The operating mode is controlled by `orin-operating-mode'."
  (interactive)
  (if (eq orin-operating-mode 'modern)
      (orin--modern-search)
    (orin--classic-search)))

(provide 'orin)

;;; orin.el ends here
