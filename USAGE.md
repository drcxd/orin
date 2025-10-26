# Orin Usage Guide

`orin` (ORg INsight) is an Emacs package for searching information in `.org` files using ripgrep. It provides fast, interactive search with live previews and flexible navigation.

## Table of Contents

- [Installation](#installation)
- [Quick Start](#quick-start)
- [Operating Modes](#operating-modes)
  - [Classic Mode](#classic-mode)
  - [Modern Mode](#modern-mode)
- [Search Modes](#search-modes)
- [Navigation](#navigation)
- [Customization](#customization)
- [Tips and Tricks](#tips-and-tricks)

## Installation

### Prerequisites

- Emacs 27.1 or later
- [ripgrep](https://github.com/BurntSushi/ripgrep) installed and available in your PATH
- (Optional) [vertico](https://github.com/minad/vertico) and [consult](https://github.com/minad/consult) for modern mode

### Manual Installation

1. Clone or download this repository
2. Add to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/orin")
(require 'orin)
```

### Using use-package

```elisp
(use-package orin
  :load-path "/path/to/orin"
  :custom
  (orin-dir "~/org")  ; Set your org directory
  (orin-operating-mode 'classic)  ; or 'modern
  :bind
  ("C-c o s" . orin))
```

### Using straight.el

```elisp
(straight-use-package
 '(orin :type git :local-repo "/path/to/orin"))
```

## Quick Start

1. **Set your org directory** (if different from `~/org`):
   ```elisp
   (setq orin-dir "~/path/to/your/org/files")
   ```

2. **Run a search**:
   ```
   M-x orin RET
   ```

3. **Enter keywords** (space-separated):
   ```
   emacs lisp tutorial
   ```

4. **Navigate results**:
   - While typing: Press `M-n`/`M-p` (or `C-<down>`/`C-<up>`) to browse results in preview buffer
   - After confirming: Press `n`/`p` to navigate matches
   - Press `RET` to open the file at that location
   - Press `q` to quit

## Operating Modes

Orin supports two operating modes that provide different user experiences.

### Classic Mode

Classic mode uses dedicated Emacs buffers for search input and results display.

**Enable classic mode:**
```elisp
(setq orin-operating-mode 'classic)
```

**Features:**
- **Live preview**: As you type keywords in the minibuffer, results appear in a preview buffer
- **Debounced search**: Search triggers after a short delay (default 0.3s) to avoid lag
- **Full result buffer**: After confirming your search, results open in a dedicated buffer
- **No external dependencies**: Works with vanilla Emacs

**Workflow:**

1. Run `M-x orin`
2. Start typing keywords in the minibuffer
3. Watch the preview buffer update automatically (appears at bottom)
4. Press `M-n`/`M-p` (or `C-<down>`/`C-<up>`) to navigate through results while still in minibuffer (file preview updates automatically)
5. Press `C-c C-o` to toggle between OR/AND search modes while typing
6. Press `RET` to confirm and open the full result buffer
7. Navigate using `n`, `p`, `N`, `P` keys
8. Press `RET` on a match to open the file
9. Press `q` to quit

**Example:**
```
M-x orin RET
Search keywords: emacs configuration [preview updates as you type]
[Press RET to confirm]
```

### Modern Mode

Modern mode integrates with vertico for a more streamlined interface.

**Enable modern mode:**
```elisp
(setq orin-operating-mode 'modern)
```

**Prerequisites:**
- vertico package installed

**Features:**
- **Dynamic completion**: Results update as you type
- **Vertico integration**: Results appear in the vertico completion list
- **Grouped display**: Results grouped by file, same as classic mode
- **Live preview**: Preview buffer shows in a side window for the highlighted entry
- **Search mode toggle**: Press `C-c C-o` to toggle between OR/AND modes with instant updates
- **Familiar interface**: Uses vertico's standard navigation

**Workflow:**

1. Run `M-x orin`
2. Type keywords in the minibuffer - results update dynamically as you type
3. Results appear in vertico completion list (grouped by file)
4. Use `C-n`/`C-p` or arrow keys to navigate
5. Preview buffer updates automatically for the highlighted entry in a side window
6. Press `C-c C-o` to toggle between OR/AND search modes
7. Press `RET` to open the selected match at the correct line
8. Press `C-g` to cancel and close preview

**Example:**
```
M-x orin RET
Search keywords: project ideas [results appear in vertico as you type]
[Navigate with C-n/C-p, preview updates automatically]
[Press C-c C-o to toggle OR/AND mode - results update instantly]
[Press RET to open selected match]
```

## Search Modes

Orin supports two logical search modes for multiple keywords.

### OR Mode (Default)

Finds lines matching **any** of the keywords.

**Example:**
```
Keywords: emacs vim
Matches: "I love emacs" OR "I prefer vim" OR "emacs and vim are both great"
```

### AND Mode

Finds lines matching **all** of the keywords.

**Example:**
```
Keywords: emacs lisp
Matches: "emacs lisp programming" (contains both "emacs" AND "lisp")
Does NOT match: "emacs is great" (missing "lisp")
```

### Toggling Search Modes

While typing keywords in the minibuffer, press **`C-c C-o`** to toggle between OR and AND modes.

A message will appear: `Search mode: OR` or `Search mode: AND`

**Behavior by mode:**
- **Classic mode**: Preview buffer updates after the debounce delay
- **Modern mode**: Results update instantly in the vertico completion list

## Navigation

### In the Minibuffer (While Typing Keywords)

| Key | Action |
|-----|--------|
| `M-n` | Move to next match in preview buffer (file preview updates) |
| `M-p` | Move to previous match in preview buffer (file preview updates) |
| `C-<down>` | Move to next match in preview buffer (alternative) |
| `C-<up>` | Move to previous match in preview buffer (alternative) |
| `C-c C-o` | Toggle between OR/AND search modes |
| `RET` | Confirm search and show full result buffer |

While you're typing keywords in the minibuffer, you can use `M-n` and `M-p` (or `C-<down>` and `C-<up>`) to navigate through the matches shown in the preview buffer. As you navigate, the file preview window automatically updates to show the content at the selected match. This allows you to browse results without leaving the minibuffer or switching windows.

### In the Result Buffer

| Key | Action |
|-----|--------|
| `n` | Move to next match |
| `p` | Move to previous match |
| `N` | Move to first match of next group (file) |
| `P` | Move to first match of previous group (file) |
| `RET` | Open file at current match (centered in window) |
| `q` | Quit result buffer and close all associated windows |

### Understanding Groups

Results are grouped by file. Each group shows:
- **Group header**: The file's `#+title:` property (for .org files) or filename
- **Matches**: Indented list showing line number and matching text

**Example:**
```
My Project Notes                    ← Group header (from #+title:)
   42: This is an emacs tutorial    ← Match (line 42)
   57: Another emacs tip here       ← Match (line 57)

ideas.org                            ← Group header (filename)
   12: Cool emacs plugin idea       ← Match (line 12)
```

Pressing `N` from line 42 would jump to line 12 (first match of next group).

### Preview Behavior

**Classic mode (in result buffer):**
- Preview updates automatically as you navigate with `n`/`p`/`N`/`P`
- Shows the file content with matching line centered
- Matching line and keywords are highlighted

**Modern mode (in vertico):**
- Preview updates as you move through the completion list
- Same highlighting as classic mode

**Classic mode (during input):**
- Preview buffer shows results as you type
- Use `M-n`/`M-p` (or `C-<down>`/`C-<up>`) to navigate through results in preview buffer
- File preview window automatically opens for the first match and updates as you navigate
- All matching keywords are highlighted in both buffers

## Customization

### Variables

#### `orin-dir`
Directory to search for .org files.

```elisp
(setq orin-dir "~/Documents/org")
```

**Default:** `"~/org"`

#### `orin-operating-mode`
Operating mode: `'classic` or `'modern`.

```elisp
(setq orin-operating-mode 'modern)  ; Use vertico integration
```

**Default:** `'classic`

#### `orin-debounce-delay`
Delay in seconds before triggering search during input.

```elisp
(setq orin-debounce-delay 0.5)  ; Wait 0.5s after typing stops
```

**Default:** `0.3`

Lower values = more responsive but potentially laggy
Higher values = less responsive but smoother

#### `orin-ripgrep-executable`
Path to the ripgrep executable.

```elisp
(setq orin-ripgrep-executable "/usr/local/bin/rg")
```

**Default:** `"rg"`

### Faces

Customize the appearance of search results:

#### `orin-match-line-face`
Face for the matching line in preview buffer.

```elisp
(set-face-attribute 'orin-match-line-face nil
                    :background "#3a3a3a"
                    :foreground "#ffffff")
```

**Default:** Inherits from `highlight`

#### `orin-match-keyword-face`
Face for highlighted keywords in preview buffer.

```elisp
(set-face-attribute 'orin-match-keyword-face nil
                    :background "#ffff00"
                    :foreground "#000000"
                    :weight 'bold)
```

**Default:** Inherits from `match`

#### `orin-group-name-face`
Face for group headers in result buffer.

```elisp
(set-face-attribute 'orin-group-name-face nil
                    :foreground "#ff8800"
                    :weight 'extra-bold)
```

**Default:** Inherits from `font-lock-function-name-face` with bold weight

### Example Configuration

```elisp
(use-package orin
  :load-path "~/.emacs.d/lisp/orin"
  :custom
  (orin-dir "~/Dropbox/org")
  (orin-operating-mode 'classic)
  (orin-debounce-delay 0.2)
  :custom-face
  (orin-match-line-face ((t (:background "#2d4f67" :extend t))))
  (orin-match-keyword-face ((t (:background "#ffa500" :foreground "#000" :weight bold))))
  (orin-group-name-face ((t (:foreground "#4ec9b0" :weight extra-bold :height 1.1))))
  :bind
  (("C-c o s" . orin)
   :map orin-result-mode-map
   ("C-n" . orin-next-match)
   ("C-p" . orin-previous-match)))
```

## Tips and Tricks

### 1. Search for Phrases

Since keywords are space-separated, searching for phrases requires listing all words:

```
Search keywords: project management workflow
```

This finds lines containing all three words (in AND mode) or any of them (in OR mode).

### 2. Use AND Mode for Precise Searches

When you have many .org files, AND mode helps narrow down results:

```
Search keywords: budget Q4 2024
[Press C-c C-o to toggle to AND mode]
```

This finds only lines containing all three terms.

### 3. Preview Results While Typing

The most efficient workflow in classic mode:
1. Start typing keywords
2. File preview automatically opens for the first match
3. Use `M-n`/`M-p` (or `C-<down>`/`C-<up>`) to browse through results while still in minibuffer
4. Watch the file preview update automatically
5. If you find what you need, press `RET` to open the file
6. If not, continue refining your keywords

This lets you explore results without committing to the full result buffer.

### 4. Quickly Switch Between Results and Files

In classic mode (after confirming search):
1. Navigate to interesting matches with `n`/`p`
2. Press `RET` to open the file
3. Press `C-x o` to switch back to the result buffer
4. Continue navigating to other matches

### 5. Organize with #+title:

Add `#+title:` to your .org files for better group headers:

```org
#+title: My Personal Wiki
#+author: Your Name

* Content here...
```

This shows "My Personal Wiki" instead of "wiki.org" in search results.

### 6. Performance Tuning

If you have many .org files and search feels slow:

**Increase debounce delay:**
```elisp
(setq orin-debounce-delay 0.5)  ; Wait longer before searching
```

**Use more specific keywords:**
Instead of searching for "emacs", search for "emacs workflow configuration"

### 7. Integration with Other Tools

**Capture search results:**
```elisp
(defun my-orin-capture-match ()
  "Capture current orin match to org-capture."
  (interactive)
  (let ((match (get-text-property (point) 'orin-match)))
    (when match
      (let ((file (plist-get match :file))
            (line (plist-get match :line))
            (text (plist-get match :text)))
        (org-capture nil "r")  ; Assumes you have a "r" template
        (insert (format "[[file:%s::%d][%s]]" file line text))))))
```

**Search from dired:**
```elisp
(defun my-orin-from-dired ()
  "Search in the directory of current dired buffer."
  (interactive)
  (let ((orin-dir default-directory))
    (call-interactively 'orin)))
```

### 8. Multiple Org Directories

If you have org files in multiple directories, create wrapper functions:

```elisp
(defun orin-work ()
  "Search in work org files."
  (interactive)
  (let ((orin-dir "~/work/org"))
    (call-interactively 'orin)))

(defun orin-personal ()
  "Search in personal org files."
  (interactive)
  (let ((orin-dir "~/personal/org"))
    (call-interactively 'orin)))

(global-set-key (kbd "C-c o w") 'orin-work)
(global-set-key (kbd "C-c o p") 'orin-personal)
```

### 9. Exclude Certain Files

Ripgrep respects `.gitignore` by default. To exclude additional files, create a `.ignore` file in your org directory:

```
# .ignore file
archive/
*.draft.org
scratch.org
```

### 10. Smart Case Search

Orin uses ripgrep's smart-case mode by default:
- **Lowercase keywords**: Match case-insensitively (e.g., "emacs" matches "emacs", "Emacs", "EMACS")
- **Mixed case keywords**: Match case-sensitively (e.g., "Emacs" only matches "Emacs")

To make searches always case-sensitive, you would need to modify `orin--run-ripgrep` in `orin.el` to change `--smart-case` to `--case-sensitive`.

To make searches always case-insensitive, change `--smart-case` to `--ignore-case` or `-i`.

### 11. Keyboard-Driven Workflow

For maximum efficiency, avoid using the mouse:

1. Bind orin to a convenient key: `(global-set-key (kbd "C-c s") 'orin)`
2. Type keywords quickly
3. Use `M-n`/`M-p` (or `C-<down>`/`C-<up>`) to browse results while typing
4. Use `C-c C-o` to toggle search mode without lifting hands
5. Navigate with `n`/`p`/`N`/`P` in result buffer
6. Open with `RET`, quit with `q`

## Troubleshooting

### "ripgrep executable not found"

Make sure ripgrep is installed:
```bash
# macOS
brew install ripgrep

# Ubuntu/Debian
sudo apt install ripgrep

# Arch Linux
sudo pacman -S ripgrep
```

Or set the full path:
```elisp
(setq orin-ripgrep-executable "/usr/local/bin/rg")
```

### "Modern mode not working" or errors about vertico

Modern mode requires vertico to be installed and enabled. Install vertico:
```elisp
(use-package vertico
  :ensure t
  :init
  (vertico-mode))
```

Or switch to classic mode:
```elisp
(setq orin-operating-mode 'classic)
```

### Search is too slow

1. Increase debounce delay: `(setq orin-debounce-delay 0.5)`
2. Use more specific keywords to reduce result set
3. Check if your org directory has very large files
4. Consider excluding directories with `.ignore` file

### Preview buffer not updating

Make sure you're on a match line (not a group header). Only match lines trigger previews.

In classic mode during input, previews only show in the result preview buffer, not file previews.

---

## Contributing

Found a bug or have a feature request? Please open an issue on the repository.

## License

See LICENSE file for details.
