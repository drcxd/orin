# `orin`

`orin` is an Emacs Lisp package that helps you search information in
`.org` files. The name comes from "ORg INsight". (It is also the name
of a villain in Baldur's Gate 3, one of my favorite games.)

## Implementation

Overall instructions:

+ Implement all code in orin.el
+ Follow Emacs Lisp package development convention
+ Add docstring to every variable and function
+ Byte compile the file and eliminate all warnings and errors
+ Performance is important, since we are using `ripgrep` to search, do
  not start searching every time the user input has changed, so that
  Emacs feels dragging. Come up with a solution for this problem.

### Main Function

This package contains a main function `orin`. It takes a string, which
is a list of keywords separated by spaces. Then it searches those
keywords in a directory specified by `orin-dir` recursively using
`ripgrep` and display the results in a buffer in a new window. We will
refer to this buffer as the result buffer from now on.

### The Result Buffer

In the results buffer, the matching results are grouped by files. Each
group consists of a line of the group name and several lines for the
matching results. Within each group, results are sorted by line number
in ascending order. The group name is usually the name of the
file. However, if the filename ends with `.org`, and it contains
`#+title: ` line in it, use this title property as the name of the
group.

In the result buffer, the user can press `n` and `p` to move cursor
between matching results, `N` and `P` to move between groups. Pressing
`N` moves the cursor to the first matching results of the next group,
and pressing `P` moves the cursor to the first matching results of the
previous group. Pressing Enter on a matching result opens the file in
its own buffer in a new window and display the matching line at the
middle of the window. Pressing `q` quits the result buffer, closing
the window of it and the window of the preview buffer as well.

When the cursor is on a matching result, it is "highlighted". Note
that only matching results can be highlighted, the group name never
gets highlighted. When a matching result is highlighted, open a buffer
in a new window displaying the matching line at the middle of the
window. This buffer is called the matching preview buffer, its details
is discussed in the next section.

### The Matching Preview Buffer

The matching preview buffer displays the content of the matching file,
with the matching line placed at the center of the window. The
matching line and keywords are highlighted using faces that can be
customized by the users.

Note that the content of the matching file should be read without
opening the file in a persistent buffer.

### Searching Mode

Since we may have multiple keywords, we support logical OR or logical
AND of these keywords as different searching mode. The searching mode
is logical OR by default. But the user can press `C-c C-o` to toggle
between logical OR and logical AND when inputting the keywords.

Searching uses ripgrep's smart-case mode: lowercase keywords match
case-insensitively, while mixed-case keywords match case-sensitively.

### Preview of the Searching Results

When the user is inputting keywords, use another buffer to display the
searching results in another buffer in a new window. This buffer is
called the result preview buffer. The result preview buffer should be
identical to the result buffer. The interfaces are also identical. The
only difference is when the user moving cursors between matching
results, the matching preview buffer is not opened.

While the user is still in the minibuffer inputting keywords, they can
press `M-n` and `M-p` (or `C-<down>` and `C-<up>`) to navigate between
matches in the result preview buffer. As they navigate, the matching
preview buffer updates to show the file content at the selected match
location. This allows users to browse results without leaving the
minibuffer or switching windows.

### Integration with vertico

What we have described above is called the classic operating mode of
the package. The user can use a variable to switch to another
operating mode using vertico. This operating mode is called the modern mode.

In the modern mode, when the user is inputting keywords, the searching
results are displayed using vertico's completion interface with dynamic
updates as you type. Matching results are grouped by file using the same
pattern as the result buffer. When any entry in the vertico list is
highlighted, a matching preview buffer opens for that entry in a side
window (split vertically). When the user presses enter on any entry,
the file opens at the matching line in a new window. Pressing `C-g` in
vertico cancels the search and closes the preview window. The user can
press `C-c C-o` while typing to toggle between OR and AND search modes,
and the results update immediately.
