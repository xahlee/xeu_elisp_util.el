;;; xeu_elisp_util.el --- xah's misc elisp utility. -*- coding: utf-8 -*-

;; Copyright © 2011, 2012 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Created: 2011-03-02
;; Keywords: emacs lisp, utility, file

;; You can redistribute this program and/or modify it. Please give credit and link. Thanks.

;;; DESCRIPTION

;; this package is some misc emacs lisp utility.
;; call list-matching-lines with “defun ”
;; to see a list of functions defined

;; The most used two are “unit-at-cursor” and “get-selection-or-unit”. They are intended as improvemnt of “thing-at-point”. For detailed discussion, see:〈Emacs Lisp: get-selection-or-unit〉 @ http://ergoemacs.org/emacs/elisp_get-selection-or-unit.html

;; This package requires 〔xfrp_find_replace_pairs.el〕
;; donate $3 please. Paypal to xah@xahlee.org , thanks.

;;; INSTALL

;; Place the file in your emacs load path. Then
;; (require 'xfrp_find_replace_pairs)
;; (require 'xeu_elisp_util)

;;; HISTORY

;; 2014-08-20 changes are no longer logged here. See git log instead. This is a hobby code, don't have time to write details.
;; version 1.4.21, 2014-04-24 modified some inline doc to reflect emacs 24.4's new functions
;; version 1.4.20, 2014-01-21 “unit-at-cursor” with 'filepath argument now also consider single quote as delimiter
;; version 1.4.19, 2013-05-10 “get-html-file-title” moved to xah-html-mode.el and name is now “xhm-get-html-file-title”
;; version 1.4.18, 2013-02-22 removed delete-subdirs-by-regex and delete-files-by-regex . These are either buggy or very inefficient. Pending work.
;; version 1.4.17, 2013-01-27 for unit-at-cursor for arg 'filepath, added no-break space as delimiter.
;; version 1.4.16, 2012-12-29 changed implementation for unit-at-cursor for arg 'filepath
;; version 1.4.15, 2012-08-23 added “file-relative-name-emacs24.1.1-fix”
;; version 1.4.14, 2012-08-14 added “hash-to-list”.
;; version 1.4.13, 2012-07-03 removed curly bracket for 'filepath in “unit-at-cursor”.
;; version 1.4.12, 2012-06-30 added “current-date-time-string”. Added 'url, 'filepath to “unit-at-cursor”.
;; version 1.4.11, 2012-05-05 added { “delete-subdirs-by-regex” “delete-files-by-regex”}
;; version 1.4.10, 2012-05-05 added “substract-path”.
;; version 1.4.9, 2012-03-15 more trivial improved implementation of “get-image-dimensions-imk”.
;; version 1.4.8, 2012-03-03 trivially improved implementation of “get-image-dimensions-imk”.
;; version 1.4.7, 2011-11-26 major change on “get-image-dimensions”. It now supports svn and gif. For gif, it calls “get-image-dimensions-imk”.
;; version 1.4.6, 2011-11-18 Added a “title-case-string-region-or-line”.
;; version 1.4.5, 2011-11-14 corrected a critical error in “asciify-text”.
;; version 1.4.4, 2011-11-14 added function “asciify-text”.
;; version 1.4.3, 2011-11-06 unit-at-cursor with 「'block」 argument will work when the text block is at beginning/end of buffer. Also, lines with just space or tab is also considered a empty line.
;; version 1.4.2, 2011-10-30 trivial implementation change on “xhm-get-html-file-title”. No user visible effect.
;; version 1.4.1, 2011-09-29 fixed a error in “trim-string”.
;; version 1.4, 2011-09-16 added “trim-string”.
;; version 1.3, 2011-08-27 fixed a bug in “unit-at-cursor” when argument is 「'block」. Now it doesn't grab a extra line ending.
;; version 1.2, 2011-07-02 inline doc improvement for “get-image-dimensions” “get-image-dimensions-imk”.
;; version 1.1, 2011-05-28 Added some comment in source code.
;; version 1.0, 2011-03-02 First version.


;;; Code:

(defun unit-at-cursor (φunit)
  "Return the string and boundary of ΦUNIT under cursor.

Returns a vector [text a b], where text is the string and a and b are its boundary.

ΦUNIT can be:

• 'word — sequence of 0 to 9, A to Z, a to z, and hyphen.

• 'glyphs — sequence of visible glyphs. Useful for file name, URL, …, anything doesn't have white spaces in it.

• 'line — delimited by “\\n”. (captured text does not include a ending “\\n”.)

• 'block — delimited by empty lines or beginning/end of buffer. Lines with just spaces or tabs are also considered empty line. (captured text does not include a ending “\\n”.)

• 'buffer — whole buffer. (respects `narrow-to-region')

• 'filepath — delimited by chars that's USUALLY not part of filepath.

• 'url — delimited by chars that's USUALLY not part of URL.

• a vector [beginRegex endRegex] — The elements are regex strings used to determine the beginning/end of boundary chars. They are passed to `skip-chars-backward' and `skip-chars-forward'. For example, if you want paren as delimiter, use [\"^(\" \"^)\"]

Example usage:
 (setq bds (unit-at-cursor 'line))
 (setq inputstr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )

This function is similar to `thing-at-point' and `bounds-of-thing-at-point'.
The main differences are:

• This function returns the text and the 2 boundaries as a vector in one shot.

• 'line always returns the line without end of line character, avoiding inconsistency when the line is at end of buffer.

• This function's behavior does not depend on syntax table. e.g. for units 「'word」, 「'block」, etc."
  (let (p1 p2)
    (save-excursion
      (cond
       ( (eq φunit 'word)
         (let ((wordcharset "-A-Za-z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ"))
           (skip-chars-backward wordcharset)
           (setq p1 (point))
           (skip-chars-forward wordcharset)
           (setq p2 (point))))

       ( (eq φunit 'glyphs)
         (progn
           (skip-chars-backward "[:graph:]")
           (setq p1 (point))
           (skip-chars-forward "[:graph:]")
           (setq p2 (point))))

       ((eq φunit 'buffer)
        (progn
          (setq p1 (point-min))
          (setq p2 (point-max))))

       ((eq φunit 'line)
        (progn
          (setq p1 (line-beginning-position))
          (setq p2 (line-end-position))))
       ((eq φunit 'block)
        (progn
          (if (re-search-backward "\n[ \t]*\n" nil "move")
              (progn (re-search-forward "\n[ \t]*\n")
                     (setq p1 (point)))
            (setq p1 (point)))
          (if (re-search-forward "\n[ \t]*\n" nil "move")
              (progn (re-search-backward "\n[ \t]*\n")
                     (setq p2 (point)))
            (setq p2 (point)))))

       ((eq φunit 'filepath)
        (let (p0)
          (setq p0 (point))
          ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
          (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
          (setq p2 (point))))

       ((eq φunit 'url)
        (let (p0
              ;; (ξdelimitors "^ \t\n,()[]{}<>〔〕“”\"`'!$^*|\;")
              (ξdelimitors "!\"#$%&'*+,-./0123456789:;=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"))
          (setq p0 (point))
          (skip-chars-backward ξdelimitors) ;"^ \t\n,([{<>〔“\""
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward ξdelimitors) ;"^ \t\n,)]}<>〕\"”"
          (setq p2 (point))))

       ((vectorp φunit)
        (let (p0)
          (setq p0 (point))
          (skip-chars-backward (elt φunit 0))
          (setq p1 (point))
          (goto-char p0)
          (skip-chars-forward (elt φunit 1))
          (setq p2 (point))))))

    (vector (buffer-substring-no-properties p1 p2) p1 p2 )))

(defun get-selection-or-unit (φunit)
  "Return the string and boundary of text selection or ΦUNIT under cursor.

If `use-region-p' is true, then the region is the φunit.  Else,
it depends on the ΦUNIT. See `unit-at-cursor' for detail about
ΦUNIT.

Returns a vector [text a b], where text is the string and a and b
are its boundary.

Example usage:
 (setq bds (get-selection-or-unit 'line))
 (setq inputstr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )"
  (interactive)
  (if (use-region-p)
      (let ((p1 (region-beginning)) (p2 (region-end)))
        (vector (buffer-substring-no-properties p1 p2) p1 p2 ))
    (unit-at-cursor φunit)))



(defun xah-filter-list (φpredicate φlist)
  "Return a new list such that φpredicate is true on all members of φlist.
Note: φlist should not have a element equal to the string \"e3824ad41f2ec1ed\"."
  (let ((ξresult (mapcar (lambda (ξx) (if (funcall φpredicate ξx) ξx "e3824ad41f2ec1ed" )) φlist)))
    (setq ξresult (delete "e3824ad41f2ec1ed" ξresult))
    ξresult
    ))

;; (xah-string-match-in-list-p
;; "/home/xah/web/xahlee_info/css_2.1_spec/propidx.html"
;;  '("/home/xah/web/xahlee_info/php/php_install.html"
;; "/home/xah/web/xahlee_info/css_2.1_spec/"
;; "/home/xah/web/xahlee_info/php/keyed_list.html"
;; "/home/xah/web/xahlee_info/php/mysql.html"
;; "/home/xah/web/xahlee_info/php/misc.html" )
;; "yes"
;; nil)

(defun xah-string-match-in-list-p (φstr φlist-of-string φmatch-case-p &optional φreverse-match-p)
  "Return the first element in φlist-of-string if φstr occur in φlist-of-string, else false.

if φreverse-match-p is true, change the direction of match. That is, true if any element in φlist-of-string occur in φstr.

φmatch-case-p determines whether case is literal for the match.
No regex is used.
Existing match data is changed. Wrap it with `save-match-data' if you need it restored."
  (let ((case-fold-search (not φmatch-case-p)))
    (if φreverse-match-p
        (progn
          (catch 'myTagName
            (mapc
             (lambda (ξx)
               (when (string-match (regexp-quote ξx) φstr ) (throw 'myTagName ξx)))
             φlist-of-string)
            nil))
      (progn
        (catch 'myTagName
          (mapc
           (lambda (ξx)
             (when (string-match (regexp-quote φstr) ξx ) (throw 'myTagName ξx)))
           φlist-of-string)
          nil)))))



(defun get-image-dimensions (φfile-path)
  "Returns a image file's width and height as a vector.
Support png jpg svg gif and any image type emacs supports.
 (for gif, it calls `get-image-dimensions-imk')
Bug: for large size png, sometimes this returns a wrong dimension 30×30."
  (let (ξx ξy)
    (cond
     ((string-match "\.gif$" φfile-path) (get-image-dimensions-imk φfile-path))
     ((string-match "\.svg$" φfile-path)
      (with-temp-buffer
        (insert-file-contents φfile-path)
        (goto-char (point-min))
        (search-forward-regexp "width=\"\\([0-9]+\\).*\"")
        (setq ξx (match-string 1 ))
        (goto-char (point-min))
        (search-forward-regexp "height=\"\\([0-9]+\\).*\"")
        (setq ξy (match-string 1 ))
        (vector (string-to-number ξx) (string-to-number ξy))))
     (t (let (ξxy )
          (progn
            (clear-image-cache t)
            (setq ξxy (image-size
                       (create-image
                        (if (file-name-absolute-p φfile-path)
                            φfile-path
                          (concat default-directory φfile-path)))
                       t)))
          (vector (car ξxy) (cdr ξxy)))))))

;; (defun get-image-dimensions-imk (φimg-file-path)
;;   "Returns a image file's width and height as a vector.
;; This function requires ImageMagick's “identify” shell command.
;; See also: `get-image-dimensions'."
;;   (let (cmd-name sh-output width height)
;;     (setq cmd-name "identify")
;;     (setq sh-output (shell-command-to-string (concat cmd-name " " φimg-file-path)))
;;     ;;  sample output from “identify”:  “xyz.png PNG 520x429+0+0 DirectClass 8-bit 9.1k 0.0u 0:01”
;;     (string-match "^[^ ]+ [^ ]+ \\([0-9]+\\)x\\([0-9]+\\)" sh-output)
;;     (setq width (match-string 1 sh-output))
;;     (setq height (match-string 2 sh-output))
;;     (vector (string-to-number width) (string-to-number height))))

(defun get-image-dimensions-imk (φimg-file-path)
  "Returns a image file's width and height as a vector.
This function requires ImageMagick's “identify” shell command.
See also: `get-image-dimensions'."
  (let ( widthHeightList )
    (setq widthHeightList (split-string (shell-command-to-string (concat "identify -format \"%w %h\" " φimg-file-path))))
    (vector
     (string-to-number (elt widthHeightList 0))
     (string-to-number (elt widthHeightList 1)))))


(defun get-string-from-file (φfile-path)
  "Return φfile-path's content."
;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman <zzbba...@aol.com>”. 2010-09-02
  (with-temp-buffer
    (insert-file-contents φfile-path)
    (buffer-string)))

(defun read-lines (φfile-path)
  "Return a list of lines of a file at φfile-path."
  (with-temp-buffer
    (insert-file-contents φfile-path)
    (split-string (buffer-string) "\n" t)))



;; 2013-02-21 INCORRECT behavior.
;(defun delete-subdirs-by-regex (φdir φregex)
;  "Delete sub-directories in φdir whose path matches ΦREGEX."
;  (require 'find-lisp)
;  (mapc
;   (lambda (ξx) (when (file-directory-p ξx)
;;;(delete-directory ξx t)
;                  (print ξx)
;                  ))
;   (find-lisp-find-files φdir φregex)) )

(defun delete-files-by-regex (φdir φregex)
  "Delete files in a φdir whose file name (not full path) matches a φregex.
 Example:
  (delete-files-by-regex \"~/web\" \"~$\") ; remove files ending in ~
"
  (require 'find-lisp)
  (mapc (lambda (ξx) (if (file-regular-p ξx) (delete-file ξx)))
        (find-lisp-find-files φdir φregex)))

(defun file-relative-name-emacs24.1.1-fix (φfile-path φdir-path)
  "fix for `file-relative-name'. If path start with cap such as “C:” (Windows file path), it won't work.
e.g.
 (file-relative-name \"c:/Users/h3/.emacs.d/test.el\" \"c:/Users/h3/.emacs.d/\" )
 (file-relative-name \"C:/Users/h3/.emacs.d/test.el\" \"C:/Users/h3/.emacs.d/\" ) ⇒ \"C:/Users/h3/.emacs.d/test.el\"
GNU Emacs 24.1.1 (i386-mingw-nt6.1.7601) of 2012-06-10 on MARVIN
"
  (file-relative-name
   (replace-regexp-in-string "\\`C:/" "c:/" φfile-path  "FIXEDCASE" "LITERAL") φdir-path ))



;; (require 'subr-x)

(defun trim-string (φstring)
  "Remove white spaces in beginning and ending of φstring.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10).

Note: in emacs GNU Emacs 24.4+ and later, there's `string-trim' function. You need to (require 'subr-x).
"
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" φstring)))

(defun substract-path (φpath1 φpath2)
  "Remove string φpath2 from the beginning of φpath1.
length of φpath1 ≥ to length φpath2.

⁖  (substract-path \"c:/Users/lisa/web/a/b\" \"c:/Users/lisa/web/\") ⇒ \"a/b\"
This is the roughly the same as emacs 24.4's `string-remove-prefix'.
 (require 'subr-x)
 (string-remove-prefix  \"c:/Users/lisa/web/\" \"c:/Users/lisa/web/a/b\" )
"
  (let ((p2length (length φpath2)))
    (if (string= (substring φpath1 0 p2length) φpath2 )
        (substring φpath1 p2length)
      (error "error 34689: beginning doesn't match: 「%s」 「%s」" φpath1 φpath2))))

(defun hash-to-list (φhashtable)
  "Return a list that represent the φhashtable.
Each element is a list: (list key value).

See also, emacs 24.4's new functions.
 (require 'subr-x)
 `hash-table-keys'
 `hash-table-values'
"
  (let (mylist)
    (maphash (lambda (kk vv) (setq mylist (cons (list kk vv) mylist))) φhashtable)
    mylist))



(defun xah-asciify-region (&optional φfrom φto)
  "Change European language characters into equivalent ASCII ones, ⁖ “café” ⇒ “cafe”.

This command does not transcode all Unicode chars such as Greek, math symbols. They remains.

When called interactively, work on text selection or current line.
URL `http://ergoemacs.org/emacs/emacs_zap_gremlins.html'
Version 2014-10-20"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((ξcharMap [
                         ["á\\|à\\|â\\|ä\\|ã\\|å" "a"]
                         ["é\\|è\\|ê\\|ë" "e"]
                         ["í\\|ì\\|î\\|ï" "i"]
                         ["ó\\|ò\\|ô\\|ö\\|õ\\|ø" "o"]
                         ["ú\\|ù\\|û\\|ü"     "u"]
                         ["Ý\\|ý\\|ÿ"     "y"]
                         ["ñ" "n"]
                         ["ç" "c"]
                         ["ð" "d"]
                         ["þ" "th"]
                         ["ß" "ss"]
                         ["æ" "ae"]
                         ]))
    (let ((case-fold-search t))
        (save-restriction
          (narrow-to-region φfrom φto)
          (mapc
           (lambda (ξpair)
             (goto-char (point-min))
             (while (search-forward-regexp (elt ξpair 0) (point-max) t)
               (replace-match (elt ξpair 1))))
           ξcharMap)))))

(defun xah-asciify-string (φstring)
  "Returns a new string. European language chars are changed ot ASCII ones ⁖ “café” ⇒ “cafe”. 
See `xah-asciify-region'
Version 2014-10-20"
  (with-temp-buffer 
      (insert φstring)
      (xah-asciify-region (point-min) (point-max))
      (buffer-string)))

;; (defun asciify-text-iconv ()
;; "Convert STRING to ASCII string.
;; For example:
;;  passé ⇒ passe
;;  -> ⇒ →
;; this command calls the Unix iconv program.
;; Code originally by Teemu Likonen."
;; (interactive
;;    (let ((bds (get-selection-or-unit 'block)))
;;      (list
;; str (elt bds 0)
;; p1 (elt bds 1)
;; p2 (elt bds 2)
;;   ) ) )
;;   (with-temp-buffer
;;     (insert string)
;;     (call-process-region (point-min) (point-max) "iconv" t t nil "--to-code=ASCII//TRANSLIT")
;;     (buffer-substring-no-properties (point-min) (point-max))))

(defun title-case-string-region-or-line (φstring &optional φregion-boundary)
  "Capitalize the current line or text selection, following title conventions.

Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, if φregion-boundary is nil, returns the changed φstring, else, work on the region. φregion-boundary is a pair [from to], it can be a vector or list."
  (interactive
   (let ((bds (get-selection-or-unit 'line)))
     (list nil (vector (elt bds 1) (elt bds 2)))))

  (let (
        (strPairs '(
                    [" A " " a "]
                    [" And " " and "]
                    [" At " " at "]
                    [" As " " as "]
                    [" By " " by "]
                    [" Be " " be "]
                    [" Into " " into "]
                    [" In " " in "]
                    [" Is " " is "]
                    [" It " " it "]
                    [" For " " for "]
                    [" Of " " of "]
                    [" Or " " or "]
                    [" On " " on "]
                    [" The " " the "]
                    [" That " " that "]
                    [" To " " to "]
                    [" Vs " " vs "]
                    [" With " " with "]
                    [" From " " from "]
                    ["'S " "'s "]
                    ))
        (workOnStringP (if φregion-boundary nil t ))
        (p1 (elt φregion-boundary 0))
        (p2 (elt φregion-boundary 1)))

    (let ((case-fold-search nil))
      (if workOnStringP
          (progn
            (replace-pairs-in-string-recursive (upcase-initials φstring) strPairs))
        (progn
          (save-restriction
            (narrow-to-region p1 p2)
            (upcase-initials-region (point-min) (point-max))
            (replace-regexp-pairs-region (point-min) (point-max) strPairs t t)))))))



(defvar month-full-names '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December") "list of English month full names.")

(defvar month-abbrev-names (mapcar (lambda (x) (substring x 0 3)) month-full-names) "list of English month 3-letter abbrev names.")

(defvar weekday-names '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday") "list of English weekday full names.")

(defun insert-date (&optional φadd-time-stamp-p)
  "Insert current date and or time.

• In this format yyyy-mm-dd.
• When called with `universal-argument', insert date and time, e.g. 2012-05-28T07:06:23-07:00
• Replaces text selection.

See also `current-date-time-string'."
  (interactive "P")
  (when (use-region-p) (delete-region (region-beginning) (region-end) ) )
  (cond
   ((equal φadd-time-stamp-p nil ) (insert (format-time-string "%Y-%m-%d")))
   (t (insert (current-date-time-string))) ) )

(defun current-date-time-string ()
  "Returns current date-time string in full ISO 8601 format.
Example: 「2012-04-05T21:08:24-07:00」.

Note, for the time zone offset, both the formats 「hhmm」 and 「hh:mm」 are valid ISO 8601. However, Atom Webfeed spec seems to require 「hh:mm」."
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (ξx) (format "%s:%s" (substring ξx 0 3) (substring ξx 3 5))) (format-time-string "%z"))))

(defun is-datetimestamp-p (φinput-string)
  "Return t if φinput-string is a date/time stamp, else nil.
This is based on heuristic, so it's not 100% correct.
If the string contains any month names, weekday names, or of the form dddd-dd-dd, dddd-dd-dddd, dddd-dd-dd, or using slash, then it's considered a date.
"
  (cond
         ((string-match (regexp-opt (append month-full-names month-abbrev-names weekday-names) 'words) φinput-string) t)
         ;; mm/dd/yyyy
         ((string-match "\\b[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]\\b" φinput-string) t)
         ;; yyyy/mm/dd
         ((string-match "\\b[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\b" φinput-string) t)
         ;; mm/dd/yy
         ((string-match "\\b[0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\b" φinput-string) t)
         ;; mm-dd-yyyy
         ((string-match "\\b[0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9]\\b" φinput-string) t)
         ;; yyyy-mm-dd
         ((string-match "\\b[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\b" φinput-string) t)
         ;; mm-dd-yy
         ((string-match "\\b[0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\b" φinput-string) t)
         (t nil) ))

(defun fix-datetimestamp (φinput-string &optional φfrom-to)
  "Change timestamp under cursor into a yyyy-mm-dd format.
If there's a text selection, use that as input, else use current line.

Any “day of week”, or “time” info, or any other parts of the string, are discarded.
For example:
 「TUESDAY, FEB 15, 2011 05:16 ET」 ⇒ 「2011-02-15」
 「November 28, 1994」              ⇒ 「1994-11-28」
 「Nov. 28, 1994」                  ⇒ 「1994-11-28」
 「11/28/1994」                     ⇒ 「1994-11-28」
 「1994/11/28」                     ⇒ 「1994-11-28」

When called in lisp program, the optional second argument “φfrom-to” is a vector [from to] of region boundary. (it can also be a list)
If “φfrom-to” is non-nil, the region is taken as input (and “φinput-string” is ignored).

Code detail: URL `http://ergoemacs.org/emacs/elisp_parse_time.html'"
  (interactive
   (progn
     (require 'xeu_elisp_util)
     (let ((bds (get-selection-or-unit 'line)))
       (list nil (vector (elt bds 1) (elt bds 2))))))
  (let (
        (ξstr (if φfrom-to (buffer-substring-no-properties (elt φfrom-to 0) (elt φfrom-to 1)) φinput-string))
        (workOnRegionP (if φfrom-to t nil)))
    (require 'parse-time)

    (setq ξstr (replace-regexp-in-string "^ *\\(.+\\) *$" "\\1" ξstr)) ; remove white spaces

    (setq ξstr
          (cond
           ;; USA convention of mm/dd/yyyy
           ((string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)" ξstr)
            (concat (match-string 3 ξstr) "-" (match-string 1 ξstr) "-" (match-string 2 ξstr)))
           ;; USA convention of m/dd/yyyy
           ((string-match "\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)" ξstr)
            (concat (match-string 3 ξstr) "-0" (match-string 1 ξstr) "-" (match-string 2 ξstr)))

           ;; USA convention of mm/dd/yy
           ((string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" ξstr)
            (concat (format-time-string "%C") (match-string 3 ξstr) "-" (match-string 1 ξstr) "-" (match-string 2 ξstr)))
           ;; USA convention of m/dd/yy
           ((string-match "\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" ξstr)
            (concat (format-time-string "%C") (match-string 3 ξstr) "-0" (match-string 1 ξstr) "-" (match-string 2 ξstr)))

           ;; yyyy/mm/dd
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" ξstr)
            (concat (match-string 1 ξstr) "-" (match-string 2 ξstr) "-" (match-string 3 ξstr)))

           ;; some ISO 8601. yyyy-mm-ddThh:mm
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)T[0-9][0-9]:[0-9][0-9]" ξstr)
            (concat (match-string 1 ξstr) "-" (match-string 2 ξstr) "-" (match-string 3 ξstr)))
           ;; some ISO 8601. yyyy-mm-dd
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)" ξstr)
            (concat (match-string 1 ξstr) "-" (match-string 2 ξstr) "-" (match-string 3 ξstr)))
           ;; some ISO 8601. yyyy-mm
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)" ξstr)
            (concat (match-string 1 ξstr) "-" (match-string 2 ξstr)))

           ;; else
           (t
            (progn
              (setq ξstr (replace-regexp-in-string "January " "Jan. " ξstr))
              (setq ξstr (replace-regexp-in-string "February " "Feb. " ξstr))
              (setq ξstr (replace-regexp-in-string "March " "Mar. " ξstr))
              (setq ξstr (replace-regexp-in-string "April " "Apr. " ξstr))
              (setq ξstr (replace-regexp-in-string "May " "May. " ξstr))
              (setq ξstr (replace-regexp-in-string "June " "Jun. " ξstr))
              (setq ξstr (replace-regexp-in-string "July " "Jul. " ξstr))
              (setq ξstr (replace-regexp-in-string "August " "Aug. " ξstr))
              (setq ξstr (replace-regexp-in-string "September " "Sep. " ξstr))
              (setq ξstr (replace-regexp-in-string "October " "Oct. " ξstr))
              (setq ξstr (replace-regexp-in-string "November " "Nov. " ξstr))
              (setq ξstr (replace-regexp-in-string "December " "Dec. " ξstr))

              (setq ξstr (replace-regexp-in-string "\\([0-9]+\\)st" "\\1" ξstr))
              (setq ξstr (replace-regexp-in-string "\\([0-9]+\\)nd" "\\1" ξstr))
              (setq ξstr (replace-regexp-in-string "\\([0-9]+\\)rd" "\\1" ξstr))
              (setq ξstr (replace-regexp-in-string "\\([0-9]\\)th" "\\1" ξstr))

              (let (dateList ξyear ξmonth ξdate ξyyyy ξmm ξdd )
                (setq dateList (parse-time-string ξstr))
                (setq ξyear (nth 5 dateList))
                (setq ξmonth (nth 4 dateList))
                (setq ξdate (nth 3 dateList))

                (setq ξyyyy (number-to-string ξyear))
                (setq ξmm (if ξmonth (format "%02d" ξmonth) "" ))
                (setq ξdd (if ξdate (format "%02d" ξdate) "" ))
                (concat ξyyyy "-" ξmm "-" ξdd))))))

    (if workOnRegionP
        (progn (delete-region  (elt φfrom-to 0) (elt φfrom-to 1))
               (insert ξstr))
      ξstr )))



(provide 'xeu_elisp_util)
