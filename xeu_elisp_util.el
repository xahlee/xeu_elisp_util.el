;;; xeu_elisp_util.el --- xah's misc elisp utility. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2013-2020, by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 1.4.20201118021915
;; Created: 02 Mar 2011
;; Package-Requires: ((emacs "24.3"))
;; License: GPL v3
;; Keywords: emacs lisp, utility, file

;; This file is not part of GNU Emacs.

;;; Commentary:

;; this package is some misc emacs lisp utility.
;; call list-matching-lines with “defun ”
;; to see a list of functions defined

;; unit-at-cursor and get-selection-or-unit
;; are renamed and moved to
;; See: http://ergoemacs.org/emacs/elisp_get-selection-or-unit.html

;;; INSTALL

;; Place the file in your emacs load path. Then
;; (require 'xeu_elisp_util)

;;; HISTORY:

;; 2015-09-16 alias 'xah-trim-string to emacs's version when possible
;; 2015-09-16 renamed trim-string to xah-trim-string
;; 2015-09-16 renamed substract-path to xah-substract-path
;; 2014-08-20 changes are no longer logged here. See git log instead. This is a hobby code, don't have time to write details.
;; version 1.4.21, 2014-04-24 modified some inline doc to reflect emacs 24.4's new functions
;; version 1.4.20, 2014-01-21 “unit-at-cursor” with 'filepath argument now also consider single quote as delimiter
;; version 1.4.19, 2013-05-10 “get-html-file-title” moved to xah-html-mode.el and name is now “xah-html-get-html-file-title”
;; version 1.4.18, 2013-02-22 removed delete-subdirs-by-regex and xah-delete-files-by-regex . These are either buggy or very inefficient. Pending work.
;; version 1.4.17, 2013-01-27 for unit-at-cursor for arg 'filepath, added no-break space as delimiter.
;; version 1.4.16, 2012-12-29 changed implementation for unit-at-cursor for arg 'filepath
;; version 1.4.15, 2012-08-23 added “xah-file-relative-name-emacs24.1.1-fix”
;; version 1.4.14, 2012-08-14 added “hash-to-list”.
;; version 1.4.13, 2012-07-03 removed curly bracket for 'filepath in “unit-at-cursor”.
;; version 1.4.12, 2012-06-30 added “xah-current-date-time-string”. Added 'url, 'filepath to “unit-at-cursor”.
;; version 1.4.11, 2012-05-05 added { “delete-subdirs-by-regex” “xah-delete-files-by-regex”}
;; version 1.4.10, 2012-05-05 added “xah-substract-path”.
;; version 1.4.9, 2012-03-15 more trivial improved implementation of “xah-get-image-dimensions-imk”.
;; version 1.4.8, 2012-03-03 trivially improved implementation of “xah-get-image-dimensions-imk”.
;; version 1.4.7, 2011-11-26 major change on “xah-get-image-dimensions”. It now supports svn and gif. For gif, it calls “xah-get-image-dimensions-imk”.
;; version 1.4.6, 2011-11-18 Added a “title-case-string-region-or-line”.
;; version 1.4.5, 2011-11-14 corrected a critical error in “asciify-text”.
;; version 1.4.4, 2011-11-14 added function “asciify-text”.
;; version 1.4.3, 2011-11-06 unit-at-cursor with 「'block」 argument will work when the text block is at beginning/end of buffer. Also, lines with just space or tab is also considered a empty line.
;; version 1.4.2, 2011-10-30 trivial implementation change on “xah-html-get-html-file-title”. No user visible effect.
;; version 1.4.1, 2011-09-29 fixed a error in “xah-trim-string”.
;; version 1.4, 2011-09-16 added “xah-trim-string”.
;; version 1.3, 2011-08-27 fixed a bug in “unit-at-cursor” when argument is 「'block」. Now it doesn't grab a extra line ending.
;; version 1.2, 2011-07-02 inline doc improvement for “xah-get-image-dimensions” “xah-get-image-dimensions-imk”.
;; version 1.1, 2011-05-28 Added some comment in source code.
;; version 1.0, 2011-03-02 First version.


;;; Code:

;; (require 'xah-get-thing)



(defun xah-filter-list (@predicate @sequence)
  "Return a new list such that @predicate is true on all members of @sequence.
URL `http://ergoemacs.org/emacs/elisp_filter_list.html'
Version 2016-07-18"
  (delete
   "e3824ad41f2ec1ed"
   (mapcar
    (lambda ($x)
      (if (funcall @predicate $x)
          $x
        "e3824ad41f2ec1ed" ))
    @sequence)))

;; (xah-string-match-in-list-p
;; "/home/xah/web/xahlee_info/css_2.1_spec/propidx.html"
;;  '("/home/xah/web/xahlee_info/php/php_install.html"
;; "/home/xah/web/xahlee_info/css_2.1_spec/"
;; "/home/xah/web/xahlee_info/php/keyed_list.html"
;; "/home/xah/web/xahlee_info/php/mysql.html"
;; "/home/xah/web/xahlee_info/php/misc.html" )
;; "yes"
;; nil)

(defun xah-string-match-in-list-p (@str @list-of-string @match-case-p &optional @reverse-contain-p)
  "If @str occur in list @list-of-string, return true (the first element), else nil.

if @reverse-contain-p is true, change the direction of match. That is, true if any element in @list-of-string occur in @str.

@match-case-p determines whether case is literal for the match.

No regex is used.

Existing match data is changed. Wrap it with `save-match-data' if you need it restored.

URL `http://ergoemacs.org/emacs/elisp_string_match_in_list.html'
Version 2016-07-18"
  (let ((case-fold-search (not @match-case-p)))
    (if @reverse-contain-p
        (catch 'tag
          (mapc
           (lambda ($x)
             (when (string-match (regexp-quote $x) @str ) (throw 'tag $x)))
           @list-of-string)
          nil)
      (catch 'tag
        (mapc
         (lambda ($x)
           (when (string-match (regexp-quote @str) $x ) (throw 'tag $x)))
         @list-of-string)
        nil))))



(defun xah-windows-style-path-to-unix  (@fpath)
  "Turn a MS Windows style full path @FPATH to unix style.
Note: This drops the drive letter.

For example:
 C:\\Users\\xah\\web\\emacs\\emacs.html
becomes
 /Users/xah/web/emacs/emacs.html

TODO: The drive letter is removed. Not sure whether that should be part of this function. But emacs 23.2's `file-relative-name' has a bug. It does not work when there's a drive letter is capitalized."
  (replace-regexp-in-string
   "\\`[A-Za-z]:" ""
   (replace-regexp-in-string "\\\\" "/" @fpath t t)))



(defun xah-get-image-dimensions (@file-path)
  "Returns a vector [width height] of a image's dimension.
The elements are integer datatype.
Support png jpg svg gif and any image type emacs supports.
If it's svg, and dimension cannot be determined, it returns [0 0]

URL `http://ergoemacs.org/emacs/elisp_image_tag.html'
Version 2017-01-11"
  (let (($x nil)
        ($y nil))
    (cond
     ((string-match "\.svg$" @file-path)
      (progn
        (with-temp-buffer
          ;; hackish. grab the first occurence of width height in file
          (insert-file-contents @file-path)
          (goto-char (point-min))
          (when (search-forward-regexp "width=\"\\([0-9]+\\).*\"" nil "NOERROR")
            (setq $x (match-string 1 )))
          (goto-char (point-min))
          (if (search-forward-regexp "height=\"\\([0-9]+\\).*\"" nil "NOERROR")
              (setq $y (match-string 1 ))))
        (if (and (not (null $x)) (not (null $y)))
            (progn (vector (string-to-number $x) (string-to-number $y)))
          (progn [0 0]))))
     (t
      (let ($xy )
        (progn
          (clear-image-cache t)
          (setq $xy (image-size
                     (create-image
                      (if (file-name-absolute-p @file-path)
                          @file-path
                        (concat default-directory @file-path)))
                     t)))
        (vector (car $xy) (cdr $xy)))))))

(defun xah-get-image-dimensions-imk (@img-file-path)
  "Returns a image file's width and height as a vector.
This function requires ImageMagick's “identify” shell command.
See also: `xah-get-image-dimensions'.
URL `http://ergoemacs.org/emacs/elisp_image_tag.html'
Version 2015-05-12"
  (let (($width-height
         (split-string
          (shell-command-to-string
           (concat
            "identify -format \"%w %h\" "
            @img-file-path)))))
    (vector
     (string-to-number (elt $width-height 0))
     (string-to-number (elt $width-height 1)))))


(defun xah-get-string-from-file (@file-path)
  "Return @file-path's content."
  (with-temp-buffer
    (insert-file-contents @file-path)
    (buffer-string)))

(defun xah-get-file-lines (@file-path)
  "Return a list of lines of a file at @file-path."
  (with-temp-buffer
    (insert-file-contents @file-path)
    (split-string (buffer-string) "\n" t)))



;; 2013-02-21 INCORRECT behavior.
;(defun delete-subdirs-by-regex (@dir @regex)
;  "Delete sub-directories in @dir whose path matches @REGEX."
;  (require 'find-lisp)
;  (mapc
;   (lambda ($x) (when (file-directory-p $x)
;;;(delete-directory $x t)
;                  (print $x)
;                  ))
;   (find-lisp-find-files @dir @regex)) )

(defun xah-delete-files-by-regex (@dir @regex)
  "Delete files in @dir whose file name (not full path) matches regex @regex.
 Example:
  (xah-delete-files-by-regex \"~/web\" \"~$\") ; remove files ending in ~
"
  (require 'find-lisp)
  (mapc (lambda ($x) (if (file-regular-p $x) (delete-file $x)))
        (find-lisp-find-files @dir @regex)))

(defun xah-file-relative-name-emacs24.1.1-fix (@file-path @dir-path)
  "fix for `file-relative-name'. If path start with cap such as “C:” (Windows file path), it won't work.
e.g.
 (file-relative-name \"c:/Users/h3/.emacs.d/test.el\" \"c:/Users/h3/.emacs.d/\" )
 (file-relative-name \"C:/Users/h3/.emacs.d/test.el\" \"C:/Users/h3/.emacs.d/\" ) ⇒ \"C:/Users/h3/.emacs.d/test.el\"
GNU Emacs 24.1.1 (i386-mingw-nt6.1.7601) of 2012-06-10 on MARVIN
"
  (file-relative-name
   (replace-regexp-in-string "\\`C:/" "c:/" @file-path  "FIXEDCASE" "LITERAL") @dir-path ))



;; define xah-trim-string, by using emacs's version
(if (version< emacs-version "24.4")
    (progn
      (defsubst string-trim-left (string)
        "Remove leading whitespace from STRING."
        (if (string-match "\\`[ \t\n\r]+" string)
            (replace-match "" t t string)
          string))

      (defsubst string-trim-right (string)
        "Remove trailing whitespace from STRING."
        (if (string-match "[ \t\n\r]+\\'" string)
            (replace-match "" t t string)
          string))

      (defsubst string-trim (string)
        "Remove leading and trailing whitespace from STRING."
        (string-trim-left (string-trim-right string))))
  (progn
    (require 'subr-x)
    (fset 'xah-trim-string 'string-trim)
    ))

;; (defalias 'xah-trim-string 'string-trim
;;   "Remove white spaces in beginning and ending of @string.
;; White space here is any of: space, tab, emacs newline (line feed, ASCII 10).

;; Note: in emacs GNU Emacs 24.4+ and later, there's `string-trim' function. You need to (require 'subr-x).
;; ")

;; (defun xah-trim-string (@string)
;;   "Remove white spaces in beginning and ending of @string.
;; White space here is any of: space, tab, emacs newline (line feed, ASCII 10).

;; Note: in emacs GNU Emacs 24.4+ and later, there's `string-trim' function. You need to (require 'subr-x).
;; "
;;   (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" @string))
;; )

(defun xah-substract-path (@path1 @path2)
  "Remove string @path2 from the beginning of @path1.
length of @path1 ≥ to length @path2.

path1
c:/Users/lisa/web/a/b

path2
c:/Users/lisa/web/

result
a/b

This is the similar to emacs 24.4's `string-remove-prefix' from (require 'subr-x).
Version 2015-12-15"
  (let (($p2length (length @path2)))
    (if (string= (substring @path1 0 $p2length) @path2 )
        (substring @path1 $p2length)
      (error "error 34689: beginning doesn't match: 「%s」 「%s」" @path1 @path2))))

(defun xah-hash-to-list (@hash-table)
  "Return a list that represent the @HASH-TABLE
Each element is a list: '(key value).

http://ergoemacs.org/emacs/elisp_hash_table.html
Version 2019-06-11"
  (let ($result)
    (maphash
     (lambda (k v)
       (push (list k v) $result))
     @hash-table)
    $result))



(defun xah-asciify-text (&optional @begin @end)
  "Remove accents in some letters and some
Change European language characters into equivalent ASCII ones, e.g. “café” ⇒ “cafe”.
When called interactively, work on current line or text selection.

URL `http://ergoemacs.org/emacs/emacs_zap_gremlins.html'
Version 2018-11-12"
  (interactive)
  (let (($charMap
         [
          ["ß" "ss"]
          ["á\\|à\\|â\\|ä\\|ā\\|ǎ\\|ã\\|å\\|ą\\|ă\\|ạ\\|ả\\|ả\\|ấ\\|ầ\\|ẩ\\|ẫ\\|ậ\\|ắ\\|ằ\\|ẳ\\|ặ" "a"]
          ["æ" "ae"]
          ["ç\\|č\\|ć" "c"]
          ["é\\|è\\|ê\\|ë\\|ē\\|ě\\|ę\\|ẹ\\|ẻ\\|ẽ\\|ế\\|ề\\|ể\\|ễ\\|ệ" "e"]
          ["í\\|ì\\|î\\|ï\\|ī\\|ǐ\\|ỉ\\|ị" "i"]
          ["ñ\\|ň\\|ń" "n"]
          ["ó\\|ò\\|ô\\|ö\\|õ\\|ǒ\\|ø\\|ō\\|ồ\\|ơ\\|ọ\\|ỏ\\|ố\\|ổ\\|ỗ\\|ộ\\|ớ\\|ờ\\|ở\\|ợ" "o"]
          ["ú\\|ù\\|û\\|ü\\|ū\\|ũ\\|ư\\|ụ\\|ủ\\|ứ\\|ừ\\|ử\\|ữ\\|ự"     "u"]
          ["ý\\|ÿ\\|ỳ\\|ỷ\\|ỹ"     "y"]
          ["þ" "th"]
          ["ď\\|ð\\|đ" "d"]
          ["ĩ" "i"]
          ["ľ\\|ĺ\\|ł" "l"]
          ["ř\\|ŕ" "r"]
          ["š\\|ś" "s"]
          ["ť" "t"]
          ["ž\\|ź\\|ż" "z"]
          [" " " "]       ; thin space etc
          ["–" "-"]       ; dash
          ["—\\|一" "--"] ; em dash etc
          ])
        $begin $end
        )
    (if (null @begin)
        (if (use-region-p)
            (setq $begin (region-beginning) $end (region-end))
          (setq $begin (line-beginning-position) $end (line-end-position)))
      (setq $begin @begin $end @end))
    (let ((case-fold-search t))
      (save-restriction
        (narrow-to-region $begin $end)
        (mapc
         (lambda ($pair)
           (goto-char (point-min))
           (while (search-forward-regexp (elt $pair 0) (point-max) t)
             (replace-match (elt $pair 1))))
         $charMap)))))

(defun xah-asciify-string (@string)
  "Returns a new string. European language chars are changed ot ASCII ones e.g. “café” ⇒ “cafe”.
See `xah-asciify-text'
Version 2015-06-08"
  (with-temp-buffer
      (insert @string)
      (xah-asciify-text (point-min) (point-max))
      (buffer-string)))



(defvar xah-month-full-names '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December") "list of English month full names.")

(defvar xah-month-abbrev-names (mapcar (lambda (x) (substring x 0 3)) xah-month-full-names) "list of English month 3-letter abbrev names.")

(defvar xah-weekday-names '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday") "list of English weekday full names.")

(defun xah-insert-date (&optional @add-time-stamp-p)
  "Insert current date and or time.

• In this format yyyy-mm-dd.
• When called with `universal-argument', insert date and time, e.g. 2012-05-28T07:06:23-07:00
• Replaces text selection.

See also `xah-current-date-time-string'."
  (interactive "P")
  (when (use-region-p) (delete-region (region-beginning) (region-end) ) )
  (cond
   ((equal @add-time-stamp-p nil ) (insert (format-time-string "%Y-%m-%d")))
   (t (insert (xah-current-date-time-string))) ) )

(defun xah-current-date-time-string ()
  "Returns current date-time string in full ISO 8601 format.
Example: 「2012-04-05T21:08:24-07:00」.

Note, for the time zone offset, both the formats 「hhmm」 and 「hh:mm」 are valid ISO 8601. However, Atom Webfeed spec seems to require 「hh:mm」."
  (concat
   (format-time-string "%Y-%m-%dT%T")
   (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z"))))

(defun xah-is-datetimestamp-p (@input-string)
  "Return t if @input-string is a date/time stamp, else nil.
This is based on heuristic, so it's not 100% correct.
If the string contains any month names, weekday names, or of the form dddd-dd-dd, dddd-dd-dddd, dddd-dd-dd, or using slash, then it's considered a date.
"
  (cond
         ((string-match (regexp-opt (append xah-month-full-names xah-month-abbrev-names xah-weekday-names) 'words) @input-string) t)
         ;; mm/dd/yyyy
         ((string-match "\\b[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]\\b" @input-string) t)
         ;; yyyy/mm/dd
         ((string-match "\\b[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\b" @input-string) t)
         ;; mm/dd/yy
         ((string-match "\\b[0-9][0-9]/[0-9][0-9]/[0-9][0-9]\\b" @input-string) t)
         ;; mm-dd-yyyy
         ((string-match "\\b[0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9]\\b" @input-string) t)
         ;; yyyy-mm-dd
         ((string-match "\\b[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\b" @input-string) t)
         ;; mm-dd-yy
         ((string-match "\\b[0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\b" @input-string) t)
         (t nil) ))

(defun xah-fix-datetime (@begin @end)
  "Change timestamp under cursor into a yyyy-mm-dd format.
If there's a text selection, use that as input, else use current line.
Replace the text in selection or current line.

Any “day of week”, or “time” info, or any other parts of the string, are discarded.
For example:
 TUESDAY, FEB 15, 2011 05:16 ET → 2011-02-15
 November 28, 1994              → 1994-11-28
 Nov. 28, 1994                  → 1994-11-28
 11/28/1994                     → 1994-11-28
 1994/11/28                     → 1994-11-28

URL `http://ergoemacs.org/emacs/elisp_datetime_parser.html'
Version 2020-09-08"
  (interactive
   (list
    (if (region-active-p) (region-beginning))
    (if (region-active-p) (region-end))))
  (require 'parse-time)
  (let ($p1 $p2 $in)
    (if @begin
        (setq $p1 @begin $p2 @end)
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (setq $in (replace-regexp-in-string "^ *\\(.+\\) *$" "\\1" (buffer-substring-no-properties $p1 $p2)))
  ; remove white spaces

    (setq $in
          (cond

           ;; yyyy/mm/dd
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" $in)
            (concat (match-string 1 $in) "-" (match-string 2 $in) "-" (match-string 3 $in)))

           ;; mm/dd/yyyy
           ((string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)" $in)
            (concat (match-string 3 $in) "-" (match-string 1 $in) "-" (match-string 2 $in)))
           ;; m/dd/yyyy
           ((string-match "\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)" $in)
            (concat (match-string 3 $in) "-0" (match-string 1 $in) "-" (match-string 2 $in)))

           ;; USA convention of mm/dd/yy
           ((string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" $in)
            (concat (format-time-string "%C") (match-string 3 $in) "-" (match-string 1 $in) "-" (match-string 2 $in)))
           ;; USA convention of m/dd/yy
           ((string-match "\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" $in)
            (concat (format-time-string "%C") (match-string 3 $in) "-0" (match-string 1 $in) "-" (match-string 2 $in)))

           ;; some ISO 8601. yyyy-mm-ddThh:mm
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)T[0-9][0-9]:[0-9][0-9]" $in)
            (concat (match-string 1 $in) "-" (match-string 2 $in) "-" (match-string 3 $in)))
           ;; some ISO 8601. yyyy-mm-dd
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)" $in)
            (concat (match-string 1 $in) "-" (match-string 2 $in) "-" (match-string 3 $in)))
           ;; some ISO 8601. yyyy-mm
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)" $in)
            (concat (match-string 1 $in) "-" (match-string 2 $in)))

           ;; else
           (t
            (progn
              (setq $in (replace-regexp-in-string "January " "Jan. " $in))
              (setq $in (replace-regexp-in-string "February " "Feb. " $in))
              (setq $in (replace-regexp-in-string "March " "Mar. " $in))
              (setq $in (replace-regexp-in-string "April " "Apr. " $in))
              (setq $in (replace-regexp-in-string "May " "May. " $in))
              (setq $in (replace-regexp-in-string "June " "Jun. " $in))
              (setq $in (replace-regexp-in-string "July " "Jul. " $in))
              (setq $in (replace-regexp-in-string "August " "Aug. " $in))
              (setq $in (replace-regexp-in-string "September " "Sep. " $in))
              (setq $in (replace-regexp-in-string "October " "Oct. " $in))
              (setq $in (replace-regexp-in-string "November " "Nov. " $in))
              (setq $in (replace-regexp-in-string "December " "Dec. " $in))

              (setq $in (replace-regexp-in-string "\\([0-9]+\\)st" "\\1" $in))
              (setq $in (replace-regexp-in-string "\\([0-9]+\\)nd" "\\1" $in))
              (setq $in (replace-regexp-in-string "\\([0-9]+\\)rd" "\\1" $in))
              (setq $in (replace-regexp-in-string "\\([0-9]\\)th" "\\1" $in))

              (let ($dateList $year $month $date $yyyy $mm $dd )
                (setq $dateList (parse-time-string $in))
                (setq $year (nth 5 $dateList))
                (setq $month (nth 4 $dateList))
                (setq $date (nth 3 $dateList))

                (setq $yyyy (number-to-string $year))
                (setq $mm (if $month (format "%02d" $month) "" ))
                (setq $dd (if $date (format "%02d" $date) "" ))
                (concat $yyyy "-" $mm "-" $dd))))))
    (delete-region $p1 $p2 )
    (insert $in)))

(defun xah-fix-datetime-string (@datetime)
  "Return a new string of @datetime in yyyy-mm-dd format.
Other datetime info such as hours, minutes, time zone, are discarded. This function calls `xah-fix-datetime' to do work.

URL `http://ergoemacs.org/emacs/elisp_datetime_parser.html'
Version 2020-09-08"
  (with-temp-buffer
    (insert @datetime)
    (xah-fix-datetime (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

;; (defun xah-fix-datetime-stamp (@input-string &optional @begin-end)
;;   "obsolete function to parse time on region or string.
;;  use `xah-fix-datetime' or `xah-fix-datetime-string' instead.
;; Declared obsolete on 2020-09-08.
;; Version 2020-11-18"
;; (interactive)
;;   (if (elt @begin-end 0)
;;       (xah-fix-datetime (elt @begin-end 0) (elt @begin-end 1))
;;     (xah-fix-datetime-string @input-string)))

;; (define-obsolete-function-alias 'xah-fix-datetime-stamp 'xah-fix-datetime-string "27.1"
;; "Change timestamp under cursor into a yyyy-mm-dd format.
;; If there's a text selection, use that as input, else use current line.

;; Any “day of week”, or “time” info, or any other parts of the string, are discarded.
;; For example:
;;  「TUESDAY, FEB 15, 2011 05:16 ET」 ⇒ 「2011-02-15」
;;  「November 28, 1994」              ⇒ 「1994-11-28」
;;  「Nov. 28, 1994」                  ⇒ 「1994-11-28」
;;  「11/28/1994」                     ⇒ 「1994-11-28」
;;  「1994/11/28」                     ⇒ 「1994-11-28」

;; When called in lisp program, the optional second argument “@begin-end” is a vector of region boundary. (it can also be a list)
;; If “@begin-end” is non-nil, the region is taken as input (and “@input-string” is ignored).

;; URL `http://ergoemacs.org/emacs/elisp_parse_time.html'
;; Version 2015-04-14" )



(provide 'xeu_elisp_util)
