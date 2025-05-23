;;; lisp-extra-font-lock.el --- Highlight bound variables and quoted exprs -*- lexical-binding: t -*-

;; Copyright (C) 2014-2018 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: languages, faces
;; Created: 2014-11-22
;; Version: 0.0.6
;; URL: https://github.com/Lindydancer/lisp-extra-font-lock

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package highlight the location where local variables is
;; created (bound, for example by `let') as well as quoted and
;; backquoted constant expressions.

;; Example:
;;
;; Below, `^' is used indicate highlighted normal variables and
;; constant expressions. `*' is used to show highlighting of special
;; variables (i.e. those defined by `defvar') and of the backquote and
;; comma operators.
;;
;; (defun my-function (next)
;;                     ^^^^             <- Parameters
;;   (let ((numbers '(one two three))
;;          ^^^^^^^  ^^^^^^^^^^^^^^^    <- Var bound by `let' and quoted expr.
;;         (buffer-read-only t))
;;          ****************            <- Special variable (different color)
;;     `(,@numbers and ,next)))
;;     *^**        ^^^ *    ^           <- Backquote and comma
;;
;; Screenshot:
;;
;; ![See doc/demo.png for screenshot](doc/demo.png)

;; What is highlighted:
;;
;; * Parameters in functions and lambdas
;;
;; * Variables bound by specal constructs like `let', `dolist',
;;   `condition-case', and `pcase-let'
;;
;; * Normal variables and variables declared as globals using `defvar'
;;   are highlighted in different colors, as a warning
;;
;; * Quoted expressions
;;
;; * Backquoted expressions. Subexpressions using the "," or ",@" are
;;   not highlighted (as they are evaluted and thus not constant).
;;   Also, the backquote and the comma operators themselves are
;;   highlighted using a bright color as a warning.
;;
;; * Hash-quoted symbols.

;; Installation:
;;
;; Place this package in a directory in the load-path. To activate it,
;; use *customize* or place the following lines in a suitable init
;; file:
;;
;;    (require 'lisp-extra-font-lock)
;;    (lisp-extra-font-lock-global-mode 1)

;; Customization:
;;
;; You can modify the following lists to add more functions that are
;; recognized:
;;
;; * `lisp-extra-font-lock-let-functions' -- List of function with the
;;   same syntax as `let'
;;
;; * `lisp-extra-font-lock-defun-functions' -- List of function with
;;   the same syntax as `defun'
;;
;; * `lisp-extra-font-lock-lambda-functions' -- List of function with
;;   the same syntax as `lambda'
;;
;; * `lisp-extra-font-lock-dolist-functions' -- List of function with
;;   the same syntax as `dolist'
;;
;; * `lisp-extra-font-lock-bind-first-functions' -- List of function
;;   that bind their first argument, like `condition-case'.
;;
;; * `lisp-extra-font-lock-loop-functions' -- List of functions with
;;   the same syntax as `cl-loop'.
;;
;; The following faces are used when highlighting. You can either
;; redefine the face (e.g. using a theme), or you can rebind the
;; corresponding variable.
;;
;; * Local variables are highlighted using the standard face
;;   `font-lock-variable-name-face'
;;
;; * Special (global) variables that are rebound are highlighted using
;;   the face bound to the variable
;;   `lisp-extra-font-lock-special-variable-name-face' (by default
;;   `lisp-extra-font-lock-special-variable-name', which inherits from
;;   `font-lock-warning-face')
;;
;; * Quoted expressions use the face bound to the variable
;;   `lisp-extra-font-lock-quoted-face' (by default
;;   `lisp-extra-font-lock-quoted', which inherits from
;;   `font-lock-constant-face')
;;
;; * The backquote and comma operators use the face bound to the
;;   variable `lisp-extra-font-lock-backquote-face' (by default
;;   `lisp-extra-font-lock-backquote', which inherits from
;;   `font-lock-warning-face').
;;
;; * Named arguments to `cl-loop' are highlighted using
;;   `font-lock-builtin-face'.
;;
;; Example:
;;
;; To set the face used to highlight quoted expressions to a gray
;; color, you can use:
;;
;;     (custom-set-faces
;;       '(lisp-extra-font-lock-quoted ((t :foreground "grey50"))))

;;; Code:

;; ------------------------------
;; Customizable variables
;;


(defgroup lisp-extra-font-lock nil
  "Highlight bound variables and quoted expressions in Lisp."
  :group 'faces)

;;;###autoload
(defcustom lisp-extra-font-lock-modes '(emacs-lisp-mode lisp-mode)
  "List of modes where Lisp Extra Font Lock Global mode should be enabled."
  :type '(repeat symbol)
  :group 'lisp-extra-font-lock)


;; ----------
;; Faces and corresponding variable.
;;

(defface lisp-extra-font-lock-backquote
  '((t :inherit font-lock-warning-face))
  "The default face used to highlight backquotes and the comma operator."
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-backquote-face 'lisp-extra-font-lock-backquote
  "The face used to highlight backquotes and the comma operator.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'lisp-extra-font-lock)


(defface lisp-extra-font-lock-quoted
  '((t :inherit font-lock-constant-face))
  "The default face used to highlight quoted expressions."
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-quoted-face 'lisp-extra-font-lock-quoted
  "The face used to highlight quoted expressions.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'lisp-extra-font-lock)


(defface lisp-extra-font-lock-quoted-function
  '((t :inherit font-lock-function-name-face))
  "The default face used to highlight #'-quoted function symbols."
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-quoted-function-face
  'lisp-extra-font-lock-quoted-function
  "The face used to highlight #'-quoted function symbols.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'lisp-extra-font-lock)


(defface lisp-extra-font-lock-special-variable-name
  '((t :inherit font-lock-warning-face))
  "The default face used to highlight special variables bound by `let'."
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-special-variable-name-face
  'lisp-extra-font-lock-special-variable-name
  "The face used to highlight special variables bound by `let'.

A special variable is a global variable defined by `defvar'. See
`special-variable-p' for details.

To disable this highlighting, set this to nil. To highlight
special variables like plain variables, set this to
`font-lock-variable-name-face'."
  :type '(choice (const nil)
                 face)
  :group 'lisp-extra-font-lock)


;; ----------
;; Function lists
;;


(defface lisp-extra-font-lock-macro-face
  '((t :inherit 'font-lock-type-face))
  "Colourful font-lock face for macro names.")
(defvar lisp-extra-font-lock-macro-face 'font-lock-macro-face
  "Face name to use for macro.")

(defvar lisp-extra-font-lock-function-call-face 'font-lock-function-call-face)
(defvar lisp-extra-font-lock-variable-use-face 'font-lock-variable-use-face)

(defvar lisp-extra-font-lock-macro-face 'font-lock-macro-face
  "Face name to use for macro.")

(defcustom lisp-extra-font-lock-macro-symbols-opt
  (regexp-opt
   '("pprint-pop" "with-condition-restarts" "defpackage" "define-setf-expander"
     "remf" "declaim" "in-package" "pop" "with-compilation-unit"
     "with-standard-io-syntax" "dotimes" "restart-case" "multiple-value-setq"
     "loop-finish" "with-package-iterator" "setf" "handler-case"
     "define-symbol-macro" "lambda" "time" "restart-bind" "pprint-logical-block"
     "ctypecase" "do" "destructuring-bind" "deftype" "with-hash-table-iterator"
     "or" "assert" "shiftf" "defmacro" "prog2" "handler-bind" "pushnew"
     "do-external-symbols" "unless" "define-compiler-macro" "prog" "ecase"
     "print-unreadable-object" "case" "define-modify-macro" "step" "and"
     "defconstant" "do*" "with-open-stream" "decf" "psetq" "trace" "rotatef"
     "multiple-value-list" "with-input-from-string" "define-method-combination"
     "do-all-symbols" "with-slots" "define-condition" "typecase" "untrace" "push"
     "pprint-exit-if-list-exhausted" "ignore-errors" "defstruct" "defgeneric"
     "defsetf" "dolist" "defclass" "when" "with-output-to-string" "prog*" "ccase"
     "prog1" "psetf" "with-open-file" "return" "incf" "defun" "defmethod" "defvar"
     "multiple-value-bind" "etypecase" "nth-value" "check-type" "formatter" "cond"
     "call-method" "with-simple-restart" "do-symbols" "with-accessors" "loop"
     "defparameter")
   'symbols)
  "Macro names."
  :group 'lisp-extra-font-lock
  :type '(list symbol))

(defcustom lisp-extra-font-lock-function-symbols-opt
  (regexp-opt
   '("nintersection" "pprint-pop" "documentation" "atan" "copy-readtable"
     "package-shadowing-symbols" "with-condition-restarts" "endp"
     "simple-bit-vector-p" "defpackage" "define-setf-expander" "input-stream-p"
     "remf" "load" "shadow" "adjoin" "nunion" "cdr" "char-int" "lower-case-p"
     "gentemp" "float-precision" "gcd" "declaim" "char-not-equal" "macro-function"
     "digit-char" "boole" "file-error-pathname" "char<" "string-upcase" "asinh"
     "warn" "cadadr" "bit" "ldb" "in-package" "pop" "unread-char" "write"
     "constantly" "dpb" "pathnamep" "slot-value" "set" "compile-file-pathname"
     "make-sequence" "with-compilation-unit" "cddr" "mismatch" "find-method"
     "with-standard-io-syntax" "dotimes" "describe" "replace" "clrhash"
     "restart-case" "translate-pathname" "string<" "vector-pop" "hash-table-count"
     "multiple-value-setq" "bit-nand" "hash-table-rehash-size" "proclaim"
     "disassemble" "loop-finish" "with-package-iterator" "method-qualifiers"
     "copy-pprint-dispatch" "<=" "type-of" "complexp" "functionp" "rationalize"
     "y-or-n-p" "setf" "plusp" "list" "make-load-form" "ensure-generic-function"
     "count-if" "handler-case" "pathname-device" "symbol-function"
     "array-has-fill-pointer-p" "enough-namestring" "round" "set-syntax-from-char"
     "machine-version" "member-if-not" "wild-pathname-p" "nthcdr"
     "get-universal-time" "file-length" "peek-char" "make-two-way-stream" "cis"
     "define-symbol-macro" "pathname" "digit-char-p" "copy-structure" "cdddr"
     "pprint-tabular" "cdadar" "logical-pathname" "union" "array-element-type"
     "floor" "lambda" "time" "logxor" "rassoc" "restart-bind"
     "pprint-logical-block" "/=" "cos" "random-state-p" "if" "nsubst" "ed"
     "remove-method" "copy-symbol" "vector-push" "ctypecase" "do" "finish-output"
     "labels" "invoke-restart" "hash-table-rehash-threshold" "subseq" "lognand"
     "sxhash" "get-properties" "caaaar" "pprint-tab" "coerce"
     "multiple-value-prog1" "destructuring-bind" "string-right-trim" "deftype"
     "output-stream-p" "funcall" "with-hash-table-iterator" "pprint-linear"
     "realpart" "describe-object" "block" "or" "rest" "shadowing-import" "first"
     "nbutlast" "bit-nor" "deposit-field" "=" "assert" "stream-element-type"
     "char-upcase" "numerator" "char-downcase" "pathname-host" "max" "open"
     "return-from" "shiftf" "nset-difference" "defmacro" "cdaddr" "break" "remprop"
     "prog2" "sort" "use-value" "read-char" "string-trim" "read" "apropos"
     "method-combination-error" "hash-table-p" "string=" "char-greaterp" "nreconc"
     "string-not-greaterp" "compute-restarts" "handler-bind" "char>=" "char"
     "tailp" "substitute-if-not" "make-string-output-stream" "logand" "tagbody"
     "sinh" "member-if" "rem" "atom" "standard-char-p" "print" "list*"
     "arithmetic-error-operation" "logandc2" "simple-condition-format-arguments"
     "unuse-package" "pushnew" "signum" "cdaar" "make-string" "function" "mapc"
     "quote" "sublis" "arrayp" "nstring-downcase" "do-external-symbols"
     "position-if" "simple-vector-p" "write-byte" "string>" "slot-missing" "unless"
     "translate-logical-pathname" "stable-sort" "nth" "nconc" "nsubstitute-if"
     "define-compiler-macro" "read-char-no-hang" "stream-error-stream"
     "simple-string-p" "macroexpand-1" "function-lambda-expression"
     "machine-instance" "delete" "add-method" "logorc2" "make-package"
     "char-not-lessp" "rplacd" "upgraded-array-element-type" "mapl"
     "set-exclusive-or" "string-greaterp" "hash-table-size" "read-sequence"
     "string<=" "sqrt" "read-from-string" "minusp" "make-pathname" "read-byte"
     "pathname-directory" "make-instance" "prog" "abs" "char-lessp" "find" "catch"
     "mapcar" "ldb-test" "write-sequence" "ecase" "boundp" "keywordp" "dribble"
     "long-site-name" "char-equal" "butlast" "get-decoded-time" "symbol-value"
     "integerp" "count-if-not" "bit-xor" "terpri" "array-row-major-index"
     "software-type" "alpha-char-p" "concatenated-stream-streams" "let*"
     "invoke-restart-interactively" "slot-unbound" "close" "nsubst-if" "logeqv"
     "make-list" "floatp" "package-name" "concatenate" "synonym-stream-symbol"
     "encode-universal-time" "get-setf-expansion" "logior"
     "ensure-directories-exist" "print-unreadable-object" "cddddr" "case" "lognot"
     "make-synonym-stream" "char=" "signal" "princ" "interactive-stream-p" "cdaadr"
     "copy-seq" "define-modify-macro" "row-major-aref" "lisp-implementation-type"
     "file-write-date" "progn" "step" "format" "zerop" "make-concatenated-stream"
     "cdaaar" "decode-universal-time" "make-load-form-saving-slots" "string-equal"
     "array-rank" "initialize-instance" "acosh" "nsubst-if-not" "write-string"
     "remhash" "copy-alist" "and" "namestring" "float-sign" "parse-integer"
     "short-site-name" "two-way-stream-input-stream" "caadr" "symbolp"
     "defconstant" "getf" "delete-file" "type-error-expected-type" "isqrt" "float"
     "byte-position" "do*" "with-open-stream" "file-string-length" "clear-output"
     "abort" "decf" "stringp" "psetq" "no-applicable-method" "array-total-size"
     "trace" "directory" "rotatef" "bit-orc1" "float-digits" "nstring-upcase"
     "seventh" "class-of" "atanh" "adjust-array" "cdadr" "subst-if-not" ">="
     "compiled-function-p" "error" "broadcast-stream-streams"
     "upgraded-complex-part-type" "class-name" "string>=" "nsubstitute-if-not"
     "macrolet" "pprint" "cons" "multiple-value-list" "position"
     "get-internal-run-time" "macroexpand" "rename-file" "progv"
     "with-input-from-string" "values-list" "define-method-combination" "import"
     "symbol-plist" "logorc1" "write-to-string" "do-all-symbols"
     "logical-pathname-translations" "assoc-if" "pathname-name" "file-author"
     "allocate-instance" "tenth" "get-macro-character" "copy-tree" "*" "with-slots"
     "position-if-not" "read-line" "type-error-datum" "read-preserving-whitespace"
     "caadar" "reinitialize-instance" "car" "store-value" "define-condition"
     "vector-push-extend" "last" "caddr" "readtablep" "bit-ior" "realp" "asin"
     "rename-package" "ffloor" "phase" "typecase" "nsublis" "length" "char-code"
     "eq" "subst-if" "graphic-char-p" "delete-if-not" "aref" "name-char"
     "conjugate" "untrace" "echo-stream-input-stream" "characterp" "caar"
     "pprint-newline" "push" "reverse" "pprint-indent" "revappend" "code-char"
     "sixth" "list-all-packages" "restart-name" "clear-input" "slot-exists-p"
     "substitute" "imagpart" "software-version" "merge-pathnames" "char/="
     "pprint-exit-if-list-exhausted" "exp" "make-hash-table" "unwind-protect"
     "apropos-list" "file-namestring" "provide" "string" "map-into" "find-if-not"
     "ceiling" "muffle-warning" "nstring-capitalize" "reduce"
     "get-dispatch-macro-character" "caddar" "fboundp" "gensym" "consp"
     "compile-file" "find-all-symbols" "byte-size" "make-condition" "bit-andc1"
     "eighth" "probe-file" "intersection" "compile" "bit-andc2" "typep"
     "read-delimited-list" "array-in-bounds-p" "ldiff" "gethash" "cddar"
     "multiple-value-call" "adjustable-array-p" "character" "get" "ignore-errors"
     "tree-equal" "map" "nsubstitute" "unexport" "pathname-match-p" "/"
     "use-package" "substitute-if" "assoc-if-not" "unbound-slot-instance" "fourth"
     "random" "invoke-debugger" "min" "defstruct" "stream-external-format"
     "bit-orc2" "symbol-macrolet" "defgeneric" "change-class" "complement" "assoc"
     "fifth" "nreverse" "the" "intern" "defsetf" "dolist" "defclass" "eval" "when"
     "with-output-to-string" "cosh" "string-not-equal" "load-time-value" "prog*"
     "delete-duplicates" "copy-list" "symbol-name" "readtable-case" "search"
     "update-instance-for-different-class" ">" "decode-float" "unintern"
     "remove-if-not" "+" "log" "eql" "require" "invalid-method-error" "list-length"
     "truncate" "logbitp" "denominator" "string-capitalize" "mask-field" "go"
     "cadar" "makunbound" "acos" "fresh-line" "continue" "integer-length"
     "integer-decode-float" "cdar" "rational" "ccase" "prog1" "logcount" "svref"
     "evenp" "get-internal-real-time" "expt" "set-dispatch-macro-character"
     "char-not-greaterp" "load-logical-pathname-translations" "room" "bit-not"
     "caaar" "make-array" "package-used-by-list" "1+" "array-dimensions"
     "shared-initialize" "caaadr" "psetf" "maphash" "byte" "null" "maplist"
     "inspect" "mapcon" "with-open-file" "print-object" "identity" "bit-vector-p"
     "delete-if" "nset-exclusive-or" "float-radix" "notany" "char<=" "append"
     "echo-stream-output-stream" "rassoc-if" "equal" "array-displacement" "rplaca"
     "let" "eval-when" "lisp-implementation-version" "yes-or-no-p" "open-stream-p"
     "upper-case-p" "return" "ftruncate" "schar" "incf" "make-symbol"
     "make-random-state" "cerror" "defun" "special-operator-p" "merge" "locally"
     "defmethod" "bit-eqv" "sbit" "remove" "slot-makunbound"
     "two-way-stream-output-stream" "numberp" "subst" "remove-duplicates"
     "make-broadcast-stream" "string-lessp" "write-line" "fmakunbound"
     "set-difference" "notevery" "defvar" "<" "constantp" "tan"
     "multiple-value-bind" "princ-to-string" "package-use-list"
     "user-homedir-pathname" "fill" "cadr" "throw" "compute-applicable-methods"
     "cadddr" "fdefinition" "packagep" "oddp" "etypecase" "find-package"
     "rassoc-if-not" "bit-and" "acons" "lcm" "update-instance-for-redefined-class"
     "logandc1" "char-name" "equalp" "nth-value" "not" "check-type" "elt"
     "arithmetic-error-operands" "pathname-version" "second" "string-downcase"
     "listen" "directory-namestring" "mapcan" "truename" "formatter" "some"
     "both-case-p" "tanh" "cond" "call-method" "array-dimension" "cadaar" "ninth"
     "find-class" "with-simple-restart" "apply" "char>" "vectorp"
     "make-string-input-stream" "sin" "cddaar" "host-namestring" "1-" "find-symbol"
     "remove-if" "subsetp" "machine-type" "parse-namestring" "hash-table-test"
     "compiler-macro-function" "set-macro-character" "flet"
     "make-instances-obsolete" "force-output" "fill-pointer" "mod" "subtypep"
     "third" "do-symbols" "cell-error-name" "pathname-type" "listp" "rationalp"
     "pprint-dispatch" "lognor" "write-char" "member" "pairlis" "file-position"
     "caaddr" "symbol-package" "find-if" "prin1" "values" "ash" "with-accessors"
     "fceiling" "print-not-readable-object" "cdddar"
     "simple-condition-format-control" "count" "alphanumericp" "vector"
     "no-next-method" "set-pprint-dispatch" "slot-boundp" "package-nicknames"
     "string-not-lessp" "fround" "setq" "find-restart" "logtest" "streamp"
     "function-keywords" "package-error-package" "string/=" "sleep" "every" "loop"
     "make-echo-stream" "string-left-trim" "-" "export"
     "make-dispatch-macro-character" "pprint-fill" "complex" "cddadr" "scale-float"
     "get-output-stream-string" "defparameter" "prin1-to-string" "delete-package")
   'symbols)
  "Function names."
  :group 'lisp-extra-font-lock
  :type '(list symbol))

(defcustom lisp-extra-font-lock-let-functions
  '("let"
    "let*"
    "letf"
    "letf*"
    "lexical-let"
    "lexical-let*"
    "multiple-value-bind"
    "pcase-let"                         ; Highlights entire UPAT:s.
    "pcase-let*"
    "cl-letf"
    "cl-letf*"
    "cl-multiple-value-bind"

    "-let"
    "-let*"
    "-if-let"
    "-if-let*"
    "-when-let"
    "-when-let*"

    "prog"
    "prog*"
    "cl-prog"
    "cl-prog*"
    "progv"
    "cl-progv"
    "compiler-let"
    "when-let"
    "when-let*"
    "if-let"
    "if-let*"
    "alexandria:when-let"
    "alexandria:when-let*"
    "alexandria:if-let"
    "alexandria:if-let*")
  "List of function using same syntax as `let' to bind variables."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)

(defcustom lisp-extra-font-lock-defun-functions
  '("defun"
    "defun*"
    "defmacro"
    "defmacro*"
    "defsubst"
    "cl-defun"
    "cl-defmacro"
    "cl-defsubst")
  "List of function using same syntax as `defun' to bind variables."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-lambda-functions
  '("lambda" "destructuring-bind" "cl-destructuring-bind")
  "List of function using same syntax as `lambda' to bind variables."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-dolist-functions
  '("dolist"
    "dotimes"
    "cl-dolist"
    "cl-dotimes"
    "do-symbols"
    "cl-do-symbols"
    "do-all-symbols"
    "cl-do-all-symbols"

    "doseq"
    "seq-doseq")
  "List of function using same syntax as `dolist' to bind variables."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-bind-first-functions
  '("condition-case")
  "List of function that bind their first argument."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


(defcustom lisp-extra-font-lock-loop-functions
  '("loop"
    "cl-loop")
  "List of functions using same syntax as `loop' to bind variables.."
  :type '(repeat string)
  :group 'lisp-extra-font-lock)


;; ------------------------------
;; The modes
;;

;;;###autoload
(define-minor-mode lisp-extra-font-lock-mode
  "Minor mode that highlights bound variables and quoted expressions in Lisp."
  :group 'lisp-extra-font-lock
  (if lisp-extra-font-lock-mode
      (lisp-extra-font-lock-add-keywords)
    (lisp-extra-font-lock-remove-keywords))
  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))


;;;###autoload
(define-global-minor-mode lisp-extra-font-lock-global-mode
  lisp-extra-font-lock-mode
  (lambda ()
    (when (apply 'derived-mode-p lisp-extra-font-lock-modes)
      (lisp-extra-font-lock-mode 1)))
  :group 'lisp-extra-font-lock)


(defun lisp-extra-font-lock-variable-face-form (name)
  "A form suitable for a font-lock face expression.

NAME is a form that should evalute to the name of the symbol, as a string."
  `(if (ignore-errors (let ((symbol (intern-soft ,name)))
                        (and symbol
                             (special-variable-p symbol))))
       lisp-extra-font-lock-special-variable-name-face
     font-lock-variable-name-face))

(defun lisp-extra-font-lock-keywords ()
  "Font-lock keywords used by `lisp-extra-font-lock'.
The keywords highlight variable bindings and quoted expressions."
  `(;; Function and lambda parameters
    (,(concat "("
              "\\(?:"
              (regexp-opt lisp-extra-font-lock-defun-functions)
              "[ \t\n]+\\_<\\(?:\\sw\\|\\s_\\)+\\_>"
              "\\|"
              (regexp-opt lisp-extra-font-lock-lambda-functions)
              "\\)"
              "[ \t\n]+(")
     (lisp-extra-font-lock-match-argument-list
      ;; Pre-match form
      (progn
        (goto-char (match-end 0))
        ;; Search limit
        (save-excursion
          (backward-char)               ; Position point before "(".
          (lisp-extra-font-lock-end-position)))
      ;; Post-match form
      nil
      (0 ,(lisp-extra-font-lock-variable-face-form '(match-string 0))
         nil t)))
    ;; Variables wrapped by asterisks
    ;; and constants wrapped by plus sign
    ("\\_<\\(?:\\*\\|\\+\\)[a-zA-Z0-9#$%:/-]+?\\(?:\\*\\|\\+\\)\\_>"
     (0 lisp-extra-font-lock-variable-use-face))
    ;; Symbol package names
    ("\\_<[a-zA-Z0-9#$%/-]+?\\:\\{1,2\\}"
     (0 font-lock-type-face))
    ;; Variables bound by `let'.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-let-functions)
              "[ \t]+(")
     (lisp-extra-font-lock-match-let
      ;; Pre-match form
      (progn
        (goto-char (match-end 0))
        ;; Search limit
        (save-excursion
          (backward-char)               ; Position point before "(".
          (lisp-extra-font-lock-end-position)))
      ;; Post-match form
      (goto-char (match-end 0))
      (0 ,(lisp-extra-font-lock-variable-face-form '(match-string 0)))))
    ;; Variables bound by `cl-dolist' etc.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-dolist-functions)
              "[ \t]+(\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
     (1 ,(lisp-extra-font-lock-variable-face-form '(match-string 1))))
    ;; Bind first argument like `condition-case'.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-bind-first-functions)
              "[ \t]+\\_<\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
     (1 (and (not (string= (match-string 1) "nil"))
             ,(lisp-extra-font-lock-variable-face-form '(match-string 1)))))
    ;; Bind variables and named arguments to `cl-loop'.
    (,(concat "("
              (regexp-opt lisp-extra-font-lock-loop-functions)
              "\\_>")
     (lisp-extra-font-lock-match-loop-keywords
      ;; Pre-match form. Value of expression is limit for submatcher.
      (progn
        (goto-char (match-end 0))
        (save-excursion
          (goto-char (match-beginning 0))
          (lisp-extra-font-lock-end-position)))
      ;; Post-match form.
      (goto-char (match-end 0))
      (1 font-lock-builtin-face)
      (2 ,(lisp-extra-font-lock-variable-face-form '(match-string 2)) nil t)))
    (;; Quote and backquote.
     ;;
     ;; Matcher: Set match-data 1 if backquote.
     lisp-extra-font-lock-match-quote-and-backquote
     (1 lisp-extra-font-lock-backquote-face nil t)
     (;; Submatcher, match part of quoted expression or comma.
      lisp-extra-font-lock-match-quoted-content
      ;; Pre-match form. Value of expression is limit for submatcher.
      (progn
        (goto-char (match-end 0))
        ;; Search limit
        (lisp-extra-font-lock-end-position))
      ;; Post-match form
      (goto-char (match-end 0))
      ;; Highlight rules for submatcher.
      (1 lisp-extra-font-lock-quoted-face append)
      (2 lisp-extra-font-lock-backquote-face nil t)))
    ;; Function read syntax
    ("#'\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>"
     1 lisp-extra-font-lock-quoted-function-face)
    (,lisp-extra-font-lock-macro-symbols-opt
     (0 lisp-extra-font-lock-macro-face))
    (,lisp-extra-font-lock-function-symbols-opt
     (0 lisp-extra-font-lock-function-call-face))))


(defvar lisp-extra-font-lock--installed-keywords nil)

(defun lisp-extra-font-lock-add-keywords ()
  "Add extra font-lock keywords to lisp."
  (set (make-local-variable 'font-lock-multiline) t)
  (when (local-variable-p 'lisp-extra-font-lock--installed-keywords)
    (font-lock-remove-keywords nil lisp-extra-font-lock--installed-keywords))
  (let ((keywords (lisp-extra-font-lock-keywords)))
    (set (make-local-variable 'lisp-extra-font-lock--installed-keywords)
         keywords)
    (font-lock-add-keywords nil keywords 'append)))


(defun lisp-extra-font-lock-remove-keywords ()
  "Remove font-lock keywords for extra lisp highlithing."
  (font-lock-remove-keywords nil lisp-extra-font-lock--installed-keywords))


;; ----------------------------------------
;; Matcher functions
;;

(defun lisp-extra-font-lock-end-position ()
  "Suitable end position of expression after point.
If expression is open-ended, the beginning of the next top-level
form is used, or `point-max' if none is found."
  (save-match-data
    (save-excursion
      (or (condition-case nil
              (progn
                (forward-sexp)
                (point))
            (error nil))
          (and (re-search-forward "^(" nil t)
               (match-beginning 0))
          (point-max)))))

(defun lisp-extra-font-lock-match-argument-list (limit)
  (forward-comment (buffer-size))
  (and (< (point) limit)
       (let ((res (looking-at "\\_<\\(?:\\sw\\|\\s_\\)+\\_>")))
         (when res
           (goto-char (match-end 0)))
         res)))


(defun lisp-extra-font-lock-match-let (limit)
  "Match next variable introduced by `let'-like constructs."
  (forward-comment (buffer-size))
  (let ((p (point)))
    (cond ((eq (following-char) ?\( )
           ;; Match "(var initial-valoue)"
           (forward-char)
           (forward-comment (buffer-size))
           (and
            (< (point) limit)
            (let ((res (looking-at "\\(?:\\sw\\|\\s_\\)+\\_>")))
              (when res
                (goto-char p)
                (condition-case nil
                    (forward-sexp)
                  (error (goto-char limit))))
              res)))
          ((looking-at "\\(?:\\sw\\|\\s_\\)+\\_>")
           ;; Match "var"
           (goto-char (match-end 0))
           (<= (point) limit))
          (t
           nil))))


(defun lisp-extra-font-lock-is-in-comment-or-string (pos)
  "Return non-nil if POS is in a comment, string, constant, or reader macro.

This assumes that Font Lock is active and has fontified comments
and strings."
  (or (nth 8 (save-excursion
	       (syntax-ppss pos)))   ; In comment or string.
      ;; Plain character constant ?<char>.
      (eq (char-before pos) ??)
      ;; Escaped character constant ?\<char>.
      (and (eq (char-before pos) ?\\)
           (eq (char-before (- pos 1)) ??))
      ;; Reader macro like #'.
      (eq (char-before pos) ?#)))


(defun lisp-extra-font-lock-match-quote-and-backquote (limit)
  "Search for quote and backquote in in code.
Set match data 1 if character matched is backquote."
  (let (res)
    (while
        (progn (setq res (re-search-forward "\\(?:\\(`\\)\\|'\\)" limit t))
               (and res
                    (lisp-extra-font-lock-is-in-comment-or-string
                     (match-beginning 0)))))
    res))


(defun lisp-extra-font-lock-match-quoted-content (limit)
  "Match next part of a quoted content.

Match up to next comma operator or quoted subexpression, or to
the end of the quoted expression."
  (and (< (point) limit)
       (let ((p (point))
             res)
         (while
             (progn
               (setq res (re-search-forward "\\(,@?\\|[`']\\)" limit t))
               (and res
                    (lisp-extra-font-lock-is-in-comment-or-string
                     (match-beginning 0)))))
         (if res
             ;; Match up to next quoted subpart or comma operator.
             (let ((is-comma (eq (char-after (match-beginning 0)) ?,)))
               (set-match-data (list
                                ;; Match data 0: Full match.
                                p (match-end 0)
                                ;; Match data 1: Part of the quoted expression
                                p
                                (match-beginning 0)
                                ;; Match data 2; Comma operator (if present)
                                (and is-comma (match-beginning 0))
                                (and is-comma (match-end 0))))
               (condition-case nil
                   (forward-sexp)
                 (error (goto-char limit))))
           ;; Match to the end of the quoted expression.
           (set-match-data (list p limit
                                 p limit))
           (goto-char limit))
         t)))

(defvar lisp-extra-font-lock-loop-keywords
  '("=" "above" "across" "across-ref" "always" "and" "append" "as"
    "being" "below" "buffer" "buffers" "by"
    "collect" "collecting" "concat" "count"
    "do" "doing" "downfrom" "downto"
    "each" "element" "elements" "else" "end"
    "extent" "extents" "external-symbol" "external-symbols"
    "finally" "frames" "from"
    "hash-key" "hash-keys" "hash-value" "hash-values"
    "if" "in" "in-ref" "initially" "interval" "intervals"
    "key-binding" "key-bindings" "key-code" "key-codes" "key-seq" "key-seqs"
    "maximize" "minimize"
    "named" "nconc" "nconcing" "never"
    "of" "of-ref" "on" "overlay" "overlays"
    "present-symbol" "present-symbols" "property"
    "repeat" "return"
    "screen" "screens" "sum" "symbol" "symbols"
    "the" "then" "thereis" "to"
    "unless" "until" "upfrom" "upto" "using"
    "vconcat"
    "when" "while" "windows")
  "List of `cl-loop' named parameters, excluding variable binding ones.")

(defvar lisp-extra-font-lock-loop-keywords-with-var '("for"
                                                      "index"
                                                      "into"
                                                      "with")
  "List of `cl-loop' named variable binding parameters.")


;; Match named loop keywords, and (optionally) any bound variables.
;;
;; Note, does not support "destructuring", i.e. binding several
;; variables using pattern matching. If this is used, the entire
;; expression is highlighted as a variable.
(defun lisp-extra-font-lock-match-loop-keywords (limit)
  "Match named keyword of `loop' and highlight variable arguments."
  (while
      (progn
        (forward-comment (buffer-size))
        (and (< (point) limit)
             (not (looking-at
                   (concat
                    "\\_<"
                    "\\("
                    (regexp-opt (append
                                 lisp-extra-font-lock-loop-keywords-with-var
                                 lisp-extra-font-lock-loop-keywords))
                    "\\)"
                    "\\_>")))))
    (condition-case nil
        (forward-sexp)
      (error (goto-char limit))))
  (if (not (< (point) limit))
      nil
    (goto-char (match-end 0))
    (when (member (match-string 1) lisp-extra-font-lock-loop-keywords-with-var)
      (forward-comment (buffer-size))
      (let ((var-start (point)))
        (when (condition-case nil
                  (progn
                    (forward-sexp)
                    t)
                (error nil))
          (set-match-data (list
                           (match-beginning 0)
                           (point)
                           (match-beginning 1)
                           (match-end 1)
                           var-start
                           (point))))))
    t))

(provide 'lisp-extra-font-lock)

;;; lisp-extra-font-lock.el ends here.
