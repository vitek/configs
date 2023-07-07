;; Based on https://github.com/dedi/gxref/blob/master/gxref.el

;;(add-to-list 'xref-backend-functions 'yaml-xref-backend)

(require 'cl-lib)
(require 'xref)


;;;###autoload
(defcustom yaml-xref-exe "yaml-xref"
  "Path to yaml-xref executable."
  :type 'string
  :group 'yaml-xref)

;;;###autoload
(defun yaml-xref-backend () 'yaml-xref)

(defun yaml-xref--symbol-at-point ()
  "Return the symbol at point, or nil if none is found."
  (let ((thing (thing-at-point 'filename)))
    (if thing (intern thing))))


(cl-defmethod xref-backend-identifier-at-point ((_backend (eql yaml-xref)))
  (let ((current-symbol (yaml-xref--symbol-at-point)))
    (when current-symbol
      (symbol-name current-symbol))))

(cl-defmethod xref-backend-definitions ((_backend (eql yaml-xref)) symbol)
  (yaml-xref--find-symbol symbol))

;; (cl-defmethod xref-backend-references ((_backend (eql yaml-xref)) symbol)
;;    (yaml-xref--find-symbol symbol "-r"))

;; (cl-defmethod xref-backend-apropos ((_backend (eql yaml-xref)) symbol)
;;   (yaml-xref--find-symbol symbol "-g"))

(defun yaml-xref--find-symbol (symbol &rest args)
  "Run GNU Global to find a symbol SYMBOL.
Return the results as a list of xref location objects.  ARGS are
any additional command line arguments to pass to GNU Global."
  (let* ((process-args
          (append args
                  (list (buffer-file-name)
                        symbol)))
         (global-output (yaml-xref--to-list process-args)))
    (remove nil
            (mapcar #'yaml-xref--make-xref-from-gtags-x-line global-output)
            )))



(defun yaml-xref--prepare-process-environment()
  "Figure out the process environment to use for running GLOBAL/GTAGS"
   process-environment
  )


(defun yaml-xref--to-list (args)
  "Run GNU Global in an external process.
Return the output as a list of strings.  Return nil if an error
occured.  ARGS is the list of arguments to use when running
global"
  (let ((process-environment (yaml-xref--prepare-process-environment)))
  (condition-case nil
      (apply #'process-lines yaml-xref-exe args)
    (error nil))))

(defun yaml-xref--make-xref-from-file-loc (file line column desc)
  "Create an xref object pointing to the given file location.
FILE, LINE, and COLUMN point to the location of the xref, DESC is
a description of it."
  (xref-make desc (xref-make-file-location file line column)))


(defun yaml-xref--make-xref-from-gtags-x-line (ctags-x-line)
  "Create and return an xref object pointing to a file location.
This uses the output of a based on global -x output line provided
in CTAGS-X-LINE argument.  If the line does not match the
expected format, return nil."
  (if (string-match
       "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t\]+\\)[ \t]+\\(.*\\)"
       ctags-x-line)
      (yaml-xref--make-xref-from-file-loc
            (match-string 1 ctags-x-line)
            (string-to-number (match-string 2 ctags-x-line))
            (string-to-number (match-string 3 ctags-x-line))
            (match-string 4 ctags-x-line))
    ))

(provide 'yaml-xref)
