(in-package #:nyxt-user)
;;; Custom keymap
;;; MPV Support
(defun mpv (url)
  "MPV launches with given url using the fast profile."
  (uiop:run-program (list "mpv" url "&")))

;; Let's create a function to hint videos, convert the url to a sting, and play them in MPV
(define-command-global hint-mpv ()
  "Show a set of element hints, and copy the URL of the user inputted one."
  (nyxt/web-mode:query-hints "Copy element URL"
                             (lambda (nyxt/web-mode::result)
                               ;; this converts the url to a string to be used in mpv
                               (let*
                                   ((url
                                     (format nil "~a"
                                             (url (first nyxt/web-mode::result)))))
                                 ;; here we take that string and pipe it into mpv
                                 (mpv url)))
                             ))

(define-command youtube-play-current-page ()
  "Watch a Youtube video with mpv"
  (uiop:run-program
   (list "mpv" (render-url (url (current-buffer))))))

;; Let's create a function to hint videos, convert the url to a sting, and download with ytdl
#|
buffer is a class that all interfaces will inharet from
it takes a list of configurations
|#
;; overflow : hidden for status css
(define-configuration prompt-buffer
  ((keymap-scheme-name scheme:emacs)))

(define-configuration nyxt/web-mode:web-mode
  ((keymap-scheme
    (define-scheme (:name-prefix "web" :import %slot-default%)
      scheme:emacs
      (list
       "C-c u"    'copy-username
       "C-c p"    'copy-password
       "C-c y"    'autofill
       "M-:"      'eval-expression
       "M-s M-l"  'nyxt/web-mode:search-buffer
       "M-s M-L"  'nyxt/web-mode:search-buffers
       "M-x"      'execute-command
       "M-g M-v"  'hint-mpv
       "M-V"      'youtube-play-current-page
       "C-c l"    'org-protocol
       "C-c c"    'org-capture
       "C-x M-s"  'start-slynk
       "C-i"      'nyxt/input-edit-mode:input-edit-mode
       "C-w"      'nyxt/input-edit-mode:delete-backwards-word
       "C-f"      'nyxt/input-edit-mode:cursor-forwards
       "C-b"      'nyxt/input-edit-mode:cursor-backwards
       "M-f"      'nyxt/input-edit-mode:cursor-forwards-word
       "M-b"      'nyxt/input-edit-mode:cursor-backwards-word
       "C-d"      'nyxt/input-edit-mode:delete-forwards
       "M-d"      'nyxt/input-edit-mode:delete-forwards-word
       )))
   (nyxt/web-mode::box-style
    (theme:themed-css (theme *browser*)
                      ;; More consistent and in my opinion better looking hints
                      (".nyxt-hint"
                       :background-color theme:background
                       :border-style solid
                       :font-size "10pt"
                       :border-color theme:accent
                       :border-width "thin"
                       :color theme:primary
                       :opacity 1
                       :color "white"
                       :padding "2px 2px 2px 2px"
                       :border-radius "2px")))))

(define-configuration browser
  (;; This is for Nyxt to never prompt me about restoring the previous session.
   (autofills (list (make-autofill :name "First Name" :fill "Gavin")
                    (make-autofill :name "Last Name" :fill "Jaeger-Freebron")
                    (make-autofill :name "Name" :fill "Gavin Jaeger-Freebron")
                    (make-autofill :name "Email" :fill "gavinfreeborn@gmail.com")
                    (make-autofill :name "Hello Printer" :fill
                                   (lambda () (format nil "hello!")))))))

;;; Colors
;; Message buffer is the small line down below where all the messages
;;; are displayed. echo-area in Emacs parlance?
(define-configuration window
  ((message-buffer-style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "black"
         :color "white")))))))

;;; Dark is a simple mode for simple HTML pages to color those in a
;;; darker palette. I don't like the default gray-ish colors,
;;; though. Thus, I'm overriding those to be a bit more laconia-like.
(define-configuration nyxt/style-mode:dark-mode
  ((style #.(cl-css:css
             '((*
                :background-color "black !important"
                :background-image "none !important"
                :color "white")
               (a
                :background-color "black !important"
                :background-image "none !important"
                :color "#7D8FA3 !important"))))))

;;; Modules
(load "~/.cache/quicklisp/setup.lisp")
;;;; SLY setup
(ql:quickload :slynk)
(define-command-global start-slynk (&optional (slynk-port slynk::default-server-port))
  "Start a Slynk server that can be connected to, for instance, in Emacs
via SLY.

Warning: This allows Nyxt to be controlled remotely, that is, to
execute arbitrary code with the privileges of the user running Nyxt.
Make sure you understand the security risks associated with this
before running this command."
  (slynk:create-server :port slynk-port :dont-close t)
  (echo "Slynk server started at port ~a" slynk-port))

;; taken from https://github.com/aartaka/nyxt-config/blob/master/commands.lisp
(define-command-global eval-expression ()
  "Prompt for the expression and evaluate it, echoing result to the
`message-area'."
  (let ((expression-string
         ;; Read an arbitrary expression. No error checking, though.
         (first (prompt :prompt "Expression to evaluate"
                        :sources (list (make-instance 'prompter:raw-source))))))
    ;; Message the evaluation result to the message-area down below.
    (echo "~S" (eval (read-from-string expression-string)))))

(defun eval-elisp-helper (elisp)
  "Evaluate ELISP in the current thread. If it can not be completed
within 5minutes fail."
  (read-from-string (uiop:run-program
                     (list "timeout" "--signal=9" "5m" "emacsclient" "--eval" elisp)
                     :output :string)))

(defmacro eval-elisp (elisp)
  "Convert ELISP into lowercase string and send it to be processed by
Emacs."
  `(eval-elisp-helper ,(write-to-string elisp
                                        :case :downcase)))

(defun get-org-templates ()
  (read-from-string (uiop:run-program
                     (list "timeout" "--signal=9" "5m" "emacsclient" "--eval"
                           "org-capture-templates")
                     :output :string)))

(defun template-description (template)
  (make-instance 'prompter:suggestion
                 :value (first template)
                 :attributes `(("Key" ,(first template))
                               ("Description" ,(second template))
                               ("Template" ,(car (last template))))))


(defun make-org-template-source ()
  "Create a prompt source based on the currently running emacs's
org-templates"
  (make-instance 'prompter:source
                 :name "Emacs Capture Templates"
                 :constructor (mapcar #'template-description
                                      ;; Ensure that we are not listing prefix keys
                                      ;; e.g. m in the me
                                      (remove-if-not #'cddr (get-org-templates)))))

(define-command-global org-capture (&optional (buffer (nyxt:current-buffer)))
  "Call org-protocol whith the type of capture using the BUFFER for
context."
  (org-protocol "capture" buffer))

(define-command-global org-protocol
    (&optional (protocol "store-link") (buffer (nyxt:current-buffer)))
  "Using the supported org-protocol type PROTOCOL execute it against the
given BUFFER's current url."
  (let ((url (render-url (nyxt:url buffer)))
        (title (title buffer))
        (body (quri:url-encode (%copy)))
        (protocol-str (format nil "org-protocol://~a?" protocol))
        (capture-template (when (equal protocol "capture")
                            (format nil "template=~a&"
                                    (first (prompt :prompt "Select a capture template"
                                                   :sources (list (make-org-template-source))))))))
    (uiop:launch-program (list "emacsclient"
                               ;; nyxt:*open-program* would also be an alternative
                               (str:concat
                                protocol-str
                                capture-template
                                (format nil "url=~a&title=~a&body=~a"
                                        url title body))))))

(setf (uiop:getenv "GTK_THEME") "Adwaita:dark")

(define-configuration browser
  ((session-restore-prompt :always-restore)
   (nyxt/web-mode:hints-alphabet "DSJKHLFAGNMXCWEIO")
    (theme (make-instance
            'theme:theme
            :dark-p t
            :background-color "#000000"
            :text-color "#CDCDCD"
            :accent-color "#85A7A5"
            :primary-color "#7D8FA3"
            :secondary-color "#8fafd7"
            :tertiary-color "#7D8FA3"
            :quaternary-color "#000000"))))

(define-configuration nyxt/style-mode:dark-mode
  ((style #.(cl-css:css
             '((*
                :background-color "black !important"
                :background-image "none !important"
                :color "white")
               (a
                :background-color "black !important"
                :background-image "none !important"))))))

(asdf:load-system :hermes)
;; https://github.com/aartaka/nx-dark-reader
(asdf:load-system :nx-dark-reader)
(define-configuration web-buffer
  ((default-modes `(nyxt/emacs-mode:emacs-mode
                    nx-dark-reader:dark-reader-mode
                    ,@%slot-default%))))
