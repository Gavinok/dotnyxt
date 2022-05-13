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

(define-configuration nyxt/web-mode:web-mode
  ((nyxt/web-mode::keymap-scheme
    ;; This will only works in >2.2.1. Change it to the hash-table way
    ;; below to make it work in <2.2.
    (nyxt::define-scheme (:name-prefix "web" :import %slot-default%)
      ;; If you want to have VI bindings overriden, just use
      ;; `scheme:vi-normal' or `scheme:vi-insert' instead of
      ;; `scheme:emacs'.
      scheme:emacs
      (list
       "C-c p"          'copy-password
       "C-c y"          'autofill
       "C-f"            'nyxt/web-mode:history-forwards-maybe-query
       "C-i"            'nyxt/input-edit-mode:input-edit-mode
       "M-:"            'eval-expression
       "M-s M-l"        'search-buffer
       "M-s M-L"        'search-buffers
       "M-x"            'execute-command
       "M-g M-v"        'hint-mpv
       "M-V"            'youtube-play-current-page
       "C-c l"          'org-protocal
       "C-x M-s"        'start-slynk
       "C-w"            'nyxt/input-edit-mode:delete-backwards-word
       "C-f"            'nyxt/input-edit-mode:cursor-forwards
       "C-b"            'nyxt/input-edit-mode:cursor-backwards
       "M-f"            'nyxt/input-edit-mode:cursor-forwards-word
       "M-b"            'nyxt/input-edit-mode:cursor-backwards-word
       "C-d"            'nyxt/input-edit-mode:delete-forwards
       "M-d"            'nyxt/input-edit-mode:delete-forwards-word
       "M-`"            'hsplit
       )))
    (nyxt/web-mode::box-style (theme:themed-css (theme *browser*)
                  (".nyxt-hint"
                   :background-color theme:primary
                   :opacity 1
                   :color "white"
                   :font-weight "bold"
                   :padding "0px 3px 0px 3px"
                   :border-radius "1px"
                   ;; :z-index #.(1- (expt 2 31))
                   :box-shadow "rgb(38, 57, 77) 0px 20px 30px -10px"))
              :documentation "The style of the boxes, e.g. link hints.")))

(define-configuration browser
  (;; This is for Nyxt to never prompt me about restoring the previous session.
   (session-restore-prompt :never-restore)
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

(defmethod customize-instance ((buffer buffer) &key)
  (nyxt/emacs-mode:emacs-mode :buffer buffer))

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
  "Start a Slynk server that can be connected to, for instance, in
Emacs via SLY.

Warning: This allows Nyxt to be controlled remotely, that is, to execute
arbitrary code with the privileges of the user running Nyxt.  Make sure
you understand the security risks associated with this before running
this command."
  (slynk:create-server :port slynk-port :dont-close t)
  (echo "Slynk server started at port ~a" slynk-port))

;; taken from https://github.com/aartaka/nyxt-config/blob/master/commands.lisp
(define-command-global eval-expression ()
  "Prompt for the expression and evaluate it, echoing result to the `message-area'."
  (let ((expression-string
          ;; Read an arbitrary expression. No error checking, though.
          (first (prompt :prompt "Expression to evaluate"
                         :sources (list (make-instance 'prompter:raw-source))))))
    ;; Message the evaluation result to the message-area down below.
    (echo "~S" (eval (read-from-string expression-string)))))

;; TODO add support for the "capture" protocol which requires the current selection
(define-command-global org-protocal (&optional (protocol "store-link") (buffer (nyxt:current-buffer)))
  "Using the supported org-protocol type PROTOCOL execute it against
the given BUFFER's current url."
  (let ((url (render-url (nyxt:url buffer)))
        (title (title buffer)))
    (uiop:launch-program (list "xdg-open"
                               (str:concat "org-protocol://" protocol "?"
                                           "url=" url
                                           "&title=" title
                                           ;; "&body=""$QUTE_SELECTED_TEXT"
                                           )))))

(setf (uiop:getenv "GTK_THEME") "Adwaita:dark")

(define-configuration browser
  ((session-restore-prompt :always-restore)
   (nyxt/web-mode:hints-alphabet "DSJKHLFAGNMXCWEIO")
   (theme (make-instance
           'theme:theme
           :dark-p t
           :background-color "#000000"
           :text-color "#CDCDCD"
           :accent-color "#7D8FA3"
           :primary-color "#7D8FA3"
           :secondary-color "#8fafd7"
           :tertiary-color "#7D8FA3"
           :quaternary-color "#000000"))))

#+nyxt-3
(nyxt::define-panel-global hsplit (&key (buffer (id (current-buffer))))
                           (panel "Duplicate panel" :right)
                           "Duplicate the current buffer URL in the panel buffer on the right.
A poor man's hsplit :)"
                           (progn
                             (ffi-window-set-panel-buffer-width (current-window) panel 750)
                             (run-thread "URL loader"
                                         (buffer-load (url (nyxt::buffers-get buffer)) :buffer panel))
                             ""))

#+nyxt-3
(define-command-global close-all-panels ()
  "Close all the panel buffers there are."
  (when (panel-buffers-right (current-window))
    (delete-panel-buffer :window (current-window) :panels (panel-buffers-right (current-window))))
  (when (panel-buffers-left (current-window))
    (delete-panel-buffer :window (current-window) :panels (panel-buffers-left (current-window)))))

#+nyxt-3
(define-command-global hsplit ()
  "Based on `hsplit-panel' above."
  (if (panel-buffers-right (current-window))
      (close-all-panels)
    (hsplit-panel)))

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
