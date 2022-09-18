(in-package :cl-xrandr-dmenu)

(defparameter *xrandrcmd* "xrandr")
(defparameter *launchercmd* "dmenu")
(defvar *launcherparam* (list "-i"
                              "-b")
  "options for `*launchercmd'")

(defparameter *ui*
  (adopt:make-interface
   :name "xrandr interface"
   :summary "Manipulating xrandr Outputs with a dmenu interface"
   :usage "[OPTIONS]"
   :help "This script is intended to replace a bunch of my bash scripts for my monitor setup. Right now it is able to turn xrandr outputs on and off manipulate their relative position and change which one is primary."
   :contents (list
              (adopt:make-option 'help
                                 :long "help"
                                 :short #\h
                                 :help "Display help and exit"
                                 :reduce (constantly t))
              (adopt:make-group 'window-manager
                                :title "WM Options"
                                :help "These options handle things specifc to the windowmanager in use."
                                :options
                                (list
                                 (adopt:make-option 'restart-i3
                                                    :long "restart-i3"
                                                    :short #\i
                                                    :help "if this option is spepcified i3 will be restarted after every xrandr action. Probably causes problems if i3 is not installed or running."
                                                    :reduce (constantly t)))))))

;; #############
;; xrandr parser
;; #############

(defun constructcmd (cmd &optional arg)
  (format nil "~{~a~^ ~}"  (append (list cmd) arg)))

(defun call-xrandr (&rest xrandrarg)
  (uiop:run-program (constructcmd *xrandrcmd* xrandrarg)
    :output :lines))


(defun connected-output-p (string)
  (if (and (search "connected" string)
           (not (search "disconnected" string)))
      string
      nil))

(defun connected-outputs ()
  (remove nil (map 'list #'connected-output-p (call-xrandr))))

(defun primary-output ()
  (funcall (lambda (x) (when x (intern (car(str:words x))
                                  :cl-xrandr-dmenu)))
           (car (remove nil (mapcar
                             #'(lambda (x)
                                 (when (search "primary" x) x))
                             (connected-outputs))))))

(defun primary-output-p (output)
  (eq output (primary-output)))

(defun list-monitors ()
  (mapcar #'(lambda (string)
              (intern (car (last (str:words string)))
                      :cl-xrandr-dmenu))
          (cdr (call-xrandr  "--listmonitors"))))

(defun list-outputs ()
  (mapcar #'(lambda (string) (intern (car (str:words string))
                                :cl-xrandr-dmenu))
       (connected-outputs)))

(defun output-on-p (output)
  (when (remove nil (member output (list-monitors)))
    t))

;; ###############
;; dmenu interface
;; ###############

(defun request-input (prompt options)
  (uiop:with-input (input-options (format nil "~{~a~^~%~}" options))
    (uiop:run-program (constructcmd *launchercmd* (append (list "-p" prompt)
                                                          *launcherparam*))
                      :input input-options
                      :output '(:string :stripped t))))

(defun verify-input (input options)
  "checks is `input' is in list `options'. If not throws an error."
  (if (member input options)
      input
      (error "Bad input. Use one of the provided options.")))

(defun request-safe-input (prompt options)
  "calls `request-input' through `verify-input' which checks whether the provided
input is in the list of options.

This function assumes that the list of options is a list of symbols and calls
`intern' on the input."
  (verify-input (intern (request-input prompt options)
                        :cl-xrandr-dmenu)
                options))

;; ###############
;; xrandr actions
;; ###############

(defun make-primary (output &optional outputs)
  "Make the provided `output' the primar output. The second argument `outputs'
does nothing. It is only needed to unify the `funcall' for all xrandr actions"
  (declare (ignore outputs))
  (call-xrandr "--output" output "--primary" ))

(defun display-off (output outputs)
  "Turn off `output'"
  (when (primary-output-p output)
    (let ((outputs (remove output outputs)))
      (if (eq 1 (length outputs))
          (make-primary (car outputs))
          (make-primary (request-safe-input "Primary?" outputs)))))
  (call-xrandr "--output" output "--off"))

(defun output-position (output outputs)
  (let* ((options-alist '((left . "--left-of")
                          (right . "--right-of")
                          (above . "--above")
                          (below . "--below")))
         (options (mapcar #'car options-alist))
         (outputs (remove output outputs)))
    (call-xrandr  "--output"
                  output
                  "--auto"
                  (cdr (assoc (request-safe-input "position?" options)
                              options-alist))
                  (if (= 1 (length outputs))
                      (car outputs)
                      (request-safe-input "of?" outputs)))))

(defun rotate-output (output outputs)
  (declare (ignore outputs))
  (let* ((options '(normal
                    right
                    left)))
    (call-xrandr "--output" output
                 "--auto"
                 "--rotate" (string-downcase (request-safe-input "rotation?" options)))))

(defparameter *actions* '(display-off
                          output-position
                          make-primary
                          rotate-output)
  "list of functions that take two arguments `output' and `outputs' where the
former is a symbol that is an element of the latter list")

(defun output-fix ()
  "Fixes issues when the cable is puled out without diabling the output. In that
case there are more monitors than outputs, so we check for that and disable the
extra monitor. There are probably cases that this doesn't catch, but it works
for waht I'm regularly dealing with."
  (let ((monitors (list-monitors))
        (outputs (list-outputs)))
    (when (< (length outputs)
             (length monitors))
      (loop for monitor in monitors
           do (unless (member monitor outputs)
                (display-off monitor outputs)))
     (uiop:run-program "notify-send 'Fixed Monitor/Output mismatch'"))))


;; ####################
;; main functions
;; ####################

(defun main-menu ()
  (output-fix)
  (let* ((outputs (list-outputs))
         (output (request-safe-input "Output?" outputs))
         (action
           (request-safe-input
            "Action?"
            (funcall (lambda (x) (if (car x)
                        x
                        (return-from main-menu)))
              (cond
               ;; Only one ouput active, so we don't allow disabling that
               ((and (output-on-p output)
                     (= 1 (length (list-monitors))))
                (remove 'display-off
                        ;; if the only connected ouput is also the primary ouput
                        ;; we remove the option to make it primary.
                        (funcall (lambda (x) (if (primary-output-p output)
                                            (remove 'make-primary x)
                                            x))
                                 ;; Also positioning is irrelevant in this case
                                 ;; but might cause issues because we expect a
                                 ;; second output as reference.
                                 (remove 'output-position *actions*))))
               ;; Check if Output is off if yes remove the option to turn it
               ;; off.
               ((not (output-on-p output))
                (remove 'make-primary
                        (remove 'display-off *actions*)))
               ;; Check if Output is primary, if yes remove option to make
               ;; it so. Has no effect if there's only one output because
               ;; then case 1 applies.
               ((primary-output-p output)
                (remove 'make-primary *actions*))
               ;; Pass on all actions
               (t *actions*))))))
    (funcall action output outputs)))

(defun main ()
  (handler-case
      (multiple-value-bind (arguments options)
          (adopt:parse-options *ui*)
        (declare (ignore arguments))
        (when (gethash 'help options)
         (adopt:print-help-and-exit *ui*))
        (main-menu)
        (when (gethash 'restart-i3 options)
          (uiop:run-program "i3-msg restart")))
    (error (c)
      (adopt:print-error-and-exit c))))
