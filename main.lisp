;;;; main.lisp

(in-package #:multi-test)

(defgame multiplayer ()
  ((ui :initform (make-hash-table)))
  (:viewport-width 200)
  (:viewport-height 150)
  (:viewport-title "Multiplayer Test"))

(defmethod ui-element ((this multiplayer) elt)
  (gethash elt (slot-value this 'ui)))

(defmethod (setf ui-element) (value (this multiplayer) elt)
  (setf (gethash elt (slot-value this 'ui)) value))

(defmethod draw-ui ((this multiplayer))
  (loop :for w :being :the :hash-value :of (slot-value this 'ui)
        :when w :do (draw-widget w)))

(defmacro initialize-ui (state &rest forms)
  `(progn
     ,@(loop :for (k . v) :in forms
             :collect `(unless (ui-element ,state ,k)
                         (setf (ui-element ,state ,k) ,@v)))))

(defvar *mouse-pos* (vec2 0 0))
(defvar *pid* 0)

(defvar *quit-hooks* nil)
(defvar *mouse-hooks*
  (list #'ui-mouse-handler
        #'(lambda (x y) (setf *mouse-pos* (vec2 x y)))))
(defvar *act-hooks* nil)

(defun quit-game ()
  (gamekit:stop)
  (loop :for h :in *quit-hooks*
        :do (funcall h)))

(defun start-hosting ()
  (push (spawn-server 7001 #'recv-server #'send :buffer-size 4) *quit-hooks*)
  (join-host))

(defun join-host ()
  (multiple-value-bind (send-fn close-fn) (create-client "127.0.0.1" 7001 #'recv-client :buffer-size 4)
    (push #'(lambda () (funcall send-fn (pack *pid* :v *mouse-pos*))) *act-hooks*)
    (push close-fn *quit-hooks*)))

(defmethod post-initialize ((this multiplayer))
  (bind-cursor (lambda (x y)
                 (loop :for l :in *mouse-hooks*
                       :do (funcall l x y))))
  (bind-button :mouse-left :pressed
               #'ui-click-handler)
  (bind-button :mouse-left :released
               #'ui-release-handler)
  (initialize-ui this
    (:host-btn (gamekit.ui:make-button :label (make-label "Host")
                                       :position (vec2 10 50)
                                       :fill-color gamekit.colors:+red+
                                       :pressed-color gamekit.colors:+darkred+
                                       :on-click #'(lambda ()
                                                     (setf (ui-element this :host-btn) nil
                                                           (ui-element this :join-btn) nil)
                                                     (start-hosting))))
    (:join-btn (gamekit.ui:make-button :label (make-label "Join")
                                       :position (vec2 10 20)
                                       :fill-color gamekit.colors:+red+
                                       :pressed-color gamekit.colors:+darkred+
                                       :on-click #'(lambda ()
                                                     (setf (ui-element this :host-btn) nil
                                                           (ui-element this :join-btn) nil)
                                                     (join-host))))
    (:quit-btn (gamekit.ui:make-button :label (make-label "Quit")
                                       :position (vec2 10 0)
                                       :fill-color gamekit.colors:+red+
                                       :pressed-color gamekit.colors:+darkred+
                                       :on-click #'quit-game))))

(defmethod gamekit:act ((this multiplayer))
  (loop :for l :in *act-hooks*
        :do (funcall l)))

(defmethod gamekit:draw ((this multiplayer))
  (draw-rect gamekit::+origin+ 200 150 :fill-paint +white+)
  (draw-ui this)
  (draw-rect *mouse-pos* 10 10 :fill-paint +skyblue+)
  (loop :for k :being :the :hash-keys :of *remote-hosts*
        :when (and *pid* (not (equal *pid* k)))
          :do (draw-rect (gethash k *remote-hosts*) 10 10 :fill-paint +darkred+)))
