;;;; bezier.lisp

(in-package #:bezier)

;;; "bezier" goes here. Hacks and glory await!

(defparameter *lines* nil)
(defparameter *lines-stream* nil)
(defparameter *running* nil)

(defun-g vert ((point :vec3))
  (v! point 1.0))
(def-glsl-stage geom-glsl (&context :330 :geometry)
  "
layout(points) in;
layout(points, max_verticies=5) out;

/**
*	Main
*/
void main()
{
	gl_PositionIn[0].xyz;
  gl_Position = gl_ModelViewProjectionMatrix * vec4( gl_PositionIn[0].xyz, 1.0 );

	EmitVertex();

	EndPrimitive();
}" ())
(defun-g frag ()
  (v! 1.0 0.0 0.0 1.0))

(def-g-> main-prog (:points)
  :vertex #'vert
  :geometry #'geom-glsl
  :fragment #'frag)

(defun step-demo ()
  (clear)
  (map-g #'main-prog *lines-stream*)
  (swap))

(defun run-loop ()
  (setf *running* t)
  (setf *lines* (make-gpu-array
                 (list (v! -1.0 1.0 -0.1)
                       (v! 1.0 -1.0 -0.1))
                 :element-type :vec3
                 :dimensions 2))
  (setf *lines-stream* (make-buffer-stream *lines* :retain-arrays t))
  (gl:depth-func :lequal)
  (loop :while (and *running* (not (shutting-down-p))) :do
    (continuable
      (step-demo)
      (livesupport:update-repl-link))))



