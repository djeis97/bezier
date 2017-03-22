;;;; bezier.lisp

(in-package #:bezier)

;;; "bezier" goes here. Hacks and glory await!

(defparameter *lines* nil)
(defparameter *lines-stream* nil)
(defparameter *running* nil)


(defun-g vert ((point g-pc))
  (values (v! (pos point) 1.0)
          (col point)))

(defun-g geom ()
  (setq gl-position (v! 1.0 1.0 1.0 1.0))
  (emit-vertex)
  (values))

(def-glsl-stage geom-glsl (("color" (:vec4 (*))) &uniform ("transform" :mat4)
                                                 &context :330 :geometry)
  "
layout(lines) in;
layout(line_strip, max_vertices=5) out;

/**
*	Main
*/
void main()
{
  gl_Position = vec4( gl_in[0].gl_Position.xyz, 1.0 );
  color_out = color[0];
	EmitVertex();
  gl_Position = vec4( gl_in[1].gl_Position.xyz, 1.0 );
  color_out = color[1];

	EmitVertex();

  EndPrimitive();
  gl_Position = transform * vec4( -gl_in[0].gl_Position.x,
                                   gl_in[0].gl_Position.yz,
                                   1.0 );
  color_out = color[1];

	EmitVertex();
  gl_Position = transform * vec4( -gl_in[1].gl_Position.x,
                                   gl_in[1].gl_Position.yz,
                                   1.0 );
  color_out = color[0];
	EmitVertex();

	EndPrimitive();
}" (("color_out" :vec4)))



(defun-g frag ((color :vec4))
  color)

(def-g-> main-prog (:lines)
  :vertex #'(vert g-pc)
  :geometry #'(geom-glsl (:vec4 (*)))
  :fragment #'(frag :vec4))

(defun step-demo ()
  (clear)
  (map-g #'main-prog *lines-stream* :transform (m4:scale (v! 1.0 0.5 1.0)))
  (swap))

(defun run-loop ()
  (setf *running* t)
  (setf *lines* (make-gpu-array
                 (list (list (v! -1.0 1.0 -0.1) (v! 0.0 0.0 1.0 1.0))
                       (list (v! 1.0 -1.0 -0.1) (v! 0.0 1.0 0.0 1.0)))
                 :element-type 'g-pc
                 :dimensions 2))
  (setf *lines-stream* (make-buffer-stream *lines* :retain-arrays t))
  (gl:depth-func :lequal)
  (loop :while (and *running* (not (shutting-down-p))) :do
    (livesupport:continuable
      (step-demo)
      (livesupport:update-repl-link))))



