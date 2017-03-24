;;;; bezier.lisp

(in-package #:bezier)

;;; "bezier" goes here. Hacks and glory await!

(defparameter *lines* nil)
(defparameter *lines-stream* nil)
(defparameter *running* nil)

(defstruct-g g-poc
  (position :vec3 :accessor pos)
  (control :vec3 :accessor control)
  (color :vec4 :accessor col))

(defun-g vert ((point g-poc))
  (values (v! (pos point) 1.0)
          (v! (control point) 1.0)
          (col point)))

(defun-g geom ()
  (setq gl-position (v! 1.0 1.0 1.0 1.0))
  (emit-vertex)
  (values))

(def-glsl-stage geom-glsl (("control" (:vec4 (*))) ("color" (:vec4 (*)))
                                                   &uniform
                                                   ("transform" :mat4)
                                                   ("detail" :int)
                                                   &context :330 :geometry)
  "
layout(lines) in;
layout(line_strip, max_vertices=20) out;

vec3 evaluateBezierPosition( vec3 v[4], float t )
{
    vec3 p;
    float OneMinusT = 1.0 - t;
    float b0 = OneMinusT*OneMinusT*OneMinusT;
    float b1 = 3.0*t*OneMinusT*OneMinusT;
    float b2 = 3.0*t*t*OneMinusT;
    float b3 = t*t*t;
    return b0*v[0] + b1*v[1] + b2*v[2] + b3*v[3];
}

/**
*	Main
*/
void main()
{
  color_out = color[0];
  vec3 pos[4];
  pos[0] = gl_in[0].gl_Position.xyz;
  pos[1] = control[0].xyz;
  pos[2] = control[1].xyz;
  pos[3] = gl_in[1].gl_Position.xyz;
  float OneOverDetail = 1.0 / float(detail-1.0);
  for( int i=0; i<detail; i++ )
  {
    float t = i * OneOverDetail;
    vec3 p = evaluateBezierPosition( pos, t );
    gl_Position = transform * vec4( p.xyz, 1.0 );
    EmitVertex();
  }

	EndPrimitive();
}" (("color_out" :vec4)))



(defun-g frag ((color :vec4))
  color)

(def-g-> main-prog (:line-strip)
  :vertex #'(vert g-poc)
  :geometry #'(geom-glsl (:vec4 (*)) (:vec4 (*)))
  :fragment #'(frag :vec4))

(defun step-demo ()
  (clear)
  (map-g #'main-prog *lines-stream* :transform (m4:scale (v! 1.0 1.0 1.0)) :detail 20)
  (swap))

(defun run-loop ()
  (setf *running* t)
  (setf *lines* (make-gpu-array
                 (list (list (v! -1.0 1.0 -0.1) (v! -1.0 0.0 0.0) (v! 0.0 0.0 1.0 1.0))
                       (list (v! 1.0 -1.0 -0.1) (v! 1.0 0.0 0.0) (v! 0.0 1.0 0.0 1.0)))
                 :element-type 'g-poc
                 :dimensions 2))
  (setf *lines-stream* (make-buffer-stream *lines* :retain-arrays t))
  (gl:depth-func :lequal)
  (loop :while (and *running* (not (shutting-down-p))) :do
    (livesupport:continuable
      (step-demo)
      (livesupport:update-repl-link))))



