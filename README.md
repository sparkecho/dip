# dip
Digital image processing library.

## Install
Make sure `GLFW` and `OpenGL` are installed, as they are used to display image.
``` shell
$ cd ~/quicklisp/local-projects
$ git clone https://github.com/sparkecho/dip.git
```

## Usage

``` common-lisp
CL-USER> (ql:quickload :dip)
```

## Example

``` common-lisp
CL-USER> (defparameter *img* (dip:imread "~/quicklisp/local-projects/dip/data/lena.jpg"))
*img*
;; Display image, but you will find that color is not right.
CL-USER> (dip:imshow *img* "lena")
;; This is because in the result of cl-jpeg channels are arranged as BGR not RGB.
;; Convert color from BGR to RGB.
CL-USER> (dip:convert *img* :bgr->rgb)
;; Display again, now the output looks right.
CL-USER> (dip:imshow *img* "lena")
;; We can do something to the image, such as flip horizontal:
CL-USER> (dip:flip *img* :h)
;; Display it, and we will see it works.
CL-USER> (dip:imshow *img* "lena")
;; This image can be saved with function imwrite.
;; But we should change image from RGB back to BGR to write it.
CL-USER> (dip:convert *img* :bgr->rgb)
CL-USER> (dip:imwrite "~/fliped_lena.jpg" *img*)
```
