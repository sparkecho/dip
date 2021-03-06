# dip
Digital image processing library.

## Install
``` shell
$ cd ~/quicklisp/local-projects
$ git clone https://github.com/sparkecho/dip.git
```

## Usage

``` common-lisp
CL-USER> (ql:quickload :dip)
```

## Dependencies
### Common Lisp Libraries
| Purposes           | Dependencies                      |
|--------------------|-----------------------------------|
|Image display       | clx, cl-opengl, cl-glfw3, sdl2    |
|Image encode/decode | cl-jpeg, pngload, zpng            |
|Auxiliary           | alexandria                        |
### Foreign Libraries
#### Image display
If you are using UNIX/Linux as CLX backend is supported, you can use clx backend to display image, without any other dependencies.
Otherwise you need to make sure `GLFW3` and `OpenGL` are installed or `SDL2` is installed, as they are used to display image.
To display an image you can choose from different backends, different dependencies are required respectly:

| Backends | Dependencies            |
|----------|-------------------------|
| GLFW3    | GLFW3, OpenGL(> 3.3)    |
| SDL2     | SDL2                    |
| CLX      | None (UNIX/Linux only)  |

## Example

``` common-lisp
CL-USER> (defparameter *img* (dip:imread "~/quicklisp/local-projects/dip/data/lena.jpg"))
*img*

```

In this project `imshow` use `glfw3` backend by default, but you can change it by provid the `backend` parameter (Note that you cannot only provide the `backend` parameter without providing the `title` parameter.) or set the special variable `dip:*default-backend*`.
For example, to use `sdl2` backend you can either do

``` common-lisp
CL-USER> (dip:imshow *img* "lena" :sdl2)
```
or

``` common-lisp
CL-USER> (setf dip:*default-backend* :sdl2)
CL-USER> (dip:imshow *img* "lena")
```
to achive it.
``` common-lisp
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
## Declaration
This project is actively updating, any part of this project may change. Becareful to use it in your project, no supportion is provided.

If you seek a much more mature image processing library, have a look at [opticl](https://github.com/slyrus/opticl).
They did a great job, this project also inspired and borrowed much from it.

But this project has some advantiges that opticl has not. For example opticl itself does not has any diaplay tools that can help you
show your image on the screen, you need to write it to file and use an image viewer to check it. But this project contains various
backends to display your image instantly, which make it more ease to use.

Issues are welcome, but PRs are too early to receive.

## Progress
For more information you can look at [TODO list](TODO.md).

## Troubleshooting
- Emacs's hints in mini-buffer disappear after used imshow-live
  - Look at http://techsnuffle.com/2018/04/17/slimeenableconcurrenthints.
