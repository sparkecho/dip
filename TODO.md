# TODO

## Modularization
- [x] Move `imread` and `imwrite` to `io.lisp`
- [x] Move `imshow` to `gui.lisp`
- [x] Move image creation related code to `image.lisp`
- [x] Reserve image processing related code in `dip.lisp`
- [x] Separate image format related io functions from `io.lisp` to other files
## Optimization
- [ ] Implement specific functions to each type of image
- [ ] Use declarations to optimize
- [ ] Use macros to optimize
- [ ] Add foreign allocated image class
## Image format supportion
- [x] JPG (JPEG)
- [x] PNG
- [ ] BMP
- [ ] GIF
- [ ] PDF
- [ ] TIF (TIFF)
- [ ] SVG
## `imshow` backends
- [x] GLFW3
- [x] SDL2
- [x] CLX
- [ ] win32-api
- [ ] Improves efficience of `imshow` (Each call of imshow creates a new context now)
  - [ ] GLFW3
  - [x] SDL2
  - [ ] CLX
  - [ ] win32-api
## Video io supportion
- [ ] avi
- [ ] mp4
- [ ] camera
  - [ ] v4l2
## Features to add
- [ ] Live coding supportion (special version of `imshow`)
  - [ ] GLFW3
  - [x] SDL2
  - [ ] CLX
  - [ ] win32-api
## Add unit test
## Add examples
