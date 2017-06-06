(in-package :cl-soil)

(docs:define-docs
  (defun load-ogl-texture
      "
Loads an image from disk into an OpenGL texture. Returns the texture-id

filename: the name of the file to upload as a texture

force-channels: One of the following:

    Name     Effect
    -------------------------------------------
    :auto    leaves the image in whatever format it was found
    :l       forces the image to load as Luminous (greyscale)
    :la      forces the image to load as Luminous with Alpha
    :rgb     forces the image to load as Red Green Blue
    :rgba    forces the image to load as Red Green Blue Alpha

reuse-texture-id: The gl texture id to reuse, or 0 to generate a new texture.

flags: One or a list of the following

    Name               Effect
    --------------------------------------------
    :power-of-two      force the image to be POT
    :mipmaps           generate mipmaps for the texture
    :texture-repeats   Sampling set to repeat (otherwise will clamp)
    :multiply-alpha    for using (:one, :one_minus_src_alpha) blending
    :invert-y          flip the image vertically
    :compress-to-dxt   if gpu support it, converts RGB to DXT1, RGBA to DXT5
    :dds-load-direct   will load DDS files directly without _ANY_ additional
                       processing.
")

  (defun load-ogl-texture-from-memory
      "
Loads an image from RAM into an OpenGL texture. Returns the texture-id

data-pointer: pointer to the image data

data-length: length of the data in bytes

force-channels: One of the following:

    Name     Effect
    -------------------------------------------
    :auto    leaves the image in whatever format it was found
    :l       forces the image to load as Luminous (greyscale)
    :la      forces the image to load as Luminous with Alpha
    :rgb     forces the image to load as Red Green Blue
    :rgba    forces the image to load as Red Green Blue Alpha

reuse-texture-id: The gl texture id to reuse, or 0 to generate a new texture.

flags: One or a list of the following

    Name               Effect
    --------------------------------------------
    :power-of-two      force the image to be POT
    :mipmaps           generate mipmaps for the texture
    :texture-repeats   Sampling set to repeat (otherwise will clamp)
    :multiply-alpha    for using (:one, :one_minus_src_alpha) blending
    :invert-y          flip the image vertically
    :compress-to-dxt   if gpu support it, converts RGB to DXT1, RGBA to DXT5
    :dds-load-direct   will load DDS files directly without _ANY_ additional
                       processing.
")

  (defun load-ogl-cubemap
      "
Loads 6 images from disk into an OpenGL cubemap texture. Returns the texture-id

x-pos-filepath: the name of the file to upload as a texture face

x-neg-filepath: the name of the file to upload as a texture face

y-pos-filepath: the name of the file to upload as a texture face

y-neg-filepath: the name of the file to upload as a texture face

z-pos-filepath: the name of the file to upload as a texture face

z-neg-filepath: the name of the file to upload as a texture face

force-channels: One of the following:

    Name     Effect
    -------------------------------------------
    :auto    leaves the image in whatever format it was found
    :l       forces the image to load as Luminous (greyscale)
    :la      forces the image to load as Luminous with Alpha
    :rgb     forces the image to load as Red Green Blue
    :rgba    forces the image to load as Red Green Blue Alpha

reuse-texture-id: The gl texture id to reuse, or 0 to generate a new texture.

flags: One or a list of the following

    Name               Effect
    --------------------------------------------
    :power-of-two      force the image to be POT
    :mipmaps           generate mipmaps for the texture
    :texture-repeats   Sampling set to repeat (otherwise will clamp)
    :multiply-alpha    for using (:one, :one_minus_src_alpha) blending
    :invert-y          flip the image vertically
    :compress-to-dxt   if gpu support it, converts RGB to DXT1, RGBA to DXT5
    :dds-load-direct   will load DDS files directly without _ANY_ additional
                       processing.
")

  (defun load-ogl-cubemap-from-memory
      "
Loads 6 images from memory into an OpenGL cubemap texture.
Returns the texture-id

x-pos-filepath: the name of the file to upload as a texture face

x-pos-length: length of the data in bytes

x-neg-filepath: the name of the file to upload as a texture face

x-neg-length: length of the data in bytes

y-pos-filepath: the name of the file to upload as a texture face

y-pos-length: length of the data in bytes

y-neg-filepath: the name of the file to upload as a texture face

y-neg-length: length of the data in bytes

z-pos-filepath: the name of the file to upload as a texture face

z-pos-length: length of the data in bytes

z-neg-filepath: the name of the file to upload as a texture face

z-neg-length: length of the data in bytes

force-channels: One of the following:

    Name     Effect
    -------------------------------------------
    :auto    leaves the image in whatever format it was found
    :l       forces the image to load as Luminous (greyscale)
    :la      forces the image to load as Luminous with Alpha
    :rgb     forces the image to load as Red Green Blue
    :rgba    forces the image to load as Red Green Blue Alpha

reuse-texture-id: The gl texture id to reuse, or 0 to generate a new texture.

flags: One or a list of the following

    Name               Effect
    --------------------------------------------
    :power-of-two      force the image to be POT
    :mipmaps           generate mipmaps for the texture
    :texture-repeats   Sampling set to repeat (otherwise will clamp)
    :multiply-alpha    for using (:one, :one_minus_src_alpha) blending
    :invert-y          flip the image vertically
    :compress-to-dxt   if gpu support it, converts RGB to DXT1, RGBA to DXT5
    :dds-load-direct   will load DDS files directly without _ANY_ additional
                       processing.
")

  (defun create-ogl-texture
      "
Creates a 2D OpenGL texture from raw image data. Returns the texture-id

data-pointer: pointer to the image data

width: width of the image in pixels

height: height of the image in pixels

channels: the number of channels:

    Count   Meaning
    ----------------------
    1       luminous
    2       luminous/alpha
    3       RGB
    4       RGBA

reuse-texture-id: The gl texture id to reuse, or 0 to generate a new texture.

flags: One or a list of the following

    Name               Effect
    --------------------------------------------
    :power-of-two      force the image to be POT
    :mipmaps           generate mipmaps for the texture
    :texture-repeats   Sampling set to repeat (otherwise will clamp)
    :multiply-alpha    for using (:one, :one_minus_src_alpha) blending
    :invert-y          flip the image vertically
    :compress-to-dxt   if gpu support it, converts RGB to DXT1, RGBA to DXT5
    :dds-load-direct   will load DDS files directly without _ANY_ additional
                       processing.
")

  (defun load-ogl-hdr-texture
      "
Loads an HDR image from disk into an OpenGL texture. Returns the texture-id

filename: the name of the file to upload as a texture

fake-hdr-format: Which fake HDR representation to use:

    Name         Meaning
    ------------------------------------------
    :rgbe        (* RGB (expt 2.0 (- A 128.0))
    :rgb-div-a   (/ RGB A)
    :rgb-div-a2  (/ RGB (* A A))


reuse-texture-id: The gl texture id to reuse, or 0 to generate a new texture.

rescale-to-max: integer. <Not documented in original>

flags: One or a list of the following

    Name               Effect
    --------------------------------------------
    :power-of-two      force the image to be POT
    :mipmaps           generate mipmaps for the texture
    :texture-repeats   Sampling set to repeat (otherwise will clamp)
    :multiply-alpha    for using (:one, :one_minus_src_alpha) blending
    :invert-y          flip the image vertically
    :compress-to-dxt   if gpu support it, converts RGB to DXT1, RGBA to DXT5
    :dds-load-direct   will load DDS files directly without _ANY_ additional
                       processing.

*Note:* If you want a true HDR texture try using `stb-image:loadf` to load an
        image as floating point data. The use GL to create an upload a
        float-texture.
")

  (defun load-ogl-single-cubemap
      "
Loads 1 image from disk and splits it into an OpenGL cubemap texture.
Returns the texture-id

filename: the name of the file to upload as a texture

face-order: the order of the faces in the file. Any combination of NSWEUD,
for North, South, Up, etc as a keyword. For example :UDNSWE

force-channels: One of the following:

    Name     Effect
    -------------------------------------------
    :auto    leaves the image in whatever format it was found
    :l       forces the image to load as Luminous (greyscale)
    :la      forces the image to load as Luminous with Alpha
    :rgb     forces the image to load as Red Green Blue
    :rgba    forces the image to load as Red Green Blue Alpha

reuse-texture-id: The gl texture id to reuse, or 0 to generate a new texture.

flags: One or a list of the following

    Name               Effect
    --------------------------------------------
    :power-of-two      force the image to be POT
    :mipmaps           generate mipmaps for the texture
    :texture-repeats   Sampling set to repeat (otherwise will clamp)
    :multiply-alpha    for using (:one, :one_minus_src_alpha) blending
    :invert-y          flip the image vertically
    :compress-to-dxt   if gpu support it, converts RGB to DXT1, RGBA to DXT5
    :dds-load-direct   will load DDS files directly without _ANY_ additional
                       processing.
")

  (defun load-ogl-single-cubemap-from-memory
      "
Loads 1 image from RAM and splits it into an OpenGL cubemap texture.
Returns the texture-id

data-pointer: pointer to the image data

data-length: length of the data in bytes

face-order: the order of the faces in the file. Any combination of NSWEUD,
for North, South, Up, etc as a keyword. For example :UDNSWE

force-channels: One of the following:

    Name     Effect
    -------------------------------------------
    :auto    leaves the image in whatever format it was found
    :l       forces the image to load as Luminous (greyscale)
    :la      forces the image to load as Luminous with Alpha
    :rgb     forces the image to load as Red Green Blue
    :rgba    forces the image to load as Red Green Blue Alpha

reuse-texture-id: The gl texture id to reuse, or 0 to generate a new texture.

flags: One or a list of the following

    Name               Effect
    --------------------------------------------
    :power-of-two      force the image to be POT
    :mipmaps           generate mipmaps for the texture
    :texture-repeats   Sampling set to repeat (otherwise will clamp)
    :multiply-alpha    for using (:one, :one_minus_src_alpha) blending
    :invert-y          flip the image vertically
    :compress-to-dxt   if gpu support it, converts RGB to DXT1, RGBA to DXT5
    :dds-load-direct   will load DDS files directly without _ANY_ additional
                       processing.
")

  (defun create-ogl-single-cubemap
      "
Creates an OpenGL cubemap texture by splitting up 1 image into 6 parts.
Returns the texture-id

data-pointer: pointer to the image data

width: width of the image in pixels

height: height of the image in pixels

channels: the number of channels:

    Count   Meaning
    ----------------------
    1       luminous
    2       luminous/alpha
    3       RGB
    4       RGBA

face-order: the order of the faces in the file. Any combination of NSWEUD,
for North, South, Up, etc as a keyword. For example :UDNSWE

reuse-texture-id: The gl texture id to reuse, or 0 to generate a new texture.

flags: One or a list of the following

    Name               Effect
    --------------------------------------------
    :power-of-two      force the image to be POT
    :mipmaps           generate mipmaps for the texture
    :texture-repeats   Sampling set to repeat (otherwise will clamp)
    :multiply-alpha    for using (:one, :one_minus_src_alpha) blending
    :invert-y          flip the image vertically
    :compress-to-dxt   if gpu support it, converts RGB to DXT1, RGBA to DXT5
    :dds-load-direct   will load DDS files directly without _ANY_ additional
                       processing.
")

  (defun load-image
      "
Loads an image from disk. Returns the following:

- A pointer to the image data
- the width of the image
- the height of the image
- the component-count of the data as stored in the file
- the component-count of the returned image data

Where the component-count is one of the following:

    Count   Meaning
    ----------------------
    1       luminous
    2       luminous/alpha
    3       RGB
    4       RGBA


filename: the name of the file to load

force-channels: One of the following:

    Name     Effect
    -------------------------------------------
    :auto    leaves the image in whatever format it was found
    :l       forces the image to load as Luminous (greyscale)
    :la      forces the image to load as Luminous with Alpha
    :rgb     forces the image to load as Red Green Blue
    :rgba    forces the image to load as Red Green Blue Alpha
")

  (defun load-image-from-memory
      "
Loads an image from memory. Returns the following:

- A pointer to the image data
- the width of the image in pixels
- the height of the image in pixels
- the component-count of the data as stored in the file
- the component-count of the returned image data

Where the component-count is one of the following:

    Count   Meaning
    ----------------------
    1       luminous
    2       luminous/alpha
    3       RGB
    4       RGBA

data-pointer: pointer to the image data

data-length: length of the data in bytes

force-channels: One of the following:

    Name     Effect
    -------------------------------------------
    :auto    leaves the image in whatever format it was found
    :l       forces the image to load as Luminous (greyscale)
    :la      forces the image to load as Luminous with Alpha
    :rgb     forces the image to load as Red Green Blue
    :rgba    forces the image to load as Red Green Blue Alpha
")

  (defun save-image
      "
Saves an image from memory. Returns the filepath on success

filename: the name of the file to save to

image-type: One of the following formats

    :tga
    :bmp
    :dds

width: the width of the image in pixels

height: the height of the image in pixels

channels: the number of channels. Where the integer is one of the following

    Count   Meaning
    ----------------------
    1       luminous
    2       luminous/alpha
    3       RGB
    4       RGBA

data: cffi pointer to the image data
")

  (defun save-screenshot
      "
Captures the OpenGL window (RGB) and saves it to disk.
Returns the filepath on success

filename: the name of the file to save to

image-type: One of the following formats

    :tga
    :bmp
    :dds

x: x component of origin in pixels

x: y component of origin in pixels

width: the width of the image in pixels

height: the height of the image in pixels
")

  (defun free-image-data
      "
Frees the image data.

This is really just cffi:foreign-free but exists as a reminder to free your
data.
")

  (defun last-result
      "
This function resturn a string describing the last thing that happened
inside SOIL.  It can be used to determine why an image failed to load.
"))
