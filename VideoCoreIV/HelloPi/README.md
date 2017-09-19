## Ultibo HelloPi examples

The examples in this folder are from the hello_pi collection that is included with Raspbian, the ones provided here have been ported to run under Ultibo which normally only requires minor adjustments for paths etc. Not all of the hello_pi examples have been ported, some do not work yet or simply don't provide any additional information that is not covered already.

The primary purpose of these examples is to prove that all functions of the VideoCore IV are available for use in Ultibo applications. These examples don't directly show how to use those functions (although the C code does provide a lot of information) however there are further examples that begin to demonstrate using the various features from an Ultibo application.

These examples are included in the Ultibo core installer download and can be found from the Lazarus IDE by selecting Tools, Example Projects.. from the menu.

### HelloPi examples:

* HelloAudio - A very simple example showing audio output over HDMI.

* HelloDispmanX - The basics of creating a DispmanX window and displaying graphics on top of the existing framebuffer.

* HelloEncode - Encoding a test pattern of vertical stripes into multiple different image formats (PNG, JPEG, GIF and Bitmap).

* HelloJPEG - Decoding a JPEG using the hardware JPEG decoder (note that this example is a decoder only and does not display the picture).

* HelloMMALEncode - Using the Multimedia Abstration Layer (MMAL) to encode a small piece of generated video.

* HelloTeapot - Using OpenGL ES and MMAL to show what the Raspberry Pi can do, if you haven't seen it before you should be impressed.

* HelloTiger - A basic OpenVG example showing a rotating picture of a tiger, this example uses a picture embedded in the code.

* HelloTriangle - A rotating 3D cube in OpenGL ES with an image rendered on each face.

* HelloTriangle2 - Using an OpenGL ES 2 shader to draw a mandelbrot set.

* HelloVideo - Playing a small piece of h264 video using the OpenMAX IL libraries.

* HelloVideocube - Combining the 3D cube from hello triangle with video from the previous example, very nice demonstration.

* Media - This folder contains the media files (image, video and 3D model) used by the Hello Pi examples, please copy these files to your SD card.