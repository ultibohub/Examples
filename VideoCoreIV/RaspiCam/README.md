## Ultibo RaspiCam examples

The official Raspberry Pi camera uses the MMAL (Multimedia Abstration Layer) API to capture and encode images, provide previews and perform many other complex transformation functions.

As yet the MMAL API headers have not been ported to Free Pascal so in order to allow access to the camera we have recompiled the RaspiStill and RaspiVid applications as libraries that can be included in an Ultibo application. 

The examples in this folder show the basic usage of these libraries as a temporary solution until more sophisticated and direct access is available.

These examples are included in the Ultibo core installer download and can be found from the Lazarus IDE by selecting Tools, Example Projects.. from the menu.

### RaspiCam examples:

* RaspiStill - An example showing how to control the RaspiStill application to take still images from the camera.

* RaspiVid - Using the RaspiVid application from within Ultibo to capture video images.
