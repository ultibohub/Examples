## Ultibo OpenVG examples

As part of supporting the VideoCore IV we have included a Free Pascal port of libShapes by A J Starks, this unit is included in the FPC packages and can be used by including VGShapes in your program.

The examples in this folder demonstrate some of the functionality of the VGShapes unit which includes a range of common shapes as well as fonts, gradients, outlines and bezier curves.

These examples are included in the Ultibo core installer download and can be found from the Lazarus IDE by selecting Tools, Example Projects.. from the menu.

Note that these examples don't work on the Raspberry Pi 4B/400 as it uses a different GPU that is not compatible with the current libraries.

### OpenVG examples:

* HelloVG - A simple introduction to the world of OpenVG, kind of Hello World for 2D graphics.

* ShapesDemo - A demonstration of many of the available features of VGShapes, ported from the refcard example in the original library.

* MultipleLayers - An example showing the use of multiple screen layers by combining the Hello VG example with some shapes moving around the screen.