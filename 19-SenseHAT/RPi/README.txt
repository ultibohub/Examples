Example 19 SenseHAT

Reading the joystick and displaying text on the 8x8 LED matrix of the Raspberry Pi SenseHAT.

Note: If the device tree files (*.dtb) are present on your SD card then you must ensure the overlays
      folder is also present and add the following line to the config.txt file in the root directory:

dtoverlay=rpi-sense

      or alternatively disable device tree loading by adding the following line to the config.txt file:

device_tree=

Raspberry Pi A/B/A+/B+/Zero/ZeroW version
