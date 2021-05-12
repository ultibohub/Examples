VideoCore IV example - RaspiVid

Using the RaspiVid application from within Ultibo to capture video images.

This example directly imports the C code from the original raspivid application.

Note: You must copy the additional firmware files start4x.elf and fixup4x.dat to your SD card and include the following settings in a config.txt file in the root directory for this example:

start_x=1
gpu_mem=128

Raspberry Pi 4B/400 version
