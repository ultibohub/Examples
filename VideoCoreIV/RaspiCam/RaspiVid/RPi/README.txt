VideoCore IV example - RaspiVid

Using the RaspiVid application from within Ultibo to capture video images.

This example directly imports the C code from the original raspivid application.

Note: You must copy the additional firmware files start_x.elf and fixup_x.dat to your SD card and include the following settings in a config.txt file in the root directory for this example:

start_x=1
gpu_mem=128

Raspberry Pi A/B/A+/B+/Zero version
