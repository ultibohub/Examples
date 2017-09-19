VideoCore IV example - RaspiStill

An example showing how to control the RaspiStill application to take still images from the camera.

This example directly imports the C code from the original raspistill application.

Note: You must copy the additional firmware files start_x.elf and fixup_x.dat to your SD card and include the following settings in a config.txt file in the root directory for this example:

start_x=1
gpu_mem=128

Raspberry Pi A/B/A+/B+/Zero version
