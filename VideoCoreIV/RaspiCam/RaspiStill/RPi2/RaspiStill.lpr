program RaspiStill;

{$mode objfpc}{$H+}

{ VideoCore IV example - RaspiStill                                            }
{                                                                              }
{ An example showing how to control the RaspiStill application to take still   }
{ images from the camera.                                                      }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ You will need to have an official Raspberry Pi camera (either V1 or V2)      }
{ connected in order to try this example.                                      }
{                                                                              }
{ You also MUST copy the start_x.elf and fixup_x.dat to your SD card AND create}
{ a config.txt file in the root directory with the following settings:         }
{                                                                              }
{ start_x=1                                                                    }
{ gpu_mem=128                                                                  }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B.              }

uses
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  SysUtils,
  UltiboUtils,  {Include Ultibo utils for some command line manipulation}
  Syscalls,     {Include the Syscalls unit to provide C library support}
  VC4;          {Include the VC4 unit to enable access to the GPU}

var
 argc:int;      {Some command line arguments to pass to the C code}
 argv:PPChar;
 
 WindowHandle:TWindowHandle;

{Link our C library to include the original example} 
{$linklib raspistill}
 
{Import the main function of the example so we can call it from Ultibo}
function vcraspistill(argc: int; argv: PPChar): int; cdecl; external 'raspistill' name 'vcraspistill';
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting RaspiStill example');

 {Wait a couple of seconds for C:\ drive to be ready}
 ConsoleWindowWriteLn(WindowHandle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;
 ConsoleWindowWriteLn(WindowHandle,'C:\ drive is ready');
 ConsoleWindowWriteLn(WindowHandle,'');
 
 {We need to call the MMALIncludeComponentVideocore function in order to force Free Pascal
  and the linker to include the relevant libraries that provide the VideoCore components
  to MMAL. 
  
  If you look for this function in the VC4 unit you will find a whole selection of components
  that can be included, please see the (very limited) MMAL documentation for more information}
 MMALIncludeComponentVideocore;
 
 {Allocate a command line for the C code, this function just takes a string and creates a
  properly formatted argv and argc which can be used to pass parameters to the example}
  
 {Because the Raspberry Pi camera uses the MMAL (Multimedia Abstration Layer) API to capture
  and encode images and there is currently no Free Pascal translation of the MMAL headers 
  we have recompiled the RaspiStill application into a library that can be included in an
  Ultibo program and called exactly the same way that you would call the original.
  
  All functions are controlled by command line parameters, the most basic command is shown 
  below but the official documentation at
  
   https://www.raspberrypi.org/documentation/usage/camera/raspicam/raspistill.md
  
  lists all of the available options} 
 
 argv:=AllocateCommandLine('--output C:\camera.jpg',argc);
 
 {Call the main function of raspistill, it will return here when completed}
 vcraspistill(argc, argv);

 {Release the C command line} 
 ReleaseCommandLine(argv);
 
 ConsoleWindowWriteLn(WindowHandle,'Completed RaspiStill example');
 ConsoleWindowWriteLn(WindowHandle,'The file camera.jpg will be on your SD card');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

