program RaspiVid;

{$mode objfpc}{$H+}

{ VideoCore IV example - RaspiVid                                              }
{                                                                              }
{ Using the RaspiVid application from within Ultibo to capture video images.   }
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
{ This version is for Raspberry Pi A/B/A+/B+/Zero                              }

uses
  RaspberryPi, {Include RaspberryPi to make sure all standard functions are included}
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
{$linklib raspivid}
 
{Import the main function of the example so we can call it from Ultibo}
function vcraspivid(argc: int; argv: PPChar): int; cdecl; external 'raspivid' name 'vcraspivid';
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting RaspiVid example');

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
  we have recompiled the RaspiVid application into a library that can be included in an
  Ultibo program and called exactly the same way that you would call the original.
  
  All functions are controlled by command line parameters, the most basic command is shown 
  below but the official documentation at
  
   https://www.raspberrypi.org/documentation/usage/camera/raspicam/raspivid.md
  
  lists all of the available options} 
 
 argv:=AllocateCommandLine('--timeout 10000 --width 640 --height 480 --framerate 25 --bitrate 1200000 --preview 100,100,640,480 --output C:\camera.h264',argc);
 
 {Call the main function of raspivid, it will return here when completed}
 vcraspivid(argc, argv);

 {Release the C command line} 
 ReleaseCommandLine(argv);
 
 ConsoleWindowWriteLn(WindowHandle,'Completed RaspiVid example');
 ConsoleWindowWriteLn(WindowHandle,'The file camera.h264 will be on your SD card');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

