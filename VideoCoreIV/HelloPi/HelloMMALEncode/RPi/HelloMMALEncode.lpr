program HelloMMALEncode;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello MMAL Encode                                     }
{                                                                              }
{ Using the Multimedia Abstration Layer (MMAL) to encode a small piece of      }
{ generated video.                                                             }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
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
{$linklib hello_mmal_encode}
 
{Import the main function of the example so we can call it from Ultibo}
function hello_mmal_encode(argc: int; argv: PPChar): int; cdecl; external 'hello_mmal_encode' name 'hello_mmal_encode';
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello MMAL Encode');

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
 argv:=AllocateCommandLine('',argc);
 
 {Call the main function of the example, it will return here when completed (if ever)}
 hello_mmal_encode(argc, argv);

 {Release the C command line} 
 ReleaseCommandLine(argv);
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello MMAL Encode');
 ConsoleWindowWriteLn(WindowHandle,'The files out.bmp, out.gif, out.png and out.jpg will be on your SD card');
  
 {Halt the main thread here}
 ThreadHalt(0);
end.

