program HelloAudio;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello Audio                                           }
{                                                                              }
{ A very simple example showing audio output over HDMI                         }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B.              }

uses
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  UltiboUtils,  {Include Ultibo utils for some command line manipulation}
  Syscalls,     {Include the Syscalls unit to provide C library support}
  VC4;          {Include the VC4 unit to enable access to the GPU}

var
 argc:int;      {Some command line arguments to pass to the C code}
 argv:PPChar;
 
 WindowHandle:TWindowHandle;

{Link our C library to include the original example} 
{$linklib hello_audio}
 
{Import the main function of the example so we can call it from Ultibo}
function hello_audio(argc: int; argv: PPChar): int; cdecl; external 'hello_audio' name 'hello_audio';
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello Audio');

 {Allocate a command line for the C code, this function just takes a string and creates a
  properly formatted argv and argc which can be used to pass parameters to the example}
 argv:=AllocateCommandLine('1',argc);
 
 {Call the main function of the example, it will return here when completed (if ever)}
 hello_audio(argc, argv);

 {Release the C command line} 
 ReleaseCommandLine(argv);
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello Audio');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

