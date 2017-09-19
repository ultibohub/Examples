program HelloEncode;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello Encode                                          }
{                                                                              }
{ Encoding a test pattern of vertical stripes into multiple different image    }
{ formats (PNG, JPEG, GIF and Bitmap).                                         }
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
{$linklib hello_encode}
 
{Import the main function of the example so we can call it from Ultibo}
function hello_encode(argc: int; argv: PPChar): int; cdecl; external 'hello_encode' name 'hello_encode';
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello Encode');

 {Wait a couple of seconds for C:\ drive to be ready}
 ConsoleWindowWriteLn(WindowHandle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;
 ConsoleWindowWriteLn(WindowHandle,'C:\ drive is ready');
 ConsoleWindowWriteLn(WindowHandle,'');
 
 {Allocate a command line for the C code, this function just takes a string and creates a
  properly formatted argv and argc which can be used to pass parameters to the example}
 argv:=AllocateCommandLine('C:\hello_encode.h264',argc);
 
 {Call the main function of the example, it will return here when completed (if ever)}
 hello_encode(argc, argv);

 {Release the C command line} 
 ReleaseCommandLine(argv);
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello Encode');
 ConsoleWindowWriteLn(WindowHandle,'The file hello_encode.h264 will be on your SD card');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

