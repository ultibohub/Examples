program HelloJPEG;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello JPEG                                            }
{                                                                              }
{ Decoding a JPEG using the hardware JPEG decoder (note that this example is a }
{ decoder only and does not display the picture).                              }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ Make sure you also copy the Beetroot.jpg file from the Media folder.         }
{                                                                              }
{ Remember that this example only demonstrates decoding a JPEG in hardware and }
{ will not display the picture on screen.                                      }
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
{$linklib hello_jpeg}
 
{Import the main function of the example so we can call it from Ultibo}
function hello_jpeg(argc: int; argv: PPChar): int; cdecl; external 'hello_jpeg' name 'hello_jpeg';
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello JPEG');

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
 argv:=AllocateCommandLine('C:\Beetroot.jpg',argc);
 
 {Call the main function of the example, it will return here when completed (if ever)}
 hello_jpeg(argc, argv);

 {Release the C command line} 
 ReleaseCommandLine(argv);
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello JPEG');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

