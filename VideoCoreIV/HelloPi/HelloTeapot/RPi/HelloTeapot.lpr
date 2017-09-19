program HelloTeapot;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello Teapot                                          }
{                                                                              }
{ Using OpenGL ES and MMAL to show what the Raspberry Pi can do, if you haven't}
{ seen it before you should be impressed.                                      }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ Make sure you also copy the teapot.obj.dat and test.h264 files from the Media}
{ folder.                                                                      }
{                                                                              }
{ You also MUST create a config.txt file in the root directory of your SD card }
{ with at least the following setting:                                         }
{                                                                              }
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
  Syscalls,     {Include the Syscalls unit to provide C library support}
  VC4;          {Include the VC4 unit to enable access to the GPU}

var
 WindowHandle:TWindowHandle;

{Link our C library to include the original example} 
{$linklib hello_teapot}
 
{Import the main function of the example so we can call it from Ultibo}
procedure hello_teapot; cdecl; external 'hello_teapot' name 'hello_teapot';
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello Teapot');

 {Wait a couple of seconds for C:\ drive to be ready}
 ConsoleWindowWriteLn(WindowHandle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;
 ConsoleWindowWriteLn(WindowHandle,'C:\ drive is ready');
 ConsoleWindowWriteLn(WindowHandle,'');
 
 {Call the main function of the example, it will return here when completed (if ever)}
 hello_teapot;
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello Teapot');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

