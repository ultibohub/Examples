program HelloVideocube;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello Videocube                                       }
{                                                                              }
{ Combining the 3D cube from hello triangle with video from the previous       }
{ example, very nice demonstration.                                            }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ Make sure you also copy the test.h264 file from the Media folder.            }
{                                                                              }
{ You also MUST create a config.txt file in the root directory of your SD card }
{ with at least the following setting:                                         }
{                                                                              }
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
  Syscalls,     {Include the Syscalls unit to provide C library support}
  VC4;          {Include the VC4 unit to enable access to the GPU}

var
 WindowHandle:TWindowHandle;

{Link our C library to include the original example} 
{$linklib hello_videocube}
 
{Import the main function of the example so we can call it from Ultibo}
procedure hello_videocube; cdecl; external 'hello_videocube' name 'hello_videocube';
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello Videocube');

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
 hello_videocube;
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello Videocube');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

