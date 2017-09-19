program HelloTriangle;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello Triangle                                        }
{                                                                              }
{ A rotating 3D cube in OpenGL ES with an image rendered on each face.         }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ Make sure you also copy the Djenne_128_128.raw, Gaudi_128_128.raw and        }
{ Lucca_128_128.raw files from the Media folder.                               }
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
{$linklib hello_triangle}
 
{Import the main function of the example so we can call it from Ultibo}
procedure hello_triangle; cdecl; external 'hello_triangle' name 'hello_triangle';
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello Triangle');

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
 hello_triangle;
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello Triangle');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

