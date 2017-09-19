program HelloDispmanX;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello DispmanX                                        }
{                                                                              }
{ The basics of creating a DispmanX window and displaying graphics on top of   }
{ the existing framebuffer.                                                    }
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
  Syscalls,     {Include the Syscalls unit to provide C library support}
  VC4;          {Include the VC4 unit to enable access to the GPU}

var
 WindowHandle:TWindowHandle;

{Link our C library to include the original example} 
{$linklib hello_dispmanx}
 
{Import the main function of the example so we can call it from Ultibo}
procedure hello_dispmanx; cdecl; external 'hello_dispmanx' name 'hello_dispmanx';
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello DispmanX');

 {Call the main function of the example, it will return here when completed (if ever)}
 hello_dispmanx;
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello DispmanX');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

