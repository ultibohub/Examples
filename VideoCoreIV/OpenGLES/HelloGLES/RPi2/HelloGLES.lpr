program HelloGLES;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello GLES                                            }
{                                                                              }
{ A direct port to Free Pascal of the hello triangle OpenGL ES 1.1 example,    }
{ except this one allows you to supply your own bitmap.                        }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ Make sure you also copy the Logo.bmp file to your SD card.                   }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B.              }
{ To create a version for the A/B/A+/B+/Zero simply create a new project and   }
{ copy this code into it.                                                      }

uses
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  SysUtils,
  GLESUnit,     {To keep things clearer all the code for this example is in the separate GLESUnit}
  VC4;          {Include the VC4 unit to enable access to the GPU}

var
 WindowHandle:TWindowHandle;
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello GLES');

 {Wait a couple of seconds for C:\ drive to be ready}
 ConsoleWindowWriteLn(WindowHandle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;
 ConsoleWindowWriteLn(WindowHandle,'C:\ drive is ready');
 ConsoleWindowWriteLn(WindowHandle,'');

 {The main purpose of this example is to demonstrate how to create an OpenGL ES 1.1 context
  and configure DispmanX, EGL and OpenGL ES so that you can perform OpenGL ES functions.
  
  All of the code to demonstrate this is included in the GLESUnit which we simply call
  here using the StartGLES procedure.
  
  Open the GLESUnit to see all the details of what it does}
 StartGLES;
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello GLES');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

