program HelloGLES2;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello GLES2                                           }
{                                                                              }
{ The spinning and rotating 3D cube done in OpenGL ES 2.0 instead, without     }
{ the texture images but you can see the differences.                          }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B/3B+/3A+.      }
{ To create a version for the A/B/A+/B+/Zero simply create a new project and   }
{ copy this code into it.                                                      }

uses
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  SysUtils,
  GLES2Unit,    {To keep things clearer all the code for this example is in the separate GLES2Unit}
  VC4;          {Include the VC4 unit to enable access to the GPU}

var
 WindowHandle:TWindowHandle;
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello GLES2');

 {Wait a couple of seconds for C:\ drive to be ready}
 ConsoleWindowWriteLn(WindowHandle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;
 ConsoleWindowWriteLn(WindowHandle,'C:\ drive is ready');
 ConsoleWindowWriteLn(WindowHandle,'');

 {The main purpose of this example is to demonstrate how to create an OpenGL ES 2.0 context
  and configure DispmanX, EGL and OpenGL ES so that you can perform OpenGL ES functions.
  
  All of the code to demonstrate this is included in the GLES2Unit which we simply call
  here using the StartGLES2 procedure.
  
  Open the GLES2Unit to see all the details of what it does}
 StartGLES2;
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello GLES2');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

