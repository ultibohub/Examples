program HelloVG;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello VG                                              }
{                                                                              }
{ A simple introduction to the world of OpenVG, kind of Hello World for 2D     }
{ graphics.                                                                    }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
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
  VGShapes,     {Include the VGShapes unit to give us access to all the functions}
  VC4;          {Include the VC4 unit to enable access to the GPU}

var
 Width:Integer;  {A couple of variables to store the width and height of the screen for our picture}
 Height:Integer;
 
 WindowHandle:TWindowHandle;
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello VG');

 {We have ported the libShapes library created by A J Starks to Ultibo so you can easily use
  all of the functionality it provides. You can find out more about libShapes and what it can
  do by visiting the GitHub repository at:
  
  https://github.com/ajstarks/openvg
  
  To get you started this example and the next one show some of the basics}
 
 {Initialize OpenVG and the VGShapes unit}
 VGShapesInit(Width,Height); 
 
 {Start a picture the full width and height of the screen}
 VGShapesStart(Width,Height);
 
 {Make the background black}
 VGShapesBackground(0,0,0);
 
 {Draw a big blue circle to represent the world}
 VGShapesFill(44,77,232,1);
 VGShapesCircle(Width / 2,0,Width);
 
 {Draw our Hello World text in white using the included Serif typeface}
 VGShapesFill(255,255,255,1);
 VGShapesTextMid(Width / 2,(Height * 0.7),'Hello OpenVG',VGShapesSerifTypeface,Width div 15);
 
 {End our picture and render it to the screen}
 VGShapesEnd;
 
 {Sleep for 10 seconds}
 Sleep(10000);
 
 {Clear our screen, cleanup OpenVG and deinitialize VGShapes}
 VGShapesFinish;
 
 {VGShapes calls BCMHostInit during initialization, we should also call BCMHostDeinit to cleanup}
 BCMHostDeinit;
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello VG');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

