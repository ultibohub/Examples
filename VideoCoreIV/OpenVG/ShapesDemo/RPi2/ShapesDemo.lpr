program ShapesDemo;

{$mode objfpc}{$H+}

{ VideoCore IV example - Shapes Demo                                           }
{                                                                              }
{ A demonstration of many of the available features of VGShapes, ported from   }
{ the refcard example in the original library.                                 }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ Make sure you also copy the Ultibo.jpg file to your SD card.                 }
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
  OpenVG,       {Include the OpenVG unit so we can use the various types and structures}
  VGShapes,     {Include the VGShapes unit to give us access to all the functions}
  VC4;          {Include the VC4 unit to enable access to the GPU}

var
 Width:Integer;  {A few variables used by our shapes example}
 Height:Integer;
 
 Top:VGfloat;
 ShapeX:VGfloat;
 ShapeY:VGfloat;
 ShapeW:VGfloat;
 ShapeH:VGfloat;
 Dotsize:VGfloat;
 Spacing:VGfloat;
 
 CX:VGfloat;
 CY:VGfloat;
 
 EX:VGfloat;
 EY:VGfloat;
 
 PolyX:array[0..4] of VGfloat;
 PolyY:array[0..4] of VGfloat;
 
 Count:Integer;
 NumShapes:Integer;
 Fontsize:Integer;
 
 Shapecolor:TVGShapesColor;
 
 WindowHandle:TWindowHandle;
 
const
 {The names of all the available shapes}
 Shapenames:array[0..10] of String = (
  'Circle',
  'Ellipse',
  'Rectangle',
  'Rounded Rectangle',
  'Line',
  'Polyline',
  'Polygon',
  'Arc',
  'Quadratic Bezier',
  'Cubic Bezier',
  'Image');
 
procedure VGCoordPoint(X,Y,Size:VGfloat;const Color:TVGShapesColor);
begin
 {}
 {A utility function used by the main program code to place a colored point in the picture}
 VGShapesFill(128,0,0,0.3);
 VGShapesCircle(X,Y,Size);
 VGShapesSetFill(Color);
end;
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting VGShapes Demo');

 {Wait a couple of seconds for C:\ drive to be ready}
 ConsoleWindowWriteLn(WindowHandle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;
 ConsoleWindowWriteLn(WindowHandle,'C:\ drive is ready');
 ConsoleWindowWriteLn(WindowHandle,'');
 
 {We have ported the libShapes library created by A J Starks to Ultibo so you can easily use
  all of the functionality it provides. You can find out more about libShapes and what it can
  do by visiting the GitHub repository at:
  
  https://github.com/ajstarks/openvg
  
  To get you started this example and the previous one show some of the basics}
  
 {Initialize OpenVG and the VGShapes unit}
 VGShapesInit(Width,Height); 
 
 {Convert the RGB color to use for our shapes into a TVGShapesColor record}
 VGShapesRGB(202,225,255,Shapecolor);
 
 {Calculate some default values based on the size of the screen, remember that
  OpenVG coordinates put 0,0 at the bottom left NOT the top left like most other
  things}
 Top:=Height * 0.93;
 ShapeX:=500;
 ShapeY:=Top;
 ShapeW:=Width * 0.05;
 ShapeH:=Height * 0.045;
 Dotsize:=7;
 Spacing:=1.9;
 NumShapes:=High(Shapenames);
 Fontsize:=Trunc(Height * 0.033);
 
 {Start a picture the full width and height of the screen}
 VGShapesStart(Width,Height);
 
 {Calculate the left hand edge of our shapes}
 ShapeX:=Width * 0.10;
 
 {Draw the title of our demo towards the right hand edge}
 VGShapesFill(128,0,0,1);
 VGShapesTextEnd(Width - 100,Height / 2,'OpenVG on Ultibo',VGShapesSansTypeface,Fontsize + (Fontsize div 2));
 
 {Draw labels for the name of each shape first}
 VGShapesFill(0,0,0,1);
 for Count:=0 to NumShapes do
  begin
   VGShapesText(ShapeX + ShapeW + ShapeW / 2,ShapeY,Shapenames[Count],VGShapesSansTypeface,Fontsize);
   ShapeY:=ShapeY - ShapeH * Spacing;
  end;
  
 {Return to the starting point for our shapes (Remember 0 is the bottom NOT the top)} 
 ShapeY:=Top;
 
 {Calculate and draw each of the shapes in the demo}
 
 {Circle} 
 CX:=ShapeX + (ShapeW / 2);
 EX:=ShapeX + ShapeW; 
 VGShapesSetFill(Shapecolor);

 VGShapesCircle(CX,ShapeY,ShapeW);
 VGCoordPoint(CX,ShapeY,Dotsize,Shapecolor);
 ShapeY:=ShapeY - ShapeH * Spacing;
 
 {Ellipse}
 VGShapesEllipse(CX,ShapeY,ShapeW,ShapeH);
 VGCoordPoint(CX,ShapeY,Dotsize,Shapecolor);
 ShapeY:=ShapeY - ShapeH * Spacing;
 
 {Rectangle}
 VGShapesRect(ShapeX,ShapeY,ShapeW,ShapeH);
 VGCoordPoint(ShapeX,ShapeY,Dotsize,Shapecolor);
 ShapeY:=ShapeY - ShapeH * Spacing;
 
 {Rounded Rectangle}
 VGShapesRoundrect(ShapeX,ShapeY,ShapeW,ShapeH,20,20);
 VGCoordPoint(ShapeX,ShapeY,Dotsize,Shapecolor);
 ShapeY:=ShapeY - ShapeH * Spacing;
 
 {Line (Diagonal)}
 VGShapesStrokeWidth(1);
 VGShapesStroke(204,204,204,1);
 VGShapesLine(ShapeX,ShapeY,EX,ShapeY + ShapeH);
 VGCoordPoint(ShapeX,ShapeY,Dotsize,Shapecolor);
 VGCoordPoint(EX,ShapeY + ShapeH,Dotsize,Shapecolor);
 ShapeY:=ShapeY - ShapeH;
 
 {Polyline}
 PolyX[0]:=ShapeX;
 PolyX[1]:=ShapeX + (ShapeW / 4);
 PolyX[2]:=ShapeX + (ShapeW / 2);
 PolyX[3]:=ShapeX + ((ShapeW * 3) / 4);
 PolyX[4]:=ShapeX + ShapeW;

 PolyY[0]:=ShapeY;
 PolyY[1]:=ShapeY - ShapeH;
 PolyY[2]:=ShapeY;
 PolyY[3]:=ShapeY - ShapeH;
 PolyY[4]:=ShapeY;
 
 VGShapesPolyline(@PolyX,@PolyY,5);
 VGCoordPoint(PolyX[0],PolyY[0],Dotsize,Shapecolor);
 VGCoordPoint(PolyX[1],PolyY[1],Dotsize,Shapecolor);
 VGCoordPoint(PolyX[2],PolyY[2],Dotsize,Shapecolor);
 VGCoordPoint(PolyX[3],PolyY[3],Dotsize,Shapecolor);
 VGCoordPoint(PolyX[4],PolyY[4],Dotsize,Shapecolor);
 ShapeY:=ShapeY - ShapeH * Spacing;
 
 {Polygon}
 PolyY[0]:=ShapeY;
 PolyY[1]:=ShapeY - ShapeH;
 PolyY[2]:=ShapeY - (ShapeH / 2);
 PolyY[3]:=PolyY[1] - (ShapeH / 4);
 PolyY[4]:=ShapeY;
 VGShapesPolygon(@PolyX,@PolyY,5);
 ShapeY:=ShapeY - ((ShapeH * Spacing) + ShapeH);
 
 {Arc}
 VGShapesArc(ShapeX + (ShapeW / 2),ShapeY,ShapeW,ShapeH,0,180);
 VGCoordPoint(ShapeX + (ShapeW / 2),ShapeY,Dotsize,Shapecolor);
 ShapeY:=ShapeY - ShapeH * Spacing;
 
 {Quadratic bezier}
 CY:=ShapeY + (ShapeH / 2);
 EY:=ShapeY;
 VGShapesQbezier(ShapeX,ShapeY,CX,CY,EX,EY);
 VGCoordPoint(ShapeX,ShapeY,Dotsize,Shapecolor);
 VGCoordPoint(CX,CY,Dotsize,Shapecolor);
 VGCoordPoint(EX,EY,Dotsize,Shapecolor);
 ShapeY:=ShapeY - ShapeH * Spacing;
 
 {Cubic bezier}
 EY:=ShapeY;
 CY:=ShapeY + ShapeH;
 VGShapesCbezier(ShapeX,ShapeY,CX,CY,CX,ShapeY,EX,EY);
 VGCoordPoint(ShapeX,ShapeY,Dotsize,Shapecolor);
 VGCoordPoint(CX,CY,Dotsize,Shapecolor);
 VGCoordPoint(CX,ShapeY,Dotsize,Shapecolor);
 VGCoordPoint(EX,EY,Dotsize,Shapecolor);
 ShapeY:=ShapeY - (ShapeH * Spacing * 1.5);

 {Image (JPEG only so far but others are easy to add)}
 VGShapesImage(ShapeX,ShapeY,90,90,'Ultibo.jpg');
 
 {End our picture and render it to the screen}
 VGShapesEnd;
 
 {Sleep for 10 seconds}
 Sleep(10000);
 
 {Clear our screen, cleanup OpenVG and deinitialize VGShapes}
 VGShapesFinish;
 
 {VGShapes calls BCMHostInit during initialization, we should also call BCMHostDeinit to cleanup}
 BCMHostDeinit;
 
 ConsoleWindowWriteLn(WindowHandle,'Completed VGShapes Demo');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

