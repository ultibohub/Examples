program MultipleLayers;

{$mode objfpc}{$H+}

{ VideoCore IV example - Multiple Layers                                       }
{                                                                              }
{ An example showing how to create multiple OpenVG layers by combining the     }
{ Hello VG examples with some shapes moving around the screen.                 }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ You MUST create a config.txt file in the root directory with the following   }
{ settings to provide sufficient GPU memory for this example:                  }
{                                                                              }
{ gpu_mem=128                                                                  }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B/3B+/3A+/Zero2W}
{ To create a version for the A/B/A+/B+/Zero simply create a new project and   }
{ copy this code into it.                                                      }

uses
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  SysUtils,
  DispmanX,     {Include the DispmanX unit so we can use the alpha constants}
  OpenVG,       {Include the OpenVG unit so we can use the various types and structures}
  VGShapes,     {Include the VGShapes unit to give us access to all the functions}
  VC4;          {Include the VC4 unit to enable access to the GPU}

const
 TOP_OFFSET_X = 4;
 TOP_OFFSET_Y = 7;
 BOTTOM_OFFSET_X = 9;
 BOTTOM_OFFSET_Y = 3;
 
var
 Width:Integer;  {A few variables used by our example}
 Height:Integer;
 
 HelloLayer:LongInt;

 TopW:VGfloat;
 TopH:VGfloat;
 TopX:VGfloat;
 TopY:VGfloat;
 TopOffsetX:VGfloat;
 TopOffsetY:VGfloat;
 TopLayer:LongInt;
 TopColor:TVGShapesColor;
 TopOutline:TVGShapesColor;

 BottomW:VGfloat;
 BottomH:VGfloat;
 BottomX:VGfloat;
 BottomY:VGfloat;
 BottomOffsetX:VGfloat;
 BottomOffsetY:VGfloat;
 BottomLayer:LongInt;
 BottomColor:TVGShapesColor;
 BottomOutline:TVGShapesColor;
 
 WindowHandle:TWindowHandle;

procedure DrawTopLayer;
begin
 {Draw our Top layer}
 VGShapesSetLayer(TopLayer);

 {Start a picture the full width and height of the screen and make the background transparent}
 VGShapesStart(Width,Height,True);
 
 {Circle}
 VGShapesSetFill(TopColor);
 VGShapesCircle(TopX,TopY,TopW);
 VGShapesSetStroke(TopOutline);
 VGShapesStrokeWidth(4);
 VGShapesCircleOutline(TopX,TopY,TopW);
 
 {Update our X position}
 TopX:=TopX + TopOffsetX;
 
 {Check our X position}
 if TopOffsetX > 0 then
  begin
   {Moving Right}
   if TopX + (TopW / 2) >= Width then
    begin
     TopOffsetX:=-TOP_OFFSET_X;
     
     {Update our Y position}
     TopY:=TopY + TopOffsetY;
    end;
  end
 else
  begin
   {Moving Left}
   if TopX - (TopW / 2) <= 0 then
    begin
     TopOffsetX:=TOP_OFFSET_X;
     
     {Update our Y position}
     TopY:=TopY + TopOffsetY;
    end;
  end;
  
 {Check our Y position}
 if TopOffsetY > 0 then
  begin
   {Moving Up}
   if TopY + (TopH / 2) >= Height then
    begin
     TopOffsetY:=-TOP_OFFSET_Y;
    end; 
  end
 else
  begin
   {Moving Down}
   if TopY - (TopH / 2) <= 0 then
    begin
     TopOffsetY:=TOP_OFFSET_Y;
    end;
  end;
 
 {End our picture and render it to the screen}
 VGShapesEnd;
end;

procedure DrawBottomLayer;
begin
 {Draw our Bottom layer}
 VGShapesSetLayer(BottomLayer);

 {Start a picture the full width and height of the screen and make the background transparent}
 VGShapesStart(Width,Height,True);

 {Rounded Rectangle}
 VGShapesSetFill(BottomColor);
 VGShapesRoundrect(BottomX,BottomY,BottomW,BottomH,20,20);
 VGShapesSetStroke(BottomOutline);
 VGShapesStrokeWidth(4);
 VGShapesRoundrectOutline(BottomX,BottomY,BottomW,BottomH,20,20);

 {Update our Y position}
 BottomY:=BottomY + BottomOffsetY;

 {Check our Y position}
 if BottomOffsetY > 0 then
  begin
   {Moving Up}
   if BottomY + BottomH >= Height then
    begin
     BottomOffsetY:=-BOTTOM_OFFSET_Y;

     {Update our X position}
     BottomX:=BottomX + BottomOffsetX;
    end; 
  end
 else
  begin
   {Moving Down}
   if BottomY <= 0 then
    begin
     BottomOffsetY:=BOTTOM_OFFSET_Y;

     {Update our X position}
     BottomX:=BottomX + BottomOffsetX;
    end;
  end;

 {Check our X position}
 if BottomOffsetX > 0 then
  begin
   {Moving Right}
   if BottomX + BottomW >= Width then
    begin
     BottomOffsetX:=-BOTTOM_OFFSET_X;
    end;
  end
 else
  begin
   {Moving Left}
   if BottomX <= 0 then
    begin
     BottomOffsetX:=BOTTOM_OFFSET_X;
    end;
  end;
  
 {End our picture and render it to the screen}
 VGShapesEnd;
end;

begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Multiple Layers Demo');

 {Wait a couple of seconds for C:\ drive to be ready}
 ConsoleWindowWriteLn(WindowHandle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a moment}
   Sleep(100);
  end;
 ConsoleWindowWriteLn(WindowHandle,'C:\ drive is ready');
 ConsoleWindowWriteLn(WindowHandle,'');
 
 {We have ported the libShapes library created by A J Starks to Ultibo so you can easily use
  all of the functionality it provides. You can find out more about libShapes and what it can
  do by visiting the GitHub repository at:
  
  https://github.com/ajstarks/openvg
  
  To get you started this example and the previous ones show some of the basics}
  
 {We're going to create three layers, the first will contain the Hello VG example
  and the other two will each contain a shape that roams around the screen.
  
  Because the shapes are on a different layer to the background each can be drawn
  independently. This is similar to what you might do with a graphical display that
  included some static information (such as controls, guages and text) and some that
  is updated continuously such as the control readings and the guage pointers.
  
  Redrawing only the information that has changed saves processing time and allows
  for a smoother interface with a more detailed display} 
  
 {The first thing to do is to initialize our Hello VG layer which will be 0}
 HelloLayer:=0;
 
 {Initialize OpenVG and the VGShapes unit for Layer 0}
 {We're also going to demonstrate tranparency and allow the framebuffer window to 
  remain visible under our demo so we set the tranparency to transparent from source}
 ConsoleWindowWriteLn(WindowHandle,'Creating Hello Layer');
 VGShapesInit(Width,Height,DISPMANX_FLAGS_ALPHA_FROM_SOURCE,HelloLayer);

 {Start a picture the full width and height of the screen}
 VGShapesStart(Width,Height,True);

 {Make the background black and 75% transparent}
 VGShapesBackgroundRGB(0,0,0,0.25);
 
 {Draw a big blue circle to represent the world, make it 50% transparent}
 VGShapesFill(44,77,232,0.5);
 VGShapesCircle(Width / 2,0,Width);
 
 {Draw our Hello World text in white using the included Serif typeface}
 VGShapesFill(255,255,255,1);
 VGShapesTextMid(Width / 2,(Height * 0.7),'OpenVG Layers',VGShapesSerifTypeface,Width div 15);
 
 {End our picture and render it to the screen}
 VGShapesEnd;

 {Sleep for a second so you see it before continuing}
 Sleep(1000);

 {Intialize the other two layers for the demo which will be numbered 1 and 2.
  The layers go from lowest at the back to highest at the front, so our top
  layer must be a higher number than out bottom layer to appear in front}
 TopLayer:=2;
 BottomLayer:=1;
  
 {Initialize OpenVG and the VGShapes unit unit for Layers 1 and 2}
 {Again we set the tranparency to transparent from source}
 ConsoleWindowWriteLn(WindowHandle,'Creating Top Layer');
 VGShapesInit(Width,Height,DISPMANX_FLAGS_ALPHA_FROM_SOURCE,TopLayer);
 ConsoleWindowWriteLn(WindowHandle,'Creating Bottom Layer');
 VGShapesInit(Width,Height,DISPMANX_FLAGS_ALPHA_FROM_SOURCE,BottomLayer);
 
 {Convert the RGB colors to use for our layers into a TVGShapesColor record}
 {We make the body of the shapes just slightly (10%) transparent as well}
 VGShapesRGBA(205,55,55,0.90,TopColor);
 VGShapesRGB(205,205,55,TopOutline);
 
 VGShapesRGBA(55,55,205,0.90,BottomColor);
 VGShapesRGB(55,205,205,BottomOutline);
 
 {Calculate some default values based on the size of the screen, remember that OpenVG 
  coordinates put 0,0 at the bottom left NOT the top left like most other things}
 TopW:=Width * 0.11;
 TopH:=TopW;
 TopX:=TopW / 2;
 TopY:=Height * 0.50;
 TopOffsetX:=TOP_OFFSET_X;
 TopOffsetY:=TOP_OFFSET_Y;

 BottomW:=Width * 0.14;
 BottomH:=BottomW;
 BottomX:=Width * 0.50;
 BottomY:=Height - BottomH;
 BottomOffsetX:=BOTTOM_OFFSET_X;
 BottomOffsetY:=-BOTTOM_OFFSET_Y;
 
 {Loop over and over drawing both of our shapes}
 while True do
  begin
   {Draw our Top layer}
   DrawTopLayer;

   {Draw our Bottom layer}
   DrawBottomLayer;
   
   Sleep(0);
   
   {Check for a key press}
   if ConsoleKeypressed then Break; 
  end;
 
 {When a key is pressed, remove each layer one at a time}
 {Sleep for a second between each one so you can see it}
 Sleep(1000);
 
 {Switch to our Top layer}
 ConsoleWindowWriteLn(WindowHandle,'Removing Top Layer');
 VGShapesSetLayer(TopLayer);
 
 {Clear our screen, cleanup OpenVG and deinitialize VGShapes for the Top layer}
 VGShapesFinish;

 Sleep(1000);

 {Switch to our Bottom layer}
 ConsoleWindowWriteLn(WindowHandle,'Removing Bottom Layer');
 VGShapesSetLayer(BottomLayer);
 
 {Clear our screen, cleanup OpenVG and deinitialize VGShapes for the Bottom layer}
 VGShapesFinish;

 Sleep(1000);
 
 {Now switch to our Hello VG layer}
 ConsoleWindowWriteLn(WindowHandle,'Removing Hello Layer');
 VGShapesSetLayer(HelloLayer);
 
 {Clear our screen, cleanup OpenVG and deinitialize VGShapes for the Hello VG layer}
 VGShapesFinish;
 
 {VGShapes calls BCMHostInit during initialization, we should also call BCMHostDeinit to cleanup}
 BCMHostDeinit;
 
 ConsoleWindowWriteLn(WindowHandle,'Completed VGShapes Demo');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

