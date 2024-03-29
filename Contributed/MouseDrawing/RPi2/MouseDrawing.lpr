program MouseDrawing;

{$mode objfpc}{$H+}

{ Contributed example - Mouse Drawing                                          }
{                                                                              }
{ An combination of example 3 and example 15 to create a very simple drawing   }
{  program.                                                                    }
{                                                                              }
{ Contributed by Mgreim 03.JUL.2016                                            }
{ Original source: https://github.com/MGreim/ultibomousegraphicdemo            }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B/3B+/3A+/Zero2W}
{ To create a version for the A/B/A+/B+/Zero or for the 4B/400 simply create a }
{ new project, copy this code into it and modify as required.                  }

{Declare some units used by this example.}
uses
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  Framebuffer,
  BCM2836,
  BCM2709,
  SysUtils,
  Mouse,
  USBHID,      {Include the USB HID driver and the HID mouse consumer}
  HIDMouse,
  HIDTouch,
  // Keyboard, {Keyboard uses USB so that will be included automatically}
  DWCOTG,      {We need to include the USB host driver for the Raspberry Pi}
  linecircle;




{We'll need a few more variables for this example.}
var
 RowCount:LongWord;
 ColumnCount:LongWord;
 CurrentX :LongWord;
 CurrentY :LongWord;
 Handle1:TWindowHandle;
 altx, alty, neux, neuy : longint;
 Count:LongWord;
 MouseData:TMouseData;
 ScreenWidth, ScreenHeight : LongWord;
 maxheight, maxwidth : LongInt;



begin
 // MouseInit;
 {Let's create a console window again but this time on the left side of the screen}
 Handle1:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 {To prove that worked let's output some text on the console window}
 ConsoleWindowWriteLn(Handle1,'Welcome to MGs extended Mouse example');


 {Now let's get the current position of the console cursor into a couple of variables}
 ConsoleWindowGetXY(Handle1,CurrentX,CurrentY);

  if FramebufferGetPhysical(ScreenWidth,ScreenHeight) = ERROR_SUCCESS then
   begin
    {Print our screen dimensions on the console}
    ConsoleWindowWriteLn(Handle1,'Screen is ' + IntToStr(ScreenWidth) + ' pixels wide by ' + IntToStr(ScreenHeight) + ' pixels high');
   end;

 // Now try to load a new cursorshape from the disk, otherwise take the default mycursor
 CreateCursor;

 // writeln seems to work as well
 writeln('Press the left mouse button to draw a line, press the right mouse button to terminate the program');

//   We'll use a couple of variables to track the position in response to mouse messages}

  // My new things... and it works
  maxWidth := ScreenWidth;
  maxHeight := ScreenHeight;
  altx:=maxWidth div 2;
  alty:=maxHeight div 2;

  ConsoleWindowSetX(Handle1,altx);
  ConsoleWindowSetY(Handle1,alty);
  CursorSetState(True,altx, alty,False);

  Count := 0;
  REPEAT
      if MouseRead(@MouseData,SizeOf(MouseData),Count) = ERROR_SUCCESS then
      begin
         neux := altx + MouseData.OffsetX;
         neuy := alty + MouseData.OffsetY;
         IF neux > maxwidth THEN neux := maxWidth;
         IF neuy > maxHeight THEN neuy := maxHeight;
         IF neux < 0 THEN neux := 0;
         IF neux < 0 THEN neuy := 0;
         CursorSetState(True,neuX,neuY,False);

        if (MouseData.Buttons and MOUSE_LEFT_BUTTON) <> 0 then
           begin
//                ConsoleWindowWriteLn(Handle1, 'Left Button pressed');

 //               FramebufferConsoleDrawLine(ConsoleDeviceGetDefault,altx,alty,neux,neuy,COLOR_RED,2);
                  line(altx, alty, neux, neuy, COLOR_RED);
           END;
         altx := altx + MouseData.Offsetx;
         alty := alty + MouseData.OffsetY;
         IF altx > maxWidth THEN altx := maxwidth;
         IF alty > maxHeight THEN alty := maxHeight;
         IF altx < 0 THEN altx := 0;
         IF alty < 0 THEN alty := 0;

      end;
  UNTIL ((MouseData.Buttons AND MOUSE_RIGHT_BUTTON) <> 0);


 // my end


 {Update our original console}
 ConsoleWindowWriteLn(Handle1,'Clearing the new console');


 {And say goodbye}
 ConsoleWindowWriteLn(Handle1,'All done, thanks for watching');

 {We're not doing a loop this time so we better halt this thread before it exits}
 ThreadHalt(0);
end.

