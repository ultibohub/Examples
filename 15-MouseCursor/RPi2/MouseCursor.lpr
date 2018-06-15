program MouseCursor;

{$mode objfpc}{$H+}

{ Example 15 Mouse Cursor                                                      }
{                                                                              }
{ This is a simple example that shows how to read data from the mouse buffer   }
{ and use it to track the position of a mouse cursor on the screen.            }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{  Once compiled copy the kernel7.img file to an SD card along with the        }
{  firmware files and use it to boot your Raspberry Pi.                        }
{                                                                              }
{  Raspberry Pi 2B version                                                     }
{   What's the difference? See Project, Project Options, Config and Target.    }

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
  Mouse,       {Mouse uses USB so that will be included automatically}
  DWCOTG;      {We need to include the USB host driver for the Raspberry Pi}

{We'll need two window handles and some mouse tracking variables.}
var
 Count:LongWord;
 Buttons:String;
 MouseData:TMouseData;

 Handle1:TWindowHandle;
 Handle2:TWindowHandle;

 CursorX:LongInt;
 CursorY:LongInt;

 ScalingX:Double;
 ScalingY:Double;
 
 ScreenWidth:LongWord;
 ScreenHeight:LongWord;

 FramebufferDevice:PFramebufferDevice;
 FramebufferProperties:TFramebufferProperties;
begin
  {Create a console window on the left}
  Handle1:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,False);

  {And another one on the right}
  Handle2:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_RIGHT,True);

  {Output some welcome text on the left console window}
  ConsoleWindowWriteLn(Handle1,'Welcome to Example 15 Mouse Cursor');
  ConsoleWindowWriteLn(Handle1,'Make sure you have a USB mouse connected, try moving it and clicking the buttons to see what happens');

  {Let's setup the mouse cursor so we can move it around the screen. The Raspberry Pi
   has cursor support built in to the graphics processor (GPU) so we can use that to
   display the cursor on screen.

   First we need to find out how big the screen is in pixels wide and high, to do this
   we ask for the default framebuffer device and then retrieve the properties which will
   give us the size as well as lots of other information.
   
   Get the default framebuffer device}
  FramebufferDevice:=FramebufferDeviceGetDefault;
  
  {Get the properties of the default framebuffer}
  FramebufferDeviceGetProperties(FramebufferDevice,@FramebufferProperties);
  
  {Get the screen width and height from the properties}
  ScreenWidth:=FramebufferProperties.PhysicalWidth;
  ScreenHeight:=FramebufferProperties.PhysicalHeight;
  
  {Print our screen dimensions on the console}
  ConsoleWindowWriteLn(Handle1,'Screen is ' + IntToStr(ScreenWidth) + ' pixels wide by ' + IntToStr(ScreenHeight) + ' pixels high');

  {We need to create a mouse cursor to display on the screen, for this example we'll just
   use the built in arrow which is the default cursor. If you read the description of the
   FramebufferDeviceSetCursor function you will see that you can define a block of pixels
   to create your own cursor shape, to use the default we simply pass zero or nil for all
   the parameters except the framebuffer device itself}
  FramebufferDeviceSetCursor(FramebufferDevice,0,0,0,0,nil,0);

  {Now we can turn on the cursor and set it to the middle of the screen.

   We'll use a couple of variables to track the position in response to mouse messages}
  CursorX:=ScreenWidth div 2;
  CursorY:=ScreenHeight div 2;
  FramebufferDeviceUpdateCursor(FramebufferDevice,True,CursorX,CursorY,False);

  {Loop endlessly checking for Mouse messages}
  while True do
   begin
    {Read the next message from the global mouse buffer. If there are multiple mice
     attached then messages from all of them will appear in the same buffer.

     What happens if no mouse is attached?

     The MouseRead function will simply wait forever, Ultibo has dynamic USB attach
     and detach so as soon as you plug in a mouse the messages will start to arrive}

    {The MouseRead function needs a buffer to return TMouseData structures into, if
     you pass a buffer that is bigger than a single TMouseData then it can return
     multiple messages in one request and the Count variable will tell you how many
     messages were received}
    if MouseRead(@MouseData,SizeOf(TMouseData),Count) = ERROR_SUCCESS then
     begin
      {We received a mouse message so let's process it to see what it contains.

      The TMouseData structure will give us an X an Y Offset as well as any buttons
      that are currently pressed.}

      {Check the buttons}
      if MouseData.Buttons = 0 then
       begin
        Buttons:='None';
       end
      else
       begin
        if (MouseData.Buttons and (MOUSE_LEFT_BUTTON or MOUSE_RIGHT_BUTTON)) = (MOUSE_LEFT_BUTTON or MOUSE_RIGHT_BUTTON) then
         begin
          Buttons:='Both';
         end
        else if (MouseData.Buttons and MOUSE_LEFT_BUTTON) = MOUSE_LEFT_BUTTON then
         begin
          Buttons:='Left';
         end
        else if (MouseData.Buttons and MOUSE_RIGHT_BUTTON) = MOUSE_RIGHT_BUTTON then
         begin
          Buttons:='Right';
         end
        else
         begin
          Buttons:='Other';
         end;
       end;

      {Display the mouse data in the right window}
      ConsoleWindowWriteLn(Handle2,'Mouse OffsetX = ' + IntToStr(MouseData.OffsetX) + ' OffsetY = ' + IntToStr(MouseData.OffsetY) + ' Buttons = ' + Buttons);

      {Now update our mouse tracking for cursor X and Y}
      {Check if the X value is absolute instead of relative}
      if (MouseData.Buttons and MOUSE_ABSOLUTE_X) = MOUSE_ABSOLUTE_X then
       begin
        {For absolute values the maximum X field allows us to scale
         the cursor X value relative to the size of our screen}
        ScalingX:=MouseData.MaximumX / ScreenWidth;
        if ScalingX <= 0 then ScalingX:=1.0;
        
        CursorX:=Trunc(MouseData.OffsetX / ScalingX);
       end
      else
       begin
        CursorX:=CursorX + MouseData.OffsetX;
       end; 
      if CursorX < 0 then CursorX:=0;
      if CursorX > (ScreenWidth - 1) then CursorX:=ScreenWidth - 1;

      {Check if the Y value is absolute}
      if (MouseData.Buttons and MOUSE_ABSOLUTE_Y) = MOUSE_ABSOLUTE_Y then
       begin
        {Use maximum Y to scale the Y value to the screen}
        ScalingY:=MouseData.MaximumY / ScreenHeight;
        if ScalingY <= 0 then ScalingY:=1.0;
        
        CursorY:=Trunc(MouseData.OffsetY / ScalingY);
       end
      else
       begin
        CursorY:=CursorY + MouseData.OffsetY;
       end;
      if CursorY < 0 then CursorY:=0;
      if CursorY > (ScreenHeight - 1) then CursorY:=ScreenHeight - 1;

      {And move the cursor on the screen}
      FramebufferDeviceUpdateCursor(FramebufferDevice,True,CursorX,CursorY,False); 
     end
    else
     begin
      {For some reason there was an error, print a message on the console}
      ConsoleWindowWriteLn(Handle2,'An error occurred reading the mouse, trying again');
     end;
   end;

  {No need to halt, since we never exit the loop}
end.
