program MouseCursor;

{$mode objfpc}{$H+}

{ Example 15 Mouse Cursor                                                      }
{                                                                              }
{ This is a simple example that shows how to read data from the mouse buffer   }
{ and use it to track the position of a mouse cursor on the screen.            }
{                                                                              }
{ We also introduce a couple of new concepts like detecting which type of      }
{ Raspberry Pi our program is running on and using some of the different types }
{ of memory available in Ultibo to pass a block of memory to the graphics      }
{ processor (GPU) so we can define a bitmap for our mouse pointer on the       }
{ screen.                                                                      }
{                                                                              }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{  Once compiled copy the kernel7.img file to an SD card along with the        }
{  firmware files and use it to boot your Raspberry Pi.                        }
{                                                                              }
{  Raspberry Pi 3B version                                                     }
{   What's the difference? See Project, Project Options, Config and Target.    }

{Declare some units used by this example.}
uses
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  Framebuffer,
  BCM2837,
  BCM2710,
  SysUtils,
  Mouse,       {Mouse uses USB so that will be included automatically}
  DWCOTG,      {We need to include the USB host driver for the Raspberry Pi}
  HeapManager; {Include the heap manager so we can allocate some different types of memory}

{We'll need two window handles and some mouse tracking variables.}
var
 Count:LongWord;
 Buttons:String;
 MouseData:TMouseData;

 Handle1:TWindowHandle;
 Handle2:TWindowHandle;

 CursorX:LongInt;
 CursorY:LongInt;

 ScreenWidth:LongWord;
 ScreenHeight:LongWord;

{See the main program below for more info about what this function is for}
procedure CreateCursor;
var
 Row:LongWord;
 Col:LongWord;
 Offset:LongWord;
 Size:LongWord;
 Cursor:PLongWord;
 Address:LongWord;
begin
  {Make our cursor 32 x 32 pixels, each pixel is 4 bytes}
  Size:=32 * 32 * 4;

  {Allocate a block of memory to create our new mouse cursor.

   For different versions of the Raspberry Pi we need to allocate different
   types of memory when communicating with the graphics processor (GPU).

   Check what type of Raspberry Pi we have}
  case BoardGetType of
   BOARD_TYPE_RPIA,BOARD_TYPE_RPIB,
   BOARD_TYPE_RPIA_PLUS,BOARD_TYPE_RPIB_PLUS,
   BOARD_TYPE_RPI_ZERO:begin
     {We have an A/B/A+/B+ or Zero}
     {Allocate some Shared memory for our cursor}
     Cursor:=AllocSharedMem(Size);
    end;
   BOARD_TYPE_RPI2B,BOARD_TYPE_RPI3B:begin
     {We have a 2B or 3B}
     {Allocate some No Cache memory instead}
     Cursor:=AllocNoCacheMem(Size);
    end;
   else
    begin
     {No idea what board this is}
     Cursor:=nil;
    end;
  end;

  {Check if we allocated some memory for our cursor}
  if Cursor <> nil then
   begin
    {Loop through all of the LongWord positions in our cursor and set alternating
     color pixels. I'll leave it to you to work out what is happening here and
     how to change it to get different mouse cursor effects. The cursor is just
     a bitmap so you can make anything you can imagine.

     The color bits are Alpha, Red, Green and Blue}
    Offset:=0;
    for Row:=0 to 31 do
     begin
      for Col:=0 to 31 do
       begin
        if ((Col and 8) xor (Row and 8)) <> 0 then
         begin
          Cursor[Col + Offset]:=$a0ff0000;
         end
        else
         begin
          Cursor[Col + Offset]:=$a00000ff;
         end;
       end;

      Inc(Offset,32);
     end;

    {Convert our cursor pointer into an address that the GPU can understand, this
     is because it sees the world differently to what we do and uses different
     address ranges}
    Address:=PhysicalToBusAddress(Cursor);

    {Now call Cursor Set Info to load our new cursor into the GPU}
    CursorSetInfo(32,32,0,0,Pointer(Address),Size);

    {And finally free the memory that we allocated}
    FreeMem(Cursor);
   end;
end;

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

   First we need to find out how big the screen is in pixels wide and high}
  if FramebufferGetPhysical(ScreenWidth,ScreenHeight) = ERROR_SUCCESS then
   begin
    {Print our screen dimensions on the console}
    ConsoleWindowWriteLn(Handle1,'Screen is ' + IntToStr(ScreenWidth) + ' pixels wide by ' + IntToStr(ScreenHeight) + ' pixels high');
   end;

  {We need to create a mouse cursor to display on the screen, this takes a little
   bit of extra code which you can follow if you look at the CreateCursor function}
  CreateCursor;

  {Now we can turn on the cursor and set it to the middle of the screen.

   We'll use a couple of variables to track the position in response to mouse messages}
  CursorX:=ScreenWidth div 2;
  CursorY:=ScreenHeight div 2;
  CursorSetState(True,CursorX,CursorY,False);

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
      CursorX:=CursorX + MouseData.OffsetX;
      if CursorX < 0 then CursorX:=0;
      if CursorX > (ScreenWidth - 1) then CursorX:=ScreenWidth - 1;

      CursorY:=CursorY + MouseData.OffsetY;
      if CursorY < 0 then CursorY:=0;
      if CursorY > (ScreenHeight - 1) then CursorY:=ScreenHeight - 1;

      {And move the cursor on the screen}
      CursorSetState(True,CursorX,CursorY,False);
     end
    else
     begin
      {For some reason there was an error, print a message on the console}
      ConsoleWindowWriteLn(Handle2,'An error occurred reading the mouse, trying again');
     end;
   end;

  {No need to halt, since we never exit the loop}
end.

