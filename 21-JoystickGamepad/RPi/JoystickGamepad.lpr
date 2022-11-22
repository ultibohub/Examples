program JoystickGamepad;

{$mode objfpc}{$H+}

{ Example 21 Joystick Gamepad                                                  }
{                                                                              }
{ An example that shows how to use the Joystick unit to read input from a USB  }
{ joystick or gamepad device and use it to move a cross around the screen and  }
{ draw a colored circle wherever a button is pressed.                          }
{                                                                              }
{ It's more exciting than it sounds, even if you don't want to use a joystick  }
{ in your application if you have one lying around give it a go just for fun.  }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{  Once compiled copy the kernel7.img file to an SD card along with the        }
{  firmware files and use it to boot your Raspberry Pi.                        }
{                                                                              }
{  Raspberry Pi A/B/A+/B+/Zero/ZeroW version                                   }
{   What's the difference? See Project, Project Options, Config and Target.    }

{Declare some units used by this example.}
uses
  GlobalConst,
  GlobalTypes,
  GlobalConfig,
  Platform,
  Threads,
  Console,
  Font,
  GraphicsConsole,
  Framebuffer,
  BCM2835,
  BCM2708,
  SysUtils,
  Joystick,
  USBHID,      {Include the USB HID driver and the HID joystick consumer}
  HIDJoystick,
  DWCOTG;      {We need to include the USB host driver for the Raspberry Pi}

const
 {Define some custom colors (From: https://www.w3schools.com/tags/ref_colornames.asp)}
 Aqua = $FF00FFFF;
 BlueViolet = $FF8A2BE2;
 Chartreuse = $FF7FFF00;
 Crimson = $FFDC143C;
 DarkOrange = $FFFF8C00;
 DarkRed = $FF8B0000;
 DeepPink = $FFFF1493;
 DeepSkyBlue = $FF00BFFF;
 FireBrick = $FFB22222;
 ForestGreen = $FF228B22;
 Fuchsia = $FFFF00FF;
 Gold = $FFFFD700;
 GreenYellow = $FFADFF2F;
 HotPink = $FFFF69B4;
 Indigo = $FF4B0082;
 LawnGreen = $FF7CFC00;
 LightCoral = $FFF08080;
 LightGreen = $FF90EE90;
 LightPink = $FFFFB6C1;
 LightSkyBlue = $FF87CEFA;
 Lime = $FF00FF00;
 Maroon = $FF800000;
 MediumOrchid = $FFBA55D3;
 MediumPurple = $FF9370DB;
 MidnightBlue = $FF191970;
 Moccasin = $FFFFE4B5;
 Olive = $FF808000;
 Orange = $FFFFA500;
 Orchid = $FFDA70D6;
 Peru = $FFCD853F;
 Plum = $FFDDA0DD;
 RoyalBlue = $FF4169E1;
 SeaGreen = $FF2E8B57;
 SkyBlue = $FF87CEEB;
 SlateBlue = $FF6A5ACD;
 SpringGreen = $FF00FF7F;
 SteelBlue = $FF4682B4;
 Teal = $FF008080;
 Tomato = $FFFF6347;
 Violet = $FFEE82EE;
 Yellow = $FFFFFF00;
 Magenta = $FFFF00FF;
 Red = $FFFF0000;

 BUTTON_COLORS:array[0..31] of LongWord = (
  Aqua,
  BlueViolet,
  Chartreuse,
  Crimson,
  DarkOrange,
  Yellow,
  DeepPink,
  DeepSkyBlue,
  Tomato,
  SpringGreen,
  Fuchsia,
  Gold,
  GreenYellow,
  HotPink,
  Indigo,
  LawnGreen,
  LightCoral,
  LightGreen,
  LightPink,
  LightSkyBlue,
  Lime,
  Magenta,
  MediumOrchid,
  MediumPurple,
  MidnightBlue,
  Moccasin,
  Olive,
  Orange,
  Orchid,
  Peru,
  Plum,
  Red);

 {Define a custom cursor}
 CURSOR_WIDTH = 32;
 CURSOR_HEIGHT = 32;
 CURSOR_FORMAT = COLOR_FORMAT_DEFAULT; {COLOR_FORMAT_ARGB32}
 CURSOR_HOTSPOTX = 16;
 CURSOR_HOTSPOTY = 16;
 CURSOR_CROSS:array[0..CURSOR_HEIGHT - 1,0..CURSOR_WIDTH - 1] of LongWord = (
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK),
  (COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK),
  (COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK),
  (COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK),
  (COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK),
  (COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE),
  (COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE,COLOR_NONE));

var
 {Some variables to track our cursor}
 CursorX:LongInt;
 CursorY:LongInt;

 ButtonX:LongInt;
 ButtonY:LongInt;
 ButtonIndex:LongInt;

 ScreenWidth:LongWord;
 ScreenHeight:LongWord;

 {The thread function to read input from our joystick}
 function JoystickThread(Parameter:Pointer):PtrInt;
 var
  Count:LongWord;
  Status:LongWord;

  AxisX:LongInt;
  AxisY:LongInt;
  IndexX:LongInt;
  IndexY:LongInt;
  MiddleX:LongInt;
  MiddleY:LongInt;

  JoystickData:TJoystickData;
  JoystickDevice:PJoystickDevice;
  JoystickProperties:TJoystickProperties;
 begin
  Result:=0;

  {Get the joystick or gamepad device}
  JoystickDevice:=JoystickDeviceFindByName('Joystick0');
  if JoystickDevice = nil then Exit;

  {Get the joystick or gamepad properties}
  if JoystickDeviceGetProperties(JoystickDevice,@JoystickProperties) <> ERROR_SUCCESS then Exit;

  {Find the X and Y axes from our device}
  {The properties will include the number of axes, hats and buttons found.

   For each axis or hat we can find the name and minimum and maximum values,
   for each button there is just a name that identifies the button.

   The name isn't a text value, that would be too slow, instead the name is
   a numeric identifier that can be easily checked by an application. We've
   defined some values in the Joystick unit but you can also assign your own
   values since the underlying code doesn't look at the name for anything}
  IndexX:=-1;
  IndexY:=-1;
  MiddleX:=-1;
  MiddleY:=-1;
  if JoystickProperties.AxisCount > 0 then
   begin
    for Count:=0 to JoystickProperties.AxisCount - 1 do
     begin
      {Check if this is an X axis}
      if JoystickProperties.Axes[Count].Name = JOYSTICK_AXIS_X then
       begin
        {Save the Index which we can use to lookup the X value in the input data}
        IndexX:=Count;

        {Calculate the middle value, most joysticks will report the middle value when idle}
        MiddleX:=(JoystickProperties.Axes[Count].Logical.Maximum - JoystickProperties.Axes[Count].Logical.Minimum) div 2;
       end;

      {Check if this is a Y axis}
      if JoystickProperties.Axes[Count].Name = JOYSTICK_AXIS_Y then
       begin
        {Save the Index again so we can lookup the Y value in the input data}
        IndexY:=Count;

        {Calculate the middle value for Y}
        MiddleY:=(JoystickProperties.Axes[Count].Logical.Maximum - JoystickProperties.Axes[Count].Logical.Minimum) div 2;
       end;
     end;
   end;

  {Save the current cursor X and Y, just for this thread}
  AxisX:=CursorX;
  AxisY:=CursorY;

  while True do
   begin
    {Read the next joystick input report from the queue}
    Status:= JoystickDeviceRead(JoystickDevice,@JoystickData,SizeOf(TJoystickData),JOYSTICK_FLAG_NONE,Count);
    if Status = ERROR_SUCCESS then
     begin
      {Check if we found an X axis}
      if IndexX <> -1 then
       begin
        {Use the latest X value to update our cursor position.

         Notice that we can use the index value from the properties to access
         the X axis value in the joystick data, that's because the indexes will
         be the same for every report received and will always match the properties.

         The same applies to hats and buttons so we can quickly reference any
         item in the data if we've already located it during initialization}
        if JoystickData.Axes[IndexX] < MiddleX then
         begin
          Dec(AxisX)
         end
        else if JoystickData.Axes[IndexX] > MiddleX then
         begin
          Inc(AxisX);
         end;
       end;

      {Check if we found a Y axis}
      if IndexY <> -1 then
       begin
        {Use the latest Y value to update our cursor position}
        if JoystickData.Axes[IndexY] < MiddleY then
         begin
          Dec(AxisY)
         end
        else if JoystickData.Axes[IndexY] > MiddleY then
         begin
          Inc(AxisY);
         end;
       end;

      {Check that our X value doesn't go off the screen}
      if AxisX < 0 then AxisX:=0;
      if AxisX > (ScreenWidth - 1) then AxisX:=ScreenWidth - 1;

      {Check that our Y value doesn't go off the screen}
      if AxisY < 0 then AxisY:=0;
      if AxisY > (ScreenHeight - 1) then AxisY:=ScreenHeight - 1;

      {Update the real cursor X and Y values}
      CursorX:=AxisX;
      CursorY:=AxisY;

      {Check if any buttons were found}
      if JoystickData.ButtonCount > 0 then
       begin
        for Count:=0 to JoystickData.ButtonCount - 1 do
         begin
          {Check if this button is pressed, buttons are only
           either on or off and don't return a value.

           Some devices have analog buttons that return a number
           and these will be reported in the axes array instead}
          if (JoystickData.Buttons and (1 shl Count)) <> 0 then
           begin
            ButtonX:=CursorX;
            ButtonY:=CursorY;
            ButtonIndex:=Count;

            Break;
           end;
         end;
       end;
     end
    else if Status <> ERROR_NO_MORE_ITEMS then
     begin
      {If something went wrong like the joystick or gamepad got
       unplugged then stop reading events and terminate our thread}
      Break;
     end;
   end;
 end;

var
 {Some variables to initialize the example and track our drawing}
 WindowHandle:TWindowHandle;

 Start:Int64;

 FontWidth:LongWord;
 FontHeight:LongWord;

 CursorLastX:LongInt;
 CursorLastY:LongInt;

 ButtonLastX:LongInt;
 ButtonLastY:LongInt;
 ButtonLastIndex:LongInt;

 ThreadHandle:TThreadHandle;

 FramebufferDevice:PFramebufferDevice;
 FramebufferProperties:TFramebufferProperties;

begin
 {Create a graphics console window that covers the full screen}
 WindowHandle:=GraphicsWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULLSCREEN);

 {Get the width and height of the font from our window}
 FontWidth:=FontGetWidth(GraphicsWindowGetFont(WindowHandle));
 FontHeight:=FontGetHeight(GraphicsWindowGetFont(WindowHandle));

 {Output some welcome text on the console window}
 GraphicsWindowDrawText(WindowHandle,'Welcome to Example 21 Joystick Gamepad',1,1);
 GraphicsWindowDrawText(WindowHandle,'Use the joystick to move the cross',1,(FontHeight * 2) + 1);
 GraphicsWindowDrawText(WindowHandle,'Press any button to draw a colored circle',1,(FontHeight * 3) + 1);

 {Wait for a joystick or gamepad to be available}
 if JoystickDeviceFindByName('Joystick0') = nil then
  begin
   GraphicsWindowDrawText(WindowHandle,'Waiting for a joystick or gamepad device',1,(FontHeight * 5) + 1);

   {Get the current tick count (in milliseconds)}
   Start:=GetTickCount64;
   while JoystickDeviceFindByName('Joystick0') = nil do
    begin
     {Sleep for 10 milliseconds}
     Sleep(10);

     {Check the tick count to see if 10 seconds has passed}
     if GetTickCount64 > (Start + 10000) then Break;
    end;
  end;

 {Check to see if a joystick or gamepad was found}
 if JoystickDeviceFindByName('Joystick0') = nil then
  begin
   GraphicsWindowDrawText(WindowHandle,'Unable to detect a joystick or gamepad device, the example may not work',1,(FontHeight * 6) + 1);
  end
 else
  begin
   GraphicsWindowDrawText(WindowHandle,'Joystick0 detected successfully',1,(FontHeight * 6) + 1);
  end;

 {We'll setup the cursor much the same as we did in Example 15. First we get the size of the screen
  by getting the properties of the default framebuffer and storing the width and height for later}

 {Get the default framebuffer device}
 FramebufferDevice:=FramebufferDeviceGetDefault;

 {Get the properties of the default framebuffer}
 FramebufferDeviceGetProperties(FramebufferDevice,@FramebufferProperties);

 {Get the screen width and height from the properties}
 ScreenWidth:=FramebufferProperties.PhysicalWidth;
 ScreenHeight:=FramebufferProperties.PhysicalHeight;

 {Set the cursor, this time we're using a custom cursor shape and not the default arrow}
 FramebufferDeviceSetCursor(FramebufferDevice,CURSOR_WIDTH,CURSOR_HEIGHT,CURSOR_HOTSPOTX,CURSOR_HOTSPOTY,@CURSOR_CROSS,SizeOf(CURSOR_CROSS));

 {Setup the cursor state, because we are going to use a thread to read the joystick input
  we'll also store the last X and Y position of the cursor so we know if it has been moved}
 CursorX:=ScreenWidth div 2;
 CursorY:=ScreenHeight div 2;
 CursorLastX:=CursorX;
 CursorLastY:=CursorY;

 {Enable the cursor and set it to the center of the screen}
 FramebufferDeviceUpdateCursor(FramebufferDevice,True,CursorX,CursorY,False);

 {Setup our button state, again we track the last position as well for easy comparison}
 ButtonX:=-1;
 ButtonY:=-1;
 ButtonIndex:=-1;
 ButtonLastX:=-1;
 ButtonLastY:=-1;
 ButtonLastIndex:=-1;

 {Joysticks and Gamepads can send data at a very rapid rate so we're going to use a thread
  to read the input data from the joystick and update the position of the cursor and our
  button state. This thread only reads the data and doesn't try to draw anything on the
  screen which means it can process the joystick events very quickly.

  If we tried to read the data and draw the screen the input buffer from the joystick
  might overflow and we could lose events coming from the joystick}
 ThreadHandle:=BeginThread(@JoystickThread,nil,ThreadHandle,THREAD_STACK_DEFAULT_SIZE);

 {Loop endlessly, checking for cursor and button changes}
 while True do
  begin
   {Check if the cursor has moved}
   if (CursorX <> CursorLastX) or (CursorY <> CursorLastY) then
    begin
     {Move the cursor on the screen}
     FramebufferDeviceUpdateCursor(FramebufferDevice,True,CursorX,CursorY,False);

     {Save the last X and Y of the cursor}
     CursorLastX:=CursorX;
     CursorLastY:=CursorY;
    end;

   {Check if a button has been pressed}
   if (ButtonX <> ButtonLastX) or (ButtonY <> ButtonLastY) or (ButtonIndex <> ButtonLastIndex) then
    begin
     {Draw a circle on the screen}
     GraphicsWindowDrawCircle(WindowHandle,ButtonX,ButtonY,BUTTON_COLORS[ButtonIndex],40,40);

     {Uncomment this line to put the button index number in the middle of the circle
      so you can see the relationship between the buttons and the joystick data}
     //GraphicsWindowDrawText(WindowHandle,IntToStr(ButtonIndex),ButtonX - (FontWidth div 2),ButtonY - (FontHeight div 2));

     {Save the last X, Y and Index of the button}
     ButtonLastX:=ButtonX;
     ButtonLastY:=ButtonY;
     ButtonLastIndex:=ButtonIndex;
    end;

   {Yield the CPU so other threads can run}
   Sleep(0);
  end;

 {No need to halt, since we never exit the loop}
end.
