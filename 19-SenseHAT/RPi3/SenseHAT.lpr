program SenseHAT;

{$mode objfpc}{$H+}

{ Example 19 SenseHAT                                                          }
{                                                                              }
{ A very simple example that shows how you can use the Raspberry Pi SenseHAT   }
{ with Ultibo.                                                                 }
{                                                                              }
{ The joystick buttons are read and the names displayed on the LED matrix to   }
{ show which button was pressed.                                               }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{  Once compiled copy the kernel7.img file to an SD card along with the        }
{  firmware files and use it to boot your Raspberry Pi.                        }
{                                                                              }
{  Raspberry Pi 3B/3B+/3A+ version                                             }
{   What's the difference? See Project, Project Options, Config and Target.    }

{Declare some units used by this example.}
uses
  RaspberryPi3,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  Framebuffer,
  Console,
  GraphicsConsole,
  Keyboard,
  Font,
  SysUtils,
  Latin1_8x8,  {Use the Latin1 8x8 font which is the right size for the LED matrix}
  RPiSenseHat; {Include the SenseHAT driver to enable it for our project}

{We'll need a window handle and a framebuffer device.}
var
 Count:Integer;
 Buffer:String;
 Character:Char;

 WindowHandle:TWindowHandle;
 FramebufferDevice:PFramebufferDevice;
begin
  {Get the framebuffer device which represents our SenseHAT 8x8 LED matrix, the
   description can be found in the header of the SenseHAT driver unit}
  FramebufferDevice:=FramebufferDeviceFindByDescription('Raspberry Pi Sense HAT Framebuffer');
  
  {Now we create a graphics window on the console which is attached to our SenseHAT.
  
   All framebuffer console devices will have a description that begins with the words
   "Framebuffer Console" and has the name of the framebuffer device in brackets}
  WindowHandle:=GraphicsWindowCreate(ConsoleDeviceFindByDescription('Framebuffer Console (' + DeviceGetName(@FramebufferDevice^.Device) + ')'),CONSOLE_POSITION_FULLSCREEN);
  
  {Set the graphics window font to be Latin1 8x8}
  GraphicsWindowSetFont(WindowHandle,FontFindByName('Latin1-8x8'));
  
  {Set the foreground and background colors to be white on black}
  GraphicsWindowSetBackcolor(WindowHandle,COLOR_BLACK);
  GraphicsWindowSetForecolor(WindowHandle,COLOR_WHITE);
  
  {And finally clear the window which will blank our SenseHAT display}
  GraphicsWindowClear(WindowHandle);
  
  {The SenseHAT joystick appears in Ultibo as a keyboard so we can just
   use ConsoleGetKey to check for the Left, Right, Up, Down and Enter 
   keys will be sent by the joystick.  
   
   If you have a real keyboard plugged in you will also see that the arrow
   and enter keys do the same thing}
   
  {Loop endlessly while checking for Keyboard characters}
  while True do
   begin
    {Read a character from the global keyboard buffer}
    if ConsoleGetKey(Character,nil) then
     begin
      {Empty our buffer}
      Buffer:='';
      
      {Before we do anything, check what key was pressed}
      if Character = #0 then
       begin
        {The Left, Right, Up and Down arrows will appear as extended keys so
         the first character received will be 0, we call ConsoleGetKey again
         to receive the actual character.
         
         You can find the complete list of these extended keys here:
         
          https://www.freepascal.org/docs-html/rtl/keyboard/kbdscancode.html}
        ConsoleGetKey(Character,nil);
        case Character of
         {Left Arrow}
         #75:Buffer:='Left';
         {Right Arrow}
         #77:Buffer:='Right';
         {Up Arrow}
         #72:Buffer:='Up';
         {Down Arrow}
         #80:Buffer:='Down';
        end;
       end
      else if Character = #13 then
       begin
        {Check if the enter key was pressed}
        Buffer:='Enter';
       end
      else
       begin
        {Some other key was pressed, just ignore it}
       end;
    
      {If one of the joystick buttons was pressed above then our buffer will
       contain the message to be displayed}
      if Length(Buffer) > 0 then
       begin
        {Display each character in the buffer on our SenseHAT window}
        for Count:=1 to Length(Buffer) do
         begin
          {The SenseHAT is exactly 8x8 pixels just like our Latin1 8x8 font
           so we just display the character at position 0, 0}
          GraphicsWindowDrawText(WindowHandle,Buffer[Count],0,0);
          
          {Sleep for a short time to allow us to see the character}
          Sleep(400);
          
          {Then loop and display the next one}
         end;

        {Sleep for a little longer after the last character before returning
         to check for the next key press}
        Sleep(1000);
        
        {Clear the display}
        GraphicsWindowClear(WindowHandle);
       end;
     end;
     
    {No need to sleep on each loop, ConsoleGetKey will wait until a key is pressed}
   end;
   
  {No need to halt, since we never exit the loop}
end.
