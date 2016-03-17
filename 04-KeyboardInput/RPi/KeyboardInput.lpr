program KeyboardInput;

{$mode objfpc}{$H+}

{ Example 04 Keyboard Input                                                    }
{                                                                              }
{  Example 03 showed some of the screen output capabilities, now we want to    }
{  read from a connected keyboard and print the typed characters on the screen.}
{                                                                              }
{  In this example we also begin to use the USB which is an important element  }
{  to allow Raspberry Pi to communicate with other devices.                    }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{  Once compiled copy the kernel.img file to an SD card along with the firmware}
{  files and use it to boot your Raspberry Pi.                                 }
{                                                                              }
{  Raspberry Pi A/B/A+/B+/Zero version                                         }
{   What's the difference? See Project, Project Options, Config and Target.    }

{Declare some units used by this example.}
uses
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  Framebuffer,
  BCM2835,
  BCM2708,
  SysUtils,
  Keyboard, {Keyboard uses USB so that will be included automatically}
  DWCOTG;   {We need to include the USB host driver for the Raspberry Pi}

{We'll need a window handle again.}
var
 Character:Char;
 WindowHandle:TWindowHandle;


begin
 {Create a console window at full size}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 {Output some welcome text on the console window}
 ConsoleWindowWriteLn(WindowHandle,'Welcome to Example 04 Keyboard Input');
 ConsoleWindowWriteLn(WindowHandle,'Make sure you have a USB keyboard connected and start typing some characters');

 {Loop endlessly while checking for Keyboard characters}
 while True do
  begin
   {Read a character from the global keyboard buffer. If multiple keyboards are
    connected all characters will end up in a single buffer and be received here

    What happens if no keyboard is connected?

    Ultibo has dynamic USB attach and detach so just plug one in and start typing}
   if ConsoleReadChar(Character,nil) then
    begin
     {Before we print the character to the screen, check what was pressed}
     if Character = #13 then
      begin
       {If the enter key was pressed, write a new line to the console instead of a
        character}
       ConsoleWindowWriteLn(WindowHandle,'');
      end
     else
      begin
       {Something other than enter was pressed, print that character on the screen}
       ConsoleWindowWriteChr(WindowHandle,Character);
      end;
    end;

   {No need to sleep on each loop, ConsoleReadChar will wait until a key is pressed}
  end;

 {No need to halt, we never reach this point}
end.

