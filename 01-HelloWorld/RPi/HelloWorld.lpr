program HelloWorld;

{$mode objfpc}{$H+}

{ Example 01 Hello World                                                       }
{                                                                              }
{  This example provides the classic first example but takes a couple of extra }
{  lines to achieve the result using Ultibo.                                   }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{  Once compiled copy the kernel.img file to an SD card along with the firmware}
{  files and use it to boot your Raspberry Pi.                                 }
{                                                                              }
{  Raspberry Pi A/B/A+/B+/Zero version                                         }
{   What's the difference? See Project, Project Options, Config and Target.    }


{ A couple of introductions first, notice the program keyword above? That tells}
{ Free Pascal that this is a program and not just a unit. When we compile this }
{ program we want Free Pascal to compile all of the required units and then    }
{ link them into a completed executable ready for us to run.                   }

{We need to declare some units that are used for this example.}
uses
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  Framebuffer,
  BCM2835,
  BCM2708;

{We also need to declare a variable to hold a console window handle.}
var
 WindowHandle:TWindowHandle;

{Our code goes between the begin and end of the program file or in other units
 we create that can be called from this unit (File, New Unit from the menu).

 Whatever is the first line after begin below will be the first thing executed
 by Ultibo after the started process has completed.}

begin
 {First we create a console window to write our text on, we make it the full
  size of the screen by specifying CONSOLE_POSITION_FULL and allow it to be the
  default window by passing True as the last parameter.}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 {Next write some text on our newly created console window, we pass the handle
  of the console window created above to tell it where to write the text since
  there can be multiple console windows on screen at once.

  This will appear in the top left corner of the screen in the default color
  and font.}
 ConsoleWindowWriteLn(WindowHandle,'Hello Ultibo!');

 {There are multiple ways to achieve the same thing, because we allowed our console
  window to be the default and we know there are no other windows present, we could
  simply call ConsoleWriteLn() or even WriteLn() instead and the text would appear
  on the default console window. Try them out if you want to experiment.}

 {Because Ultibo is always multi threaded, we should never allow the code to exit
  from the begin..end block of the program. See the Wiki for more information on
  why but unless you are sure of what will happen it is best to simply halt this
  thread here.}
 ThreadHalt(0);
end.

