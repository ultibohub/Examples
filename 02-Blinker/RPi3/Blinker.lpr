program Blinker;

{$mode objfpc}{$H+}

{ Example 02 Blinker                                                           }
{                                                                              }
{  This example shows the standard starting point for bare metal programming   }
{  by blinking the activity LED on and off repeatedly.                         }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{  Once compiled copy the kernel7.img file to an SD card along with the        }
{   firmware files and use it to boot your Raspberry Pi.                       }
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
  BCM2710;

{Declare some variables as well.}
var
 WindowHandle:TWindowHandle;

{Our code goes between the begin and end of the program file or in other units
 we create that can be called from this unit (File, New Unit from the menu).}

begin
 {Before we can turn the LED on or off we need to enable it which sets the GPIO
  pins to the appropriate function.}
 ActivityLEDEnable;

 {So that we can see what's happening, let's create a console window again.}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 {Let's create a loop so that the blinking happens forever, or until you pull the
  power plug at least.

  This loop will execute while the condition is true. Since the value True is always
  true then the loop will continue forever.}
 while True do
  begin
   {Write to the console}
   ConsoleWindowWriteLn(WindowHandle,'Turning on the Activity LED');

   {Turn on the LED}
   ActivityLEDOn;

   {Wait for a while so that things happen at a speed we can see, 500 milliseconds,
    or half a second, should be enough.}
   ThreadSleep(500);

   {Write to the console}
   ConsoleWindowWriteLn(WindowHandle,'Turning off the Activity LED');

   {Turn off the LED}
   ActivityLEDOff;
   
   {Wait a while before turning the LED back on again.}
   ThreadSleep(500);

   {You might want to experiment with the values of ThreadSleep() to make things
    happen faster or maybe you might try rearranging the code in the loop to do
    a double blink each time.}
  end;

 {We will never get to here, so there is no need for ThreadHalt() like in other
  examples.}
end.

