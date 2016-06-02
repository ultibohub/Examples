program GPIOHandling;

{$mode objfpc}{$H+}

{ Example 14 GPIO Handling                                                     }
{                                                                              }
{ In this example we look at using the GPIO functions to detect a simple switch}
{ being opened or closed and turn on or off an LED in response.                }
{                                                                              }
{ To try this example you'll need the following:                               }
{                                                                              }
{ A switch connected between GPIO Pin 18 and Ground                            }
{                                                                              }
{ An LED and a 330 ohm resistor connected between GPIO Pin 16 and Ground       }
{  (Ensure you connect the LED and resistor in series with each other and that }
{   the short leg or the flat side of the LED is connected to Ground)          }
{                                                                              }
{ It's usually best to use a breadboard or breakout board to connect components}
{ to the Pi just to avoid any accidental damage.                               }
{                                                                              }
{ The documentation below shows you where to find each of the pins.            }
{                                                                              }
{ Raspberry Pi Model A and B (26 pin header)                                   }
{   https://www.raspberrypi.org/documentation/usage/gpio/                      }
{                                                                              }
{ Raspberry Pi Models A+/B+/Zero/2B/3B (40 pin header)                         }
{   https://www.raspberrypi.org/documentation/usage/gpio-plus-and-raspi2/      }
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
  GPIO;      {Include the GPIO unit to allow access to the functions}

{We'll need a window handle and a couple of other variables.}
var
 LastValue:LongWord;
 CurrentValue:LongWord;
 WindowHandle:TWindowHandle;

{We also add another function, see below for what is does}
procedure GPIOPinEvent(Data:Pointer;Pin,Trigger:LongWord);
begin
 {Print a message to show what the event was. You might notice that sometimes
  multiple events occur when opening or closing the switch. This can be caused
  by the contacts in the switch bouncing, see below for one way to handle this.}
 ConsoleWindowWriteLn(WindowHandle,'GPIO Pin Event on pin ' + GPIOPinToString(Pin) + ' the event is ' + GPIOTriggerToString(Trigger));
end;

begin
 {Let's create a console window so we can report what is happening}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 {Display a welcome message}
 ConsoleWindowWriteLn(WindowHandle,'Welcome to Example 14 GPIO Handling');

 {We'll use the GPIO functions in the Platform unit for this example, there are
  also GPIO functions in the GPIO unit that allow you to specify some extra parameters
  if you need them.
  
  We need to do some setup of the GPIO pins to tell the GPIO controller what we 
  want to do with them.
    
  All of the GPIO_ constants used below are available in the GlobalConst unit.}
  
 {Set GPIO pin 18 which is our switch to Pull Up so that when the switch is open
  the value read from the pin will be High} 
 GPIOPullSelect(GPIO_PIN_18,GPIO_PULL_UP);
 
 {And make GPIO pin 18 an Input so we can detect the switch being opened or closed}
 GPIOFunctionSelect(GPIO_PIN_18,GPIO_FUNCTION_IN);
 
 {Now set GPIO pin 16 to Pull None}
 GPIOPullSelect(GPIO_PIN_16,GPIO_PULL_NONE);
 
 {And make GPIO pin 16 an Output so we can turn the LED on or off}
 GPIOFunctionSelect(GPIO_PIN_16,GPIO_FUNCTION_OUT);
 
 {Finally set the value of GPIO pin 16 to Low so the LED will be off}
 GPIOOutputSet(GPIO_PIN_16,GPIO_LEVEL_LOW);

 {Let's read the current value of GPIO pin 18 so we know the position of the switch}
 LastValue:=GPIOInputGet(GPIO_PIN_18);
 
 {Start an endless loop checking the switch}
 while True do
  begin
   {Read the value of the switch on GPIO pin 18}
   CurrentValue:=GPIOInputGet(GPIO_PIN_18);
   
   {Compare the latest value with the old value}
   if CurrentValue <> LastValue then
    begin
     {Check if the switch is on.
     
      Notice that we check for Low to see if the switch is on, that's because we
      connected the switch from GPIO pin 18 to Ground so the value will be Low when
      the switch is closed and High when the switch is open.}
     if CurrentValue = GPIO_LEVEL_LOW then
      begin
       {Display a message on the console}
       ConsoleWindowWriteLn(WindowHandle,'The switch is ON');
       
       {Turn on the LED to match the switch}
       GPIOOutputSet(GPIO_PIN_16,GPIO_LEVEL_HIGH);
      end
     else
      begin
       {Display a message on the console}
       ConsoleWindowWriteLn(WindowHandle,'The switch is OFF');
       
       {Turn off the LED when the switch is off}
       GPIOOutputSet(GPIO_PIN_16,GPIO_LEVEL_LOW);
      end;      
     
     {Save the value of GPIO pin 18 for next time}
     LastValue:=CurrentValue;
    end;
   
   {Instead of just looping over and over we can use the features of the GPIO controller
    to tell us when the pin our switch is connected to has changed. That way our thread
    does not consume any CPU time and other things can happen while we wait.}
    
   {There are two ways to wait for a GPIO pin to change, you can call GPIOInputWait which
    will cause the current thread to wait until the change occurs and then return.
    
    Alternatively we can use GPIOInputEvent to register a function to be called when the
    GPIO pin changes, this can be useful if you want your thread to be doing something else
    instead of waiting.
    
    We'll do both here just to show what happens.}
    
   {First we check the value of the switch and decide what to wait for}
   if CurrentValue = GPIO_LEVEL_LOW then
    begin
     {The switch is currently closed (value is Low) so we wait for it to change to High}
      
     {Let's register an event to call when the pin changes to High.
     
      The GPIO_TRIGGER_HIGH parameter tells the GPIO driver what change to wait for.
      
      You can also pass a data parameter to the event, this can be anything you want
      and is not used by the GPIO driver. For example you could use the same function
      for multiple different events and use the data to detect which one occurred.}
     GPIOInputEvent(GPIO_PIN_18,GPIO_TRIGGER_HIGH,INFINITE,@GPIOPinEvent,nil); 
     
     {And make the current thread wait for the change as well.
     
      The INFINITE parameter tells the driver to wait forever.}
     GPIOInputWait(GPIO_PIN_18,GPIO_TRIGGER_HIGH,INFINITE);
     
     {The thread will return here when the GPIO pin changes to High but sometimes switches
      do not change from open to closed cleanly and the contacts may "bounce" and cause a
      false change to occur. To stop this we add a little sleep here to give the switch time
      to settle before checking for the new state, this is called "debouncing".}
     Sleep(10);
    end
   else
    begin
     {The value is High so the switch must be open, wait for it to change to Low}
     
     {The same as above but in reverse, register an event for when the pin changes to Low}
     GPIOInputEvent(GPIO_PIN_18,GPIO_TRIGGER_LOW,INFINITE,@GPIOPinEvent,nil);
     
     {And wait as well for the change to Low}
     GPIOInputWait(GPIO_PIN_18,GPIO_TRIGGER_LOW,INFINITE);
     
     {Add our sleep for debouncing}
     Sleep(10);
    end;
  end;
  
 {Halt the main thread if we ever get to here}
 ThreadHalt(0);
end.

